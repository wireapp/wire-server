-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Gundeck.Push.Websocket
  ( push,
    bulkPush,
    MonadBulkPush (..),
  )
where

import Bilge hiding (trace)
import Bilge.RPC
import Bilge.Retry (rpcHandlers)
import Control.Arrow ((&&&))
import Control.Exception (ErrorCall (ErrorCall))
import Control.Lens (view, (%~), (^.), _2)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow, catch, throwM, try)
import Control.Retry
import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy as L
import Data.Id
import Data.List1
import qualified Data.Map as Map
import qualified Data.Metrics as Metrics
import Data.Misc (Milliseconds (..))
import qualified Data.Set as Set
import Data.Time.Clock.POSIX
import Gundeck.Monad
import qualified Gundeck.Presence.Data as Presence
import Gundeck.Types.BulkPush
import Gundeck.Types.Notification
import Gundeck.Types.Presence
import Gundeck.Util
import Imports
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..))
import qualified Network.HTTP.Client.Internal as Http
import Network.HTTP.Types (StdMethod (POST), status200, status410)
import qualified Network.URI as URI
import System.Logger.Class (val, (+++), (~~))
import qualified System.Logger.Class as Log
import UnliftIO (handleAny, mapConcurrently)

class (Monad m, MonadThrow m, Log.MonadLogger m) => MonadBulkPush m where
  mbpBulkSend :: URI -> BulkPushRequest -> m (URI, Either SomeException BulkPushResponse)
  mbpDeleteAllPresences :: [Presence] -> m ()
  mbpPosixTime :: m Milliseconds
  mbpMapConcurrently :: Traversable t => (a -> m b) -> t a -> m (t b)
  mbpMonitorBadCannons :: (URI, (SomeException, [Presence])) -> m ()

instance MonadBulkPush Gundeck where
  mbpBulkSend = bulkSend
  mbpDeleteAllPresences = Presence.deleteAll
  mbpPosixTime = posixTime
  mbpMapConcurrently = mapConcurrently
  mbpMonitorBadCannons = monitorBadCannons

-- | Send a 'Notification's to associated 'Presence's.  Send at most one request to each Cannon.
-- Return the lists of 'Presence's successfully reached for each resp. 'Notification'.
bulkPush :: forall m. MonadBulkPush m => [(Notification, [Presence])] -> m [(NotificationId, [Presence])]
-- REFACTOR: make presences lists (and notification list) non-empty where applicable?  are there
-- better types to express more of our semantics / invariants?  (what about duplicates in presence
-- lists?)
bulkPush notifs = do
  let reqs = fanOut notifs
  flbck <- flowBack <$> (uncurry mbpBulkSend `mbpMapConcurrently` reqs)
  let -- lookup by 'URI' can fail iff we screwed up URI handling in this module.
      presencesByCannon = mkPresencesByCannon . mconcat $ snd <$> notifs
      -- lookup by 'PushTarget' can fail iff Cannon sends an invalid key.
      presenceByPushTarget = mkPresenceByPushTarget . mconcat $ snd <$> notifs
  badCannons :: [(URI, (SomeException, [Presence]))] <-
    forM (flowBackBadCannons flbck) $ \(uri, e) -> (uri,) . (e,) <$> presencesByCannon uri
  prcsGone :: [Presence] <-
    presenceByPushTarget `mapM` flowBackLostPrcs flbck
  successes :: [(NotificationId, Presence)] <-
    (\(nid, trgt) -> (nid,) <$> presenceByPushTarget trgt) `mapM` flowBackDelivered flbck
  (\info -> mbpMonitorBadCannons info >> logBadCannons info) `mapM_` badCannons
  logPrcsGone `mapM_` prcsGone
  logSuccesses `mapM_` successes
  mbpDeleteAllPresences =<< do
    now <- mbpPosixTime
    let deletions = prcsGone <> (filter dead . mconcat $ snd . snd <$> badCannons)
        dead prc = now - createdAt prc > 10 * posixDay
        posixDay = Ms (round (1000 * posixDayLength))
    pure deletions
  pure (groupAssoc successes)

-- | log all cannons with response status @/= 200@.
monitorBadCannons ::
  (MonadIO m, MonadReader Env m) =>
  (uri, (error, [Presence])) ->
  m ()
monitorBadCannons (_uri, (_err, prcs)) = do
  view monitor
    >>= Metrics.counterAdd
      (fromIntegral $ length prcs)
      (Metrics.path "push.ws.unreachable")

logBadCannons :: Log.MonadLogger m => (URI, (SomeException, [Presence])) -> m ()
logBadCannons (uri, (err, prcs)) = do
  forM_ prcs $ \prc ->
    Log.warn $
      logPresence prc
        ~~ Log.field "created_at" (ms $ createdAt prc)
        ~~ Log.field "cannon_uri" (show uri)
        ~~ Log.field "resource_target" (show $ resource prc)
        ~~ Log.field "http_exception" (intercalate " | " . lines . show $ err)
        ~~ Log.msg (val "WebSocket presence unreachable: ")

logPrcsGone :: Log.MonadLogger m => Presence -> m ()
logPrcsGone prc = Log.debug $ logPresence prc ~~ Log.msg (val "WebSocket presence gone")

logSuccesses :: Log.MonadLogger m => (a, Presence) -> m ()
logSuccesses (_, prc) = Log.debug $ logPresence prc ~~ Log.msg (val "WebSocket push success")

fanOut :: [(Notification, [Presence])] -> [(URI, BulkPushRequest)]
fanOut =
  fmap (_2 %~ (mkBulkPushRequest . groupByNotification))
    . groupByURI
    . mconcat
    . fmap pullUri
  where
    mkBulkPushRequest :: [(Notification, [Presence])] -> BulkPushRequest
    mkBulkPushRequest = BulkPushRequest . fmap (_2 %~ fmap mkPushTarget)
    groupByNotification :: [(Notification, Presence)] -> [(Notification, [Presence])]
    groupByNotification = groupAssoc' (compare `on` ntfId)
    groupByURI :: [(Notification, (URI, Presence))] -> [(URI, [(Notification, Presence)])]
    groupByURI = groupAssoc . fmap (\(notif, (uri, prc)) -> (uri, (notif, prc)))
    pullUri :: (notif, [Presence]) -> [(notif, (URI, Presence))]
    pullUri (notif, prcs) = (notif,) . (bulkresource &&& id) <$> prcs

bulkSend ::
  forall m.
  ( MonadIO m,
    MonadThrow m,
    MonadCatch m,
    MonadMask m,
    HasRequestId m,
    MonadHttp m,
    MonadUnliftIO m
  ) =>
  URI ->
  BulkPushRequest ->
  m (URI, Either SomeException BulkPushResponse)
bulkSend uri req = (uri,) <$> ((Right <$> bulkSend' uri req) `catch` (pure . Left))

bulkSend' ::
  forall m.
  ( MonadIO m,
    MonadThrow m,
    MonadCatch m,
    MonadMask m,
    HasRequestId m,
    MonadHttp m,
    MonadUnliftIO m
  ) =>
  URI ->
  BulkPushRequest ->
  m BulkPushResponse
bulkSend' uri (encode -> jsbody) = do
  req <-
    ( check
        . method POST
        . contentJson
        . lbytes jsbody
        . timeout 3000 -- ms
      )
      <$> Http.setUri empty (fromURI uri)
  try (submit req) >>= \case
    Left e -> throwM (e :: SomeException)
    Right r -> decodeBulkResp $ responseBody r
  where
    submit req = recovering (limitRetries 1) rpcHandlers $ const (rpc' "cannon" req id)
    check req =
      req
        { Http.checkResponse = \rq rs ->
            when (responseStatus rs /= status200) $
              let ex = StatusCodeException (rs {responseBody = ()}) mempty
               in throwM $ HttpExceptionRequest rq ex
        }
    decodeBulkResp :: MonadThrow m => Maybe L.ByteString -> m BulkPushResponse
    decodeBulkResp Nothing = throwM $ ErrorCall "missing response body from cannon"
    decodeBulkResp (Just lbs) = either err pure $ eitherDecode lbs
      where
        err = throwM . ErrorCall . ("bad response body from cannon: " <>)

-- | NOTE: 'PushTarget's may occur several times both in the "lost" and in the "delivered" list.
-- This happens iff there are several 'Notifcation's for the same 'PushTarget', and some of them are
-- delivered while others aren't.
data FlowBack = FlowBack
  { -- | list of cannons that failed to respond with status 200
    flowBackBadCannons :: [(URI, SomeException)],
    -- | 401 inside the body (for one presence)
    flowBackLostPrcs :: [PushTarget],
    flowBackDelivered :: [(NotificationId, PushTarget)]
  }

flowBack :: [(URI, Either SomeException BulkPushResponse)] -> FlowBack
flowBack rawresps = FlowBack broken gone delivered
  where
    broken :: [(URI, SomeException)]
    broken =
      lefts' rawresps
    gone :: [PushTarget]
    gone =
      map (snd . snd)
        . filter
          ( \(st, _) -> case st of
              PushStatusOk -> False
              PushStatusGone -> True
          )
        $ responsive
    delivered :: [(NotificationId, PushTarget)]
    delivered =
      map snd
        . filter
          ( \(st, _) -> case st of
              PushStatusOk -> True
              PushStatusGone -> False
          )
        $ responsive
    responsive :: [(PushStatus, (NotificationId, PushTarget))]
    responsive =
      map (\(n, t, s) -> (s, (n, t)))
        . mconcat
        . fmap fromBulkPushResponse
        . rights
        $ snd <$> rawresps
    lefts' :: [(c, Either a b)] -> [(c, a)]
    lefts' [] = []
    lefts' ((c, Left x) : xs) = (c, x) : lefts' xs
    lefts' ((_, Right _) : xs) = lefts' xs

{-# INLINE mkPresencesByCannon #-}
mkPresencesByCannon :: MonadThrow m => [Presence] -> URI -> m [Presence]
mkPresencesByCannon prcs uri = maybe (throwM err) pure $ Map.lookup uri mp
  where
    err = ErrorCall "internal error in Gundeck: invalid URL in bulkpush result"
    mp :: Map URI [Presence]
    mp = foldl' collect mempty $ (bulkresource &&& id) <$> prcs
    collect :: Map URI [Presence] -> (URI, Presence) -> Map URI [Presence]
    collect mp' (uri', prc) = Map.alter (go prc) uri' mp'
    go :: Presence -> Maybe [Presence] -> Maybe [Presence]
    go prc Nothing = Just [prc]
    go prc (Just prcs') = Just $ prc : prcs'

{-# INLINE mkPresenceByPushTarget #-}
mkPresenceByPushTarget :: MonadThrow m => [Presence] -> PushTarget -> m Presence
mkPresenceByPushTarget prcs ptarget = maybe (throwM err) pure $ Map.lookup ptarget mp
  where
    err = ErrorCall "internal error in Cannon: invalid PushTarget in bulkpush response"
    mp :: Map PushTarget Presence
    mp = Map.fromList $ (mkPushTarget &&& id) <$> prcs

{-# INLINE bulkresource #-}
bulkresource :: Presence -> URI
bulkresource = URI . (\x -> x {URI.uriPath = "/i/bulkpush"}) . fromURI . resource

-- TODO: a Map-based implementation would be faster for sufficiently large inputs.  do we want to
-- take the time and benchmark the difference?  move it to types-common?
{-# INLINE groupAssoc #-}
groupAssoc :: (Eq a, Ord a) => [(a, b)] -> [(a, [b])]
groupAssoc = groupAssoc' compare

-- TODO: Also should we give 'Notification' an 'Ord' instance?
{-# INLINE groupAssoc' #-}
groupAssoc' :: (Eq a) => (a -> a -> Ordering) -> [(a, b)] -> [(a, [b])]
groupAssoc' cmp =
  fmap
    ( \case
        xs@(x : _) -> (fst x, snd <$> xs)
        [] -> error "impossible: list elements returned by groupBy are never empty."
    )
    . groupBy ((==) `on` fst)
    . sortBy (cmp `on` fst)

{-# INLINE mkPushTarget #-}
mkPushTarget :: Presence -> PushTarget
mkPushTarget pre = PushTarget (userId pre) (connId pre)

-----------------------------------------------------------------------------
-- old, multi-request push.

push ::
  Notification ->
  List1 NotificationTarget ->
  UserId -> -- Origin user.
  Maybe ConnId -> -- Origin device connection.
  Set ConnId -> -- Only target these connections.
  Gundeck [Presence]
push notif (toList -> tgts) originUser originConn conns = do
  pp <- handleAny noPresences listPresences
  (ok, gone) <- foldM onResult ([], []) =<< send notif pp
  Presence.deleteAll gone
  return ok
  where
    listPresences =
      excludeOrigin
        . filterByConnection
        . concat
        . filterByClient
        . zip tgts
        <$> Presence.listAll (view targetUser <$> tgts)
    noPresences exn = do
      Log.err $
        Log.field "error" (show exn)
          ~~ Log.msg (val "Failed to get presences.")
      return []
    filterByClient = map $ \(tgt, ps) ->
      let cs = tgt ^. targetClients
       in if null cs
            then ps
            else filter (maybe True (`elem` cs) . clientId) ps
    filterByConnection =
      if Set.null conns
        then id
        else filter ((`Set.member` conns) . connId)
    excludeOrigin =
      let neqUser p = originUser /= userId p
          neqConn p = originConn /= Just (connId p)
       in filter (\p -> neqUser p || neqConn p)
    onResult (ok, gone) (PushSuccess p) = do
      Log.debug $ logPresence p ~~ Log.msg (val "WebSocket push success")
      return (p : ok, gone)
    onResult (ok, gone) (PushGone p) = do
      Log.debug $ logPresence p ~~ Log.msg (val "WebSocket presence gone")
      return (ok, p : gone)
    onResult (ok, gone) (PushFailure p _) = do
      view monitor >>= Metrics.counterIncr (Metrics.path "push.ws.unreachable")
      Log.info $
        logPresence p
          ~~ Log.field "created_at" (ms $ createdAt p)
          ~~ Log.msg (val "WebSocket presence unreachable: " +++ toByteString (resource p))
      now <- posixTime
      if now - createdAt p > 10 * posixDay
        then return (ok, p : gone)
        else return (ok, gone)
    posixDay = Ms (round (1000 * posixDayLength))

-----------------------------------------------------------------------------
-- Internal

-- | Not to be confused with 'PushStatus': 'PushResult' is in internal to Gundeck, carries a
-- 'Presence', and can express HTTP errors.
data PushResult
  = PushSuccess Presence
  | PushGone Presence
  | PushFailure Presence SomeException

send :: Notification -> [Presence] -> Gundeck [PushResult]
send n pp =
  let js = encode n
   in zipWith eval pp <$> mapAsync (fn js) pp
  where
    fn js p = do
      req <- Http.setUri empty (fromURI (resource p))
      recovering x1 rpcHandlers $
        const $
          rpc' "cannon" (check req) $
            method POST
              . contentJson
              . lbytes js
              . timeout 3000 -- ms
    check r =
      r
        { Http.checkResponse = \rq rs ->
            when (responseStatus rs `notElem` [status200, status410]) $
              let ex = StatusCodeException (rs {responseBody = ()}) mempty
               in throwM $ HttpExceptionRequest rq ex
        }
    eval p (Left e) = PushFailure p e
    eval p (Right r) = if statusCode r == 200 then PushSuccess p else PushGone p
    x1 = limitRetries 1

logPresence :: Presence -> Log.Msg -> Log.Msg
logPresence p =
  Log.field "user" (toByteString (userId p))
    ~~ Log.field "zconn" (toByteString (connId p))
