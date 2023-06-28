{-# OPTIONS_GHC -Wno-unused-matches #-}
-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the termbrigl Public License as published by the Free
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
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Gundeck.Notification.Data
  ( ResultPage (..),
    add,
    addDeduplicated,
    fetch,
    fetchId,
    fetchLast,
    deleteAll,
  )
where

import Cassandra as C
import Control.Error (MaybeT (..))
import Control.Lens ((^.), _1)
import Control.Monad.Catch
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BSL
import Data.Id
import Data.List1 (List1)
import Data.Range (Range, fromRange)
import Data.Sequence (Seq, ViewL (..), ViewR (..), (<|), (><))
import qualified Data.Sequence as Seq
import Debug.Trace (traceM)
import Gundeck.Options (NotificationTTL (..))
import Imports hiding (cs)
import UnliftIO (pooledForConcurrentlyN_)
import UnliftIO.Async (pooledMapConcurrentlyN)
import Wire.API.Internal.Notification

data ResultPage = ResultPage
  { -- | A sequence of notifications.
    resultSeq :: Seq QueuedNotification,
    -- | Whether there might be more notifications that can be
    -- obtained through another query, starting the the ID of the
    -- last notification in 'resultSeq'.
    resultHasMore :: !Bool,
    -- | Whether there might be a gap in the 'resultSeq'. This is 'True'
    -- iff a start ID ('since') has been given which could not be found.
    resultGap :: !Bool
  }

data Payload = Payload

type PayloadId = Id 'Payload

addDeduplicated ::
  (MonadClient m, MonadUnliftIO m, MonadCatch m) =>
  NotificationId ->
  List1 NotificationTarget ->
  BSL.ByteString ->
  NotificationTTL ->
  m ()
addDeduplicated n tgts b (notificationTTLSeconds -> t) = do
  traceM ("addDeduplicated: " <> show (tgts, b))
  payloadId <- randomId
  traceM "inserting payload"
  write cqlInsertPayload (params LocalQuorum (payloadId, Blob b, fromIntegral t)) & retry x5
  let payloadRefSize = fromIntegral $ BSL.length b

  pooledForConcurrentlyN_ 32 tgts $ \tgt -> do
    let u = tgt ^. targetUser
        cs = C.Set (tgt ^. targetClients)
    traceM "inserting notfications"
    catch (write cqlInsert (params LocalQuorum (u, n, payloadId, payloadRefSize, cs, fromIntegral t)) & retry x5) (\(e :: SomeException) -> traceM (displayException e))
  where
    cqlInsert :: PrepQuery W (UserId, NotificationId, PayloadId, Int32, C.Set ClientId, Int32) ()
    cqlInsert =
      "INSERT INTO notifications \
      \(user, id, payload_ref, payload_ref_size, clients) VALUES \
      \(?, ?, ?, ?, ?) \
      \USING TTL ?"

    cqlInsertPayload :: PrepQuery W (PayloadId, Blob, Int32) ()
    cqlInsertPayload =
      "INSERT INTO notification_payload \
      \(id, payload) VALUES \
      \(?   , ?) \
      \USING TTL ?"

add ::
  (MonadClient m, MonadUnliftIO m) =>
  NotificationId ->
  List1 NotificationTarget ->
  List1 JSON.Object ->
  NotificationTTL ->
  m ()
add n tgts (Blob . JSON.encode -> p) (notificationTTLSeconds -> t) =
  pooledForConcurrentlyN_ 32 tgts $ \tgt ->
    let u = tgt ^. targetUser
        cs = C.Set (tgt ^. targetClients)
     in write cqlInsert (params LocalQuorum (u, n, p, cs, fromIntegral t)) & retry x5
  where
    cqlInsert :: PrepQuery W (UserId, NotificationId, Blob, C.Set ClientId, Int32) ()
    cqlInsert =
      "INSERT INTO notifications \
      \(user, id, payload, clients) VALUES \
      \(?   , ? , ?      , ?) \
      \USING TTL ?"

fetchId :: MonadClient m => UserId -> NotificationId -> Maybe ClientId -> m (Maybe QueuedNotification)
fetchId u n c =
  listToMaybe . foldr' (toNotif c) []
    <$> query cqlById (params LocalQuorum (u, n))
    & retry x1
  where
    cqlById :: PrepQuery R (UserId, NotificationId) (TimeUuid, Blob, Maybe (C.Set ClientId))
    cqlById =
      "SELECT id, payload, clients \
      \FROM notifications \
      \WHERE user = ? AND id = ?"

fetchLast :: MonadClient m => UserId -> Maybe ClientId -> m (Maybe QueuedNotification)
fetchLast u c = do
  ls <- query cqlLast (params LocalQuorum (Identity u)) & retry x1
  case ls of
    [] -> pure Nothing
    ns@(n : _) ->
      ns `getFirstOrElse` do
        p <- paginate cqlSeek (paramsP LocalQuorum (u, n ^. _1) 100) & retry x1
        seek p
  where
    seek p =
      result p
        `getFirstOrElse` if hasMore p
          then liftClient (nextPage p) >>= seek
          else pure Nothing
    getFirstOrElse ns f =
      case listToMaybe (foldr' (toNotif c) [] ns) of
        Just n -> pure (Just n)
        Nothing -> f
    cqlLast :: PrepQuery R (Identity UserId) (TimeUuid, Blob, Maybe (C.Set ClientId))
    cqlLast =
      "SELECT id, payload, clients \
      \FROM notifications \
      \WHERE user = ? \
      \ORDER BY id DESC LIMIT 1"
    cqlSeek :: PrepQuery R (UserId, TimeUuid) (TimeUuid, Blob, Maybe (C.Set ClientId))
    cqlSeek =
      "SELECT id, payload, clients \
      \FROM notifications \
      \WHERE user = ? AND id < ? \
      \ORDER BY id DESC"

fetchPayload :: MonadClient m => Maybe ClientId -> PRow -> m (Maybe QueuedNotification)
fetchPayload c (id_, mbPayload, mbPayloadRef, mbPayloadRefSize, mbClients) =
  case (mbPayload, mbPayloadRef) of
    (Just payload, _) -> pure $ toNotifSingle c (id_, payload, mbClients)
    (_, Just payloadRef) -> runMaybeT $ do
      pl <- MaybeT $ error "TODO"
      maybe mzero pure $ toNotifSingle c (id_, pl, mbClients)
    _ -> pure Nothing

type PRow = (TimeUuid, Maybe Blob, Maybe PayloadId, Maybe Int32, Maybe (C.Set ClientId))

payloadSize :: PRow -> Int32
payloadSize (_, mbPayload, _, mbPayloadRefSize, _) =
  case (mbPayload, mbPayloadRefSize) of
    (Just blob, _) -> fromIntegral $ BSL.length (fromBlob blob)
    (_, Just size) -> size
    _ -> 0

-- | Fetches referenced payloads until maxTotalSize payload bytes are fetched from the database.
-- At least the first row is fetched regardless of the payload size.
fetchPayloads :: (MonadClient m, MonadUnliftIO m) => Maybe ClientId -> Int32 -> [PRow] -> m (Seq QueuedNotification)
fetchPayloads c maxTotalSize rows = do
  let rows' = truncateNotifs [] (0 :: Int) maxTotalSize rows
  Seq.fromList . catMaybes <$> pooledMapConcurrentlyN 16 (fetchPayload c) rows'
  where
    truncateNotifs acc i left [] = reverse acc
    truncateNotifs acc i left (row : rest)
      | i > 0 && left <= 0 = reverse acc
      | otherwise = truncateNotifs (row : acc) (i + 1) (left - payloadSize row) rest

-- | Tries to fetch @remaining@ many notifications.
-- The returned 'Seq' might contain more notifications than @remaining@, (see
-- https://docs.datastax.com/en/developer/java-driver/3.2/manual/paging/).
--
-- The boolean indicates whether more notifications can be fetched.
collect :: (MonadClient m, MonadUnliftIO m) => Maybe ClientId -> Seq QueuedNotification -> Bool -> Int -> m (Page PRow) -> m (Seq QueuedNotification, Bool)
collect c acc lastPageHasMore remaining getPage
  | remaining <= 0 || not lastPageHasMore = pure (acc, lastPageHasMore)
  | otherwise = do
      page <- getPage
      s <- fetchPayloads c 100000000 (result page)
      let remaining' = remaining - Seq.length s
      collect c (acc >< s) (hasMore page) remaining' (liftClient (nextPage page))

fetch :: (MonadClient m, MonadUnliftIO m) => UserId -> Maybe ClientId -> Maybe NotificationId -> Range 100 10000 Int32 -> m ResultPage
fetch u c since (fromRange -> size) = do
  traceM "this is fetch"
  -- We always need to look for one more than requested in order to correctly
  -- report whether there are more results.
  let size' = bool (+ 1) (+ 2) (isJust since) size
  let page1 = case TimeUuid . toUUID <$> since of
        -- TODO: don't use size' here
        Nothing -> paginate cqlStart (paramsP LocalQuorum (Identity u) size') & retry x1
        Just s -> paginate cqlSince (paramsP LocalQuorum (u, s) size') & retry x1
  -- Collect results, requesting more pages until we run out of data
  -- or have found size + 1 notifications (not including the 'since').
  -- let isize = fromIntegral size' :: Int
  (ns, more) <- collect c Seq.empty True (fromIntegral size') page1
  -- Drop the extra element from the end as well as the inclusive start
  -- value (if a 'since' was given and found).
  pure $! case Seq.viewl (trim (fromIntegral size' - 1) ns) of
    EmptyL -> ResultPage Seq.empty False (isJust since)
    x :< xs -> case since of
      Just s
        | s == x ^. queuedNotificationId ->
            ResultPage xs more False
      _ -> ResultPage (x <| xs) more (isJust since)
  where
    trim l ns
      | Seq.length ns <= l = ns
      | otherwise = case Seq.viewr ns of
          EmptyR -> ns
          xs :> _ -> xs
    cqlStart :: PrepQuery R (Identity UserId) (TimeUuid, Maybe Blob, Maybe PayloadId, Maybe Int32, Maybe (C.Set ClientId))
    cqlStart =
      "SELECT id, payload, payload_ref, payload_ref_size, clients \
      \FROM notifications \
      \WHERE user = ? \
      \ORDER BY id ASC"
    cqlSince :: PrepQuery R (UserId, TimeUuid) (TimeUuid, Maybe Blob, Maybe PayloadId, Maybe Int32, Maybe (C.Set ClientId))
    cqlSince =
      "SELECT id, payload, payload_ref, payload_ref_size, clients \
      \FROM notifications \
      \WHERE user = ? AND id >= ? \
      \ORDER BY id ASC"

deleteAll :: MonadClient m => UserId -> m ()
deleteAll u = write cql (params LocalQuorum (Identity u)) & retry x5
  where
    cql :: PrepQuery W (Identity UserId) ()
    cql = "DELETE FROM notifications WHERE user = ?"

-------------------------------------------------------------------------------
-- Conversions

toNotif ::
  Maybe ClientId ->
  (TimeUuid, Blob, Maybe (C.Set ClientId)) ->
  [QueuedNotification] ->
  [QueuedNotification]
toNotif c (i, b, cs) ns =
  let clients = maybe [] fromSet cs
      notifId = Id (fromTimeUuid i)
   in case JSON.decode' (fromBlob b) of
        Nothing -> ns
        Just pl ->
          -- nb. At some point we should be able to do:
          -- @@@ if null clients || maybe False (`elem` clients) c @@@
          -- i.e. not return notifications targeted at specific clients,
          -- if no client ID is given. We currently return all of them
          -- in this case for backward compatibility with existing internal
          -- clients.
          if null clients || maybe True (`elem` clients) c
            then queuedNotification notifId pl : ns
            else ns

toNotifSingle ::
  Maybe ClientId ->
  (TimeUuid, Blob, Maybe (C.Set ClientId)) ->
  Maybe QueuedNotification
toNotifSingle c (i, b, cs) =
  let clients = maybe [] fromSet cs
      notifId = Id (fromTimeUuid i)
   in case JSON.decode' (fromBlob b) of
        Nothing -> Nothing
        Just pl ->
          -- nb. At some point we should be able to do:
          -- @@@ if null clients || maybe False (`elem` clients) c @@@
          -- i.e. not return notifications targeted at specific clients,
          -- if no client ID is given. We currently return all of them
          -- in this case for backward compatibility with existing internal
          -- clients.
          if null clients || maybe True (`elem` clients) c
            then Just (queuedNotification notifId pl)
            else Nothing
