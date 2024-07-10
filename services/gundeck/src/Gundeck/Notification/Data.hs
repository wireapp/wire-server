-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Gundeck.Notification.Data
  ( ResultPage (..),
    add,
    fetch,
    fetchId,
    fetchLast,
    deleteAll,
  )
where

import Cassandra as C
import Control.Error (MaybeT (..))
import Control.Lens (view, (^.), _1)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Aeson qualified as JSON
import Data.ByteString.Lazy qualified as BSL
import Data.Id
import Data.List1 (List1)
import Data.Map qualified as Map
import Data.Range (Range, fromRange)
import Data.Sequence (Seq, ViewL ((:<)))
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Gundeck.Env
import Gundeck.Options (NotificationTTL (..), internalPageSize, maxPayloadLoadSize, notificationTTL, settings)
import Gundeck.Push.Native.Serialise ()
import Imports
import Network.AMQP qualified as Q
import Network.AMQP.Types qualified as Q
import UnliftIO (pooledForConcurrentlyN_, pooledMapConcurrentlyN)
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

add ::
  forall m.
  (MonadReader Env m, MonadUnliftIO m) =>
  List1 NotificationTarget ->
  List1 JSON.Object ->
  m ()
add tgts event = do
  --  TODO: maybe tryRead and fail?
  chan <- readMVar =<< view rabbitmqChannel
  pooledForConcurrentlyN_ 32 tgts $ \tgt -> do
    let uid = tgt ^. targetUser
    ensureNotifStream uid
    let msg =
          Q.newMsg
            { Q.msgBody =
                Aeson.encode $
                  Aeson.object
                    [ "target_clients" .= (tgt ^. targetClients),
                      "event" .= event
                    ]
            }
    liftIO $ Q.publishMsg chan "" (userStreamName uid) msg

ensureNotifStream :: (MonadReader Env m, MonadIO m) => UserId -> m ()
ensureNotifStream uid = do
  chan <- readMVar =<< view rabbitmqChannel
  NotificationTTL ttlSeconds <- view $ options . settings . notificationTTL
  let qOpts =
        Q.newQueue
          { Q.queueName = userStreamName uid,
            Q.queueHeaders =
              Q.FieldTable $
                Map.fromList
                  [ ("x-queue-type", (Q.FVString "stream")),
                    ("x-max-age", (Q.FVString $ Text.encodeUtf8 $ Text.pack $ show ttlSeconds <> "s"))
                  ]
          }
  void $ liftIO $ Q.declareQueue chan qOpts

userStreamName :: UserId -> Text
userStreamName uid = "client-notifications." <> Text.pack (show uid)

fetchId :: (MonadClient m) => UserId -> NotificationId -> Maybe ClientId -> m (Maybe QueuedNotification)
fetchId u n c = runMaybeT $ do
  row <- MaybeT $ retry x1 $ query1 cqlById (params LocalQuorum (u, n))
  MaybeT $ fetchPayload c row
  where
    cqlById :: PrepQuery R (UserId, NotificationId) NotifRow
    cqlById =
      "SELECT id, payload, payload_ref, payload_ref_size, clients \
      \FROM notifications \
      \WHERE user = ? AND id = ?"

fetchLast :: (MonadReader Env m, MonadClient m) => UserId -> Maybe ClientId -> m (Maybe QueuedNotification)
fetchLast u c = do
  pageSize <- fromMaybe 100 <$> asks (^. options . settings . internalPageSize)
  go (Page True [] (firstPage pageSize))
  where
    go page = case result page of
      (row : rows) -> do
        mNotif <- fetchPayload c row
        case mNotif of
          Nothing -> go page {result = rows}
          Just notif -> pure (Just notif)
      [] | hasMore page -> do
        page' <- liftClient (nextPage page)
        go page'
      _ -> pure Nothing

    -- The first page consists of at most one row. We retrieve the first page
    -- with a direct query with a LIMIT, and the following pages using
    -- Cassandra pagination.
    firstPage pageSize = do
      results <- retry x1 $ query cqlLast (params LocalQuorum (Identity u))
      let nextPage = case results of
            [] -> pure emptyPage
            (n : _) ->
              retry x1 $
                paginate cqlSeek (paramsP LocalQuorum (u, n ^. _1) pageSize)
      pure $ Page True results nextPage

    cqlLast :: PrepQuery R (Identity UserId) NotifRow
    cqlLast =
      "SELECT id, payload, payload_ref, payload_ref_size, clients \
      \FROM notifications \
      \WHERE user = ? \
      \ORDER BY id DESC LIMIT 1"
    cqlSeek :: PrepQuery R (UserId, TimeUuid) NotifRow
    cqlSeek =
      "SELECT id, payload, payload_ref, payload_ref_size, clients \
      \FROM notifications \
      \WHERE user = ? AND id < ? \
      \ORDER BY id DESC"

fetchPayload :: (MonadClient m) => Maybe ClientId -> NotifRow -> m (Maybe QueuedNotification)
fetchPayload c (id_, mbPayload, mbPayloadRef, _mbPayloadRefSize, mbClients) =
  case (mbPayload, mbPayloadRef) of
    (Just payload, _) -> pure $ toNotifSingle c (id_, payload, mbClients)
    (_, Just payloadRef) -> runMaybeT $ do
      pl <- MaybeT $ fmap (fmap runIdentity) (query1 cqlSelectPayload (params LocalQuorum (Identity payloadRef)))
      maybe mzero pure $ toNotifSingle c (id_, pl, mbClients)
    _ -> pure Nothing
  where
    cqlSelectPayload :: PrepQuery R (Identity PayloadId) (Identity Blob)
    cqlSelectPayload = "SELECT payload from notification_payload where id = ?"

type NotifRow = (TimeUuid, Maybe Blob, Maybe PayloadId, Maybe Int32, Maybe (C.Set ClientId))

payloadSize :: NotifRow -> Int32
payloadSize (_, mbPayload, _, mbPayloadRefSize, _) =
  case (mbPayload, mbPayloadRefSize) of
    (Just blob, _) -> fromIntegral $ BSL.length (fromBlob blob)
    (_, Just size) -> size
    _ -> 0

-- | Fetches referenced payloads until maxTotalSize payload bytes are fetched from the database.
-- At least the first row is fetched regardless of the payload size.
fetchPayloads :: (MonadClient m, MonadUnliftIO m) => Maybe ClientId -> Int32 -> [NotifRow] -> m (Seq QueuedNotification, Int32)
fetchPayloads c left rows = do
  let (rows', left') = truncateNotifs [] (0 :: Int) left rows
  s <- Seq.fromList . catMaybes <$> pooledMapConcurrentlyN 16 (fetchPayload c) rows'
  pure (s, left')
  where
    truncateNotifs acc _i l [] = (reverse acc, l)
    truncateNotifs acc i l (row : rest)
      | i > 0 && l <= 0 = (reverse acc, l)
      | otherwise = truncateNotifs (row : acc) (i + 1) (l - payloadSize row) rest

-- | Tries to fetch @remaining@ many notifications.
-- The returned 'Seq' might contain more notifications than @remaining@, (see
-- https://docs.datastax.com/en/developer/java-driver/3.2/manual/paging/).
--
-- The boolean indicates whether more notifications can be fetched.
collect :: (MonadReader Env m, MonadClient m, MonadUnliftIO m) => Maybe ClientId -> Seq QueuedNotification -> Bool -> Int -> Int32 -> m (Page NotifRow) -> m (Seq QueuedNotification, Bool)
collect c acc lastPageHasMore remaining remainingBytes getPage
  | remaining <= 0 = pure (acc, lastPageHasMore)
  | remainingBytes <= 0 = pure (acc, True)
  | not lastPageHasMore = pure (acc, False)
  | otherwise = do
      page <- getPage
      let rows = result page
      (s, remaingBytes') <- fetchPayloads c remainingBytes rows
      let remaining' = remaining - Seq.length s
      collect c (acc <> s) (hasMore page) remaining' remaingBytes' (liftClient (nextPage page))

mkResultPage :: Int -> Bool -> Seq QueuedNotification -> ResultPage
mkResultPage size more ns =
  ResultPage
    { resultSeq = Seq.take size ns,
      resultHasMore = Seq.length ns > size || more,
      resultGap = False
    }

fetch :: (MonadReader Env m, MonadClient m, MonadUnliftIO m) => UserId -> Maybe ClientId -> Maybe NotificationId -> Range 100 10000 Int32 -> m ResultPage
fetch u c Nothing (fromIntegral . fromRange -> size) = do
  pageSize <- fromMaybe 100 <$> asks (^. options . settings . internalPageSize)
  let page1 = retry x1 $ paginate cqlStart (paramsP LocalQuorum (Identity u) pageSize)
  -- We always need to look for one more than requested in order to correctly
  -- report whether there are more results.
  maxPayloadSize <- fromMaybe (5 * 1024 * 1024) <$> asks (^. options . settings . maxPayloadLoadSize)
  (ns, more) <- collect c Seq.empty True (size + 1) maxPayloadSize page1
  -- Drop the extra element at the end if present
  pure $! mkResultPage size more ns
  where
    cqlStart :: PrepQuery R (Identity UserId) NotifRow
    cqlStart =
      "SELECT id, payload, payload_ref, payload_ref_size, clients \
      \FROM notifications \
      \WHERE user = ? \
      \ORDER BY id ASC"
fetch u c (Just since) (fromIntegral . fromRange -> size) = do
  pageSize <- fromMaybe 100 <$> asks (^. options . settings . internalPageSize)
  let page1 =
        retry x1 $
          paginate cqlSince (paramsP LocalQuorum (u, TimeUuid (toUUID $ undefined since)) pageSize)
  -- We fetch 2 more rows than requested. The first is to accommodate the
  -- notification corresponding to the `since` argument itself. The second is
  -- to get an accurate `hasMore`, just like in the case above.

  maxPayloadSize <- fromMaybe (5 * 1024 * 1024) <$> asks (^. options . settings . maxPayloadLoadSize)
  (ns, more) <- collect c Seq.empty True (size + 2) maxPayloadSize page1
  -- Remove notification corresponding to the `since` argument, and record if it is found.
  let (ns', sinceFound) = case Seq.viewl ns of
        x :< xs | since == x ^. queuedNotificationId -> (xs, True)
        _ -> (ns, False)
  pure $!
    (mkResultPage size more ns')
      { resultGap = not sinceFound
      }
  where
    cqlSince :: PrepQuery R (UserId, TimeUuid) NotifRow
    cqlSince =
      "SELECT id, payload, payload_ref, payload_ref_size, clients \
      \FROM notifications \
      \WHERE user = ? AND id >= ? \
      \ORDER BY id ASC"

deleteAll :: (MonadClient m) => UserId -> m ()
deleteAll u = write cql (params LocalQuorum (Identity u)) & retry x5
  where
    cql :: PrepQuery W (Identity UserId) ()
    cql = "DELETE FROM notifications WHERE user = ?"

-------------------------------------------------------------------------------
-- Conversions

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
            then Just (queuedNotification (Text.pack $ show notifId) pl)
            else Nothing
