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

-- | The original Cassandra implementation of "Gundeck.Notification.Data",
-- moved here verbatim. The two @MonadReader Env@ config reads are replaced by
-- the explicit 'NotificationStoreConfig' argument.
module Wire.NotificationStore.Cassandra
  ( interpretNotificationStoreToCassandra,
  )
where

import Cassandra as C
import Control.Error (MaybeT (..))
import Control.Lens ((^.), _1)
import Data.Aeson qualified as JSON
import Data.ByteString.Lazy qualified as BSL
import Data.Id
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Range (Range, fromRange)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Imports
import Polysemy
import Polysemy.Input
import UnliftIO (pooledForConcurrentlyN_)
import UnliftIO.Async (pooledMapConcurrentlyN)
import Wire.API.Internal.Notification
import Wire.NotificationStore
import Wire.Util (embedClientInput)

interpretNotificationStoreToCassandra ::
  ( Member (Embed IO) r,
    Member (Input ClientState) r,
    Member (Input NotificationStoreConfig) r
  ) =>
  Sem (NotificationStore ': r) a ->
  Sem r a
interpretNotificationStoreToCassandra = interpret $ \case
  AddNotification n tgts objs ttl ->
    embedClientInput $ add n tgts objs ttl
  FetchNotifications u c msince range -> do
    cfg <- input
    embedClientInput $ fetch cfg u c msince range
  FetchNotificationById u n c ->
    embedClientInput $ fetchId u n c
  FetchLastNotification u c -> do
    cfg <- input
    embedClientInput $ fetchLast cfg u c
  DeleteAllNotifications u ->
    embedClientInput $ deleteAll u

add ::
  (MonadClient m, MonadUnliftIO m) =>
  NotificationId ->
  NonEmpty NotificationTarget ->
  NonEmpty JSON.Object ->
  NotificationTTL ->
  m ()
add n tgts (JSON.encode -> payload) (notificationTTLSeconds -> t) = do
  -- inline payload when there is exactly one target
  let inlinePayload = null (NonEmpty.tail tgts)
  if inlinePayload
    then do
      pooledForConcurrentlyN_ 32 tgts $ \tgt ->
        let u = tgt ^. targetUser
            cs = C.Set (tgt ^. targetClients)
         in retry x5 $ write cqlInsertInline (params LocalQuorum (u, n, Blob payload, cs, fromIntegral t))
    else do
      payloadId <- randomId
      write cqlInsertPayload (params LocalQuorum (payloadId, Blob payload, fromIntegral t)) & retry x5
      let payloadRefSize = fromIntegral $ BSL.length payload

      pooledForConcurrentlyN_ 32 tgts $ \tgt ->
        let u = tgt ^. targetUser
            cs = C.Set (tgt ^. targetClients)
         in retry x5 $ write cqlInsertReference (params LocalQuorum (u, n, payloadId, payloadRefSize, cs, fromIntegral t))
  where
    cqlInsertInline :: PrepQuery W (UserId, NotificationId, Blob, C.Set ClientId, Int32) ()
    cqlInsertInline =
      "INSERT INTO notifications \
      \(user, id, payload, clients) VALUES \
      \(?   , ? , ?      , ?) \
      \USING TTL ?"
    cqlInsertReference :: PrepQuery W (UserId, NotificationId, PayloadId, Int32, C.Set ClientId, Int32) ()
    cqlInsertReference =
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

fetchLast :: (MonadClient m) => NotificationStoreConfig -> UserId -> Maybe ClientId -> m (Maybe QueuedNotification)
fetchLast cfg u c = do
  let pageSize = fromMaybe 100 (nscPageSize cfg)
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

data FetchPayloadsResult = FetchPayloadsResult
  { notifications :: Seq QueuedNotification,
    remainingBytes :: Int32,
    truncatedByPayloadLimit :: Bool
  }

-- | Fetches referenced payloads until maxTotalSize payload bytes are fetched from the database.
-- At least the first row is fetched regardless of the payload size.
fetchPayloads :: (MonadClient m, MonadUnliftIO m) => Maybe ClientId -> Int32 -> [NotifRow] -> m FetchPayloadsResult
fetchPayloads client remainingBytes rows = do
  let isFirstRow = True
  let (rows', remainingBytes', truncatedByPayloadLimit) = truncateNotifs [] isFirstRow remainingBytes rows
  notifications <- Seq.fromList . catMaybes <$> pooledMapConcurrentlyN 16 (fetchPayload client) rows'
  pure $ FetchPayloadsResult notifications remainingBytes' truncatedByPayloadLimit
  where
    truncateNotifs :: [NotifRow] -> Bool -> Int32 -> [NotifRow] -> ([NotifRow], Int32, Bool)
    truncateNotifs acc _ remainingBytes' [] = (reverse acc, remainingBytes', False)
    truncateNotifs acc isFirstRow remainingBytes' (row : rest)
      | not isFirstRow && remainingBytes' <= 0 = (reverse acc, remainingBytes', not (null rest))
      | otherwise = truncateNotifs (row : acc) False (remainingBytes' - payloadSize row) rest

-- | Tries to fetch @remaining@ many notifications.
-- The returned 'Seq' might contain more notifications than @remaining@, (see
-- https://docs.datastax.com/en/developer/java-driver/3.2/manual/paging/).
--
-- The boolean indicates whether more notifications can be fetched.
collect :: (MonadClient m, MonadUnliftIO m) => Maybe ClientId -> Seq QueuedNotification -> Bool -> Int -> Int32 -> m (Page NotifRow) -> m (Seq QueuedNotification, Bool)
collect c acc prevPageHasMore remaining remainingBytes getPage
  -- we have fetched at least the requested size: terminating the recursion
  | remaining <= 0 = pure (acc, prevPageHasMore)
  -- we reached or exceeded the max payload: terminating the recursion
  | remainingBytes <= 0 = pure (acc, prevPageHasMore)
  -- there is no more data: terminating the recursion
  | not prevPageHasMore = pure (acc, False)
  -- in any other case we are going to get the next page
  | otherwise = do
      page <- getPage
      let rows = result page
      fetchResult <- fetchPayloads c remainingBytes rows
      let remaining' = remaining - Seq.length fetchResult.notifications
          more' = hasMore page || fetchResult.truncatedByPayloadLimit
      collect c (acc <> fetchResult.notifications) more' remaining' fetchResult.remainingBytes (liftClient (nextPage page))

fetch :: (MonadClient m, MonadUnliftIO m) => NotificationStoreConfig -> UserId -> Maybe ClientId -> Maybe NotificationId -> Range 100 10000 Int32 -> m ResultPage
fetch cfg u c Nothing (fromIntegral . fromRange -> size) = do
  let pageSize = fromMaybe 100 (nscPageSize cfg)
  let page1 = retry x1 $ paginate cqlStart (paramsP LocalQuorum (Identity u) pageSize)
  -- We always need to look for one more than requested in order to correctly
  -- report whether there are more results.
  let maxPayloadSize = fromMaybe (5 * 1024 * 1024) (nscMaxPayloadLoadSize cfg)
  let prevPageHasMore = True
  (ns, more) <- collect c Seq.empty prevPageHasMore (size + 1) maxPayloadSize page1
  -- Drop the extra element at the end if present
  pure $! mkResultPage size more ns
  where
    cqlStart :: PrepQuery R (Identity UserId) NotifRow
    cqlStart =
      "SELECT id, payload, payload_ref, payload_ref_size, clients \
      \FROM notifications \
      \WHERE user = ? \
      \ORDER BY id ASC"
fetch cfg u c (Just since) (fromIntegral . fromRange -> size) = do
  let pageSize = fromMaybe 100 (nscPageSize cfg)
  sinceFound <- isJust <$> retry x1 (query1 cqlExists (params LocalQuorum (u, TimeUuid (toUUID since))))
  let page1 =
        retry x1 $
          paginate cqlAfterSince (paramsP LocalQuorum (u, TimeUuid (toUUID since)) pageSize)
  let maxPayloadSize = fromMaybe (5 * 1024 * 1024) (nscMaxPayloadLoadSize cfg)
  let prevPageHasMore = True
  -- We always need to look for one more than requested in order to correctly
  -- report whether there are more results.
  (ns, more) <- collect c Seq.empty prevPageHasMore (size + 1) maxPayloadSize page1
  pure $!
    (mkResultPage size more ns)
      { resultGap = not sinceFound
      }
  where
    cqlExists :: PrepQuery R (UserId, TimeUuid) (Identity TimeUuid)
    cqlExists =
      "SELECT id \
      \FROM notifications \
      \WHERE user = ? AND id = ?"

    cqlAfterSince :: PrepQuery R (UserId, TimeUuid) NotifRow
    cqlAfterSince =
      "SELECT id, payload, payload_ref, payload_ref_size, clients \
      \FROM notifications \
      \WHERE user = ? AND id > ? \
      \ORDER BY id ASC"

deleteAll :: (MonadClient m) => UserId -> m ()
deleteAll u = write cql (params LocalQuorum (Identity u)) & retry x5
  where
    cql :: PrepQuery W (Identity UserId) ()
    cql = "DELETE FROM notifications WHERE user = ?"
