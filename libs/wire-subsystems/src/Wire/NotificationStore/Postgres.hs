-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

-- | Postgres implementation of the user notification store.
--
-- The row layout mirrors the Cassandra @notifications@ /
-- @notification_payload@ tables. A notification is either stored inline
-- (single target: @payload@ is set) or fanned out over a shared
-- @notification_payload@ row referenced by @payload_ref@. 'expires_at' plays
-- the role of Cassandra's row TTL and is garbage-collected by the hourly
-- pg_cron schedules installed by the migration.
module Wire.NotificationStore.Postgres
  ( interpretNotificationStoreToPostgres,
    notificationIdTime,
    insertNotificationRow,
    insertNotificationPayloadRow,
  )
where

import Cassandra (Blob (..), TimeUuid (..))
import Cassandra qualified as Cql
import Control.Lens ((^.))
import Data.Aeson qualified as JSON
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString qualified as BS
import Data.ByteString.Conversion (fromByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Id (ClientId, Id (..), UserId, clientToText, toUUID)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Range (Range, fromRange)
import Data.Ratio ((%))
import Data.Text.Encoding qualified as Text
import Data.Sequence (Seq)
import Data.Set qualified as Set
import Data.Sequence qualified as Seq
import Data.Time
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import Polysemy.Input
import Wire.API.Internal.Notification (NotificationId, NotificationTarget, QueuedNotification, targetClients, targetUser)
import Wire.API.Notification qualified as API
import Wire.NotificationStore
import Wire.Postgres

interpretNotificationStoreToPostgres ::
  (PGConstraints r, Member (Input NotificationStoreConfig) r) =>
  Sem (NotificationStore ': r) a ->
  Sem r a
interpretNotificationStoreToPostgres = interpret $ \case
  AddNotification nid tgts objs ttl -> pgAddNotification nid tgts objs ttl
  FetchNotifications u c msince range -> pgFetchNotifications u c msince range
  FetchNotificationById u n c -> pgFetchNotificationById u n c
  FetchLastNotification u c -> pgFetchLastNotification u c
  DeleteAllNotifications u ->
    runStatement (toUUID u) deleteAllStatement

-- | Note: like the Cassandra implementation, 'DeleteAllNotifications' does
-- not touch @notification_payload@; unreferenced payload rows expire via
-- TTL / pg_cron.

pgAddNotification ::
  (PGConstraints r) =>
  NotificationId ->
  NonEmpty NotificationTarget ->
  NonEmpty JSON.Object ->
  NotificationTTL ->
  Sem r ()
pgAddNotification nid tgts objs (notificationTTLSeconds -> ttlSeconds) = do
  now <- embed getCurrentTime
  let payload = LBS.toStrict (JSON.encode objs)
      occurred = fromMaybe now (notificationIdTime nid)
      -- like Cassandra's USING TTL, the row expires relative to the write
      -- time (this also matches the copy loop's 'now + remaining' anchor)
      expires = addUTCTime (fromIntegral ttlSeconds) now
  if null (NonEmpty.tail tgts)
    then
      -- inline payload when there is exactly one target
      for_ tgts $ \tgt ->
        insertNotificationRow
          (tgt ^. targetUser)
          nid
          (Just payload)
          Nothing
          Nothing
          (Set.fromList (tgt ^. targetClients))
          occurred
          expires
    else do
      payloadId <- Id <$> embed UUID.nextRandom
      insertNotificationPayloadRow payloadId payload occurred expires
      let payloadRefSize = fromIntegral (BS.length payload)
      for_ tgts $ \tgt ->
        insertNotificationRow
          (tgt ^. targetUser)
          nid
          Nothing
          (Just payloadId)
          (Just payloadRefSize)
          (Set.fromList (tgt ^. targetClients))
          occurred
          expires

-- | Insert a @notifications@ row. Shared by the runtime path and the
-- migration copy loop so they cannot drift. The payload bytes are copied
-- verbatim (no re-encode), faithful to the Cassandra copy.
insertNotificationRow ::
  (PGConstraints r) =>
  UserId ->
  NotificationId ->
  -- | inline payload
  Maybe ByteString ->
  -- | payload ref
  Maybe PayloadId ->
  -- | payload ref size
  Maybe Int32 ->
  Set ClientId ->
  UTCTime ->
  UTCTime ->
  Sem r ()
insertNotificationRow u nid mbPayload mbPayloadRef mbPayloadRefSize clients occurred expires =
  runStatement
    ( toUUID u,
      toUUID nid,
      mbPayload,
      toUUID <$> mbPayloadRef,
      mbPayloadRefSize,
      Vector.fromList (map clientToText (Set.toList clients)),
      occurred,
      expires
    )
    insertNotificationRowStatement

insertNotificationRowStatement :: Hasql.Statement (UUID, UUID, Maybe ByteString, Maybe UUID, Maybe Int32, Vector Text, UTCTime, UTCTime) ()
insertNotificationRowStatement =
  [resultlessStatement|
      INSERT INTO notifications
        (user_id, id, payload, payload_ref, payload_ref_size, clients, occurred_at, expires_at)
      VALUES
        ($1 :: uuid, $2 :: uuid, $3 :: bytea?, $4 :: uuid?, $5 :: int4?, $6 :: text[], $7 :: timestamptz, $8 :: timestamptz)
      ON CONFLICT (user_id, id) DO NOTHING
    |]

insertNotificationPayloadRow ::
  (PGConstraints r) =>
  PayloadId ->
  ByteString ->
  UTCTime ->
  UTCTime ->
  Sem r ()
insertNotificationPayloadRow payloadId payload occurred expires =
  runStatement
    (toUUID payloadId, payload, occurred, expires)
    insertNotificationPayloadRowStatement

insertNotificationPayloadRowStatement :: Hasql.Statement (UUID, ByteString, UTCTime, UTCTime) ()
insertNotificationPayloadRowStatement =
  [resultlessStatement|
      INSERT INTO notification_payload (id, payload, occurred_at, expires_at)
      VALUES ($1 :: uuid, $2 :: bytea, $3 :: timestamptz, $4 :: timestamptz)
      ON CONFLICT (id) DO NOTHING
    |]

-- | One row of @notifications@:
-- (occurred_at, id, payload, payload_ref, payload_ref_size, clients)
type PGNotifRow = (UTCTime, UUID, Maybe ByteString, Maybe UUID, Maybe Int32, Vector Text)

rowClients :: Vector Text -> Maybe (Cql.Set ClientId)
rowClients cs
  | Vector.null cs = Nothing
  | otherwise = Just . Cql.Set . mapMaybe (fromByteString . Text.encodeUtf8) . Vector.toList $ cs

-- | Row size in payload bytes (inline length or stored ref size), mirroring
-- 'payloadSize' of the Cassandra implementation.
rowPayloadSize :: PGNotifRow -> Int32
rowPayloadSize (_, _, mbPayload, _, mbPayloadRefSize, _) =
  case (mbPayload, mbPayloadRefSize) of
    (Just payload, _) -> fromIntegral (BS.length payload)
    (_, Just size) -> size
    _ -> 0

-- | Truncate rows over the payload byte budget, keeping at least the first
-- row regardless of its size (verbatim port of the Cassandra reader).
truncateNotifs :: Int32 -> [PGNotifRow] -> ([PGNotifRow], Int32, Bool)
truncateNotifs = go True []
  where
    go _ acc remaining [] = (reverse acc, remaining, False)
    go isFirstRow acc remaining (row : rest)
      | not isFirstRow && remaining <= 0 = (reverse acc, remaining, not (null rest))
      | otherwise = go False (row : acc) (remaining - rowPayloadSize row) rest

-- | Resolve rows to notifications: inline payloads directly, referenced
-- payloads via one batched lookup of the @notification_payload@ table
-- (replacing the per-row concurrent lookups of the Cassandra reader).
-- Rows whose referenced payload is missing or expired are skipped, and
-- undecodable payloads are skipped silently, like the Cassandra reader.
resolveRows ::
  (PGConstraints r) =>
  Maybe ClientId ->
  [PGNotifRow] ->
  Sem r (Seq QueuedNotification)
resolveRows c rows = do
  let refs = mapMaybe (\(_, _, _, ref, _, _) -> ref) rows
  payloadMap <-
    if null refs
      then pure Map.empty
      else (Map.fromList . Vector.toList) <$> runStatement (Vector.fromList refs) selectPayloadsStatement
  pure . Seq.fromList . mapMaybe (resolveWith payloadMap) $ rows
  where
    resolveWith pm row@(_, _, mbPayload, mbRef, _, cs) =
      case (mbPayload, mbRef) of
        (Just payload, _) -> decodeAndFilter c row cs payload
        (_, Just ref) ->
          -- referenced payloads are only visible while unexpired
          Map.lookup ref pm >>= decodeAndFilter c row cs
        _ -> Nothing

decodeAndFilter ::
  Maybe ClientId ->
  PGNotifRow ->
  Vector Text ->
  ByteString ->
  Maybe QueuedNotification
decodeAndFilter c (_, i, _, _, _, _) cs payload =
  toNotifSingle c (TimeUuid i, Blob (LBS.fromStrict payload), rowClients cs)

pgFetchNotifications ::
  (PGConstraints r, Member (Input NotificationStoreConfig) r) =>
  UserId ->
  Maybe ClientId ->
  Maybe NotificationId ->
  Range 100 10000 Int32 ->
  Sem r ResultPage
pgFetchNotifications u c msince (fromRange -> size0) = do
  cfg <- input
  let pageSize = fromMaybe 100 (nscPageSize cfg)
      maxPayloadSize = fromMaybe (5 * 1024 * 1024) (nscMaxPayloadLoadSize cfg)
      size = fromIntegral size0 :: Int
      pageSize64 = fromIntegral pageSize :: Int64
  -- The Cassandra reader checks separately whether the 'since' row exists
  -- (for 'resultGap') and queries strictly after it. We keep those semantics.
  sinceFound <- case msince of
    Nothing -> pure True
    Just since -> isJust <$> runStatement (toUUID u, toUUID since) existsStatement
  -- We always need to look for one more than requested in order to correctly
  -- report whether there are more results.
  let firstPage = case msince of
        Nothing -> runStatement (toUUID u, pageSize64) fetchPageStartStatement
        Just since ->
          runStatement
            (toUUID u, sinceTime since, sinceTime since, toUUID since, pageSize64)
            fetchPageSinceStatement
      prevPageHasMore = True
  (ns, more) <-
    collect pageSize Seq.empty prevPageHasMore (size + 1) maxPayloadSize firstPage
  pure $!
    (mkResultPage size more ns)
      { resultGap = not sinceFound
      }
  where
    -- Keyset paging over (occurred_at, id). 'hasMore' is approximated as
    -- "the last page came back full" (see the note in
    -- "Wire.TeamNotificationStore.Postgres").
    collect pageSize acc prevPageHasMore remaining remainingBytes getPage
      | remaining <= 0 = pure (acc, prevPageHasMore)
      | remainingBytes <= 0 = pure (acc, prevPageHasMore)
      | not prevPageHasMore = pure (acc, False)
      | otherwise = do
          rows <- getPage
          let rowList = Vector.toList rows
              (rows', remainingBytes', truncated) = truncateNotifs remainingBytes rowList
          notifs <- resolveRows c rows'
          let remaining' = remaining - Seq.length notifs
              more' = Vector.length rows == fromIntegral pageSize || truncated
              nextPage = case unsnoc (Vector.toList rows) of
                Just (_, (occurred, i, _, _, _, _)) ->
                  runStatement (toUUID u, occurred, occurred, i, fromIntegral pageSize) fetchPageNextStatement
                Nothing -> pure Vector.empty
          collect pageSize (acc <> notifs) more' remaining' remainingBytes' nextPage

fetchPageStartStatement :: Hasql.Statement (UUID, Int64) (Vector PGNotifRow)
fetchPageStartStatement =
  [vectorStatement|
      SELECT occurred_at :: timestamptz, id :: uuid, payload :: bytea?, payload_ref :: uuid?, payload_ref_size :: int4?, clients :: text[]
      FROM notifications
      WHERE user_id = ($1 :: uuid) AND expires_at > now()
      ORDER BY occurred_at ASC, id ASC
      LIMIT ($2 :: int8)
    |]

fetchPageSinceStatement :: Hasql.Statement (UUID, UTCTime, UTCTime, UUID, Int64) (Vector PGNotifRow)
fetchPageSinceStatement =
  [vectorStatement|
      SELECT occurred_at :: timestamptz, id :: uuid, payload :: bytea?, payload_ref :: uuid?, payload_ref_size :: int4?, clients :: text[]
      FROM notifications
      WHERE user_id = ($1 :: uuid) AND expires_at > now()
        AND (occurred_at > ($2 :: timestamptz) OR (occurred_at = ($3 :: timestamptz) AND id > ($4 :: uuid)))
      ORDER BY occurred_at ASC, id ASC
      LIMIT ($5 :: int8)
    |]

fetchPageNextStatement :: Hasql.Statement (UUID, UTCTime, UTCTime, UUID, Int64) (Vector PGNotifRow)
fetchPageNextStatement =
  [vectorStatement|
      SELECT occurred_at :: timestamptz, id :: uuid, payload :: bytea?, payload_ref :: uuid?, payload_ref_size :: int4?, clients :: text[]
      FROM notifications
      WHERE user_id = ($1 :: uuid) AND expires_at > now()
        AND (occurred_at > ($2 :: timestamptz) OR (occurred_at = ($3 :: timestamptz) AND id > ($4 :: uuid)))
      ORDER BY occurred_at ASC, id ASC
      LIMIT ($5 :: int8)
    |]

existsStatement :: Hasql.Statement (UUID, UUID) (Maybe Bool)
existsStatement =
  [maybeStatement|
      SELECT true :: bool
      FROM notifications
      WHERE user_id = ($1 :: uuid) AND id = ($2 :: uuid) AND expires_at > now()
    |]

selectPayloadsStatement :: Hasql.Statement (Vector UUID) (Vector (UUID, ByteString))
selectPayloadsStatement =
  [vectorStatement|
      SELECT id :: uuid, payload :: bytea
      FROM notification_payload
      WHERE id = ANY (($1 :: uuid[])) AND expires_at > now()
    |]

pgFetchNotificationById ::
  (PGConstraints r) =>
  UserId ->
  NotificationId ->
  Maybe ClientId ->
  Sem r (Maybe QueuedNotification)
pgFetchNotificationById u n c = do
  rows <- runStatement (toUUID u, toUUID n) fetchByIdStatement
  notifs <- resolveRows c (Vector.toList rows)
  pure $ case Seq.viewl notifs of
    Seq.EmptyL -> Nothing
    x Seq.:< _ -> Just x

fetchByIdStatement :: Hasql.Statement (UUID, UUID) (Vector PGNotifRow)
fetchByIdStatement =
  [vectorStatement|
      SELECT occurred_at :: timestamptz, id :: uuid, payload :: bytea?, payload_ref :: uuid?, payload_ref_size :: int4?, clients :: text[]
      FROM notifications
      WHERE user_id = ($1 :: uuid) AND id = ($2 :: uuid) AND expires_at > now()
    |]

-- | Newest notification for a user: walk (occurred_at, id)-descending pages,
-- skipping undecodable and client-filtered rows, until a notification is
-- found or the user has none left (port of the cqlLast/cqlSeek loop).
pgFetchLastNotification ::
  (PGConstraints r, Member (Input NotificationStoreConfig) r) =>
  UserId ->
  Maybe ClientId ->
  Sem r (Maybe QueuedNotification)
pgFetchLastNotification u c = do
  cfg <- input
  let pageSize = fromMaybe 100 (nscPageSize cfg)
      pageSize64 = fromIntegral pageSize :: Int64
  -- The first page consists of at most one row (the newest notification).
  firstRows <- runStatement (toUUID u) fetchLastFirstStatement
  case Vector.toList firstRows of
    [] -> pure Nothing
    (seed : rest) -> do
      -- the seed row itself is the newest candidate (the first statement is
      -- LIMIT 1, so 'rest' is empty; tryRows keeps us from skipping it)
      found <- tryRows c (seed : rest)
      case found of
        Just n -> pure (Just n)
        Nothing -> seekLoop pageSize64 pageSize seed
  where
    tryRows :: (PGConstraints r) => Maybe ClientId -> [PGNotifRow] -> Sem r (Maybe QueuedNotification)
    tryRows _ [] = pure Nothing
    tryRows cl (row : rest) = do
      notifs <- resolveRows cl [row]
      case Seq.viewl notifs of
        Seq.EmptyL -> tryRows cl rest
        x Seq.:< _ -> pure (Just x)

    seekLoop pageSize64 pageSize seed = do
      let (occurred, i, _, _, _, _) = seed
      rows <- runStatement (toUUID u, occurred, occurred, i, pageSize64) fetchLastSeekStatement
      let rowList = Vector.toList rows
          more = length rowList == fromIntegral pageSize
      found <- tryRows c rowList
      case (found, unsnoc rowList) of
        (Just n, _) -> pure (Just n)
        (Nothing, _) | not more -> pure Nothing
        (Nothing, Just (_, lastRow)) -> seekLoop pageSize64 pageSize lastRow
        (Nothing, Nothing) -> pure Nothing

-- | First row in descending (occurred_at, id) order: at most one row.
fetchLastFirstStatement :: Hasql.Statement UUID (Vector PGNotifRow)
fetchLastFirstStatement =
  [vectorStatement|
      SELECT occurred_at :: timestamptz, id :: uuid, payload :: bytea?, payload_ref :: uuid?, payload_ref_size :: int4?, clients :: text[]
      FROM notifications
      WHERE user_id = ($1 :: uuid) AND expires_at > now()
      ORDER BY occurred_at DESC, id DESC
      LIMIT 1
    |]

fetchLastSeekStatement :: Hasql.Statement (UUID, UTCTime, UTCTime, UUID, Int64) (Vector PGNotifRow)
fetchLastSeekStatement =
  [vectorStatement|
      SELECT occurred_at :: timestamptz, id :: uuid, payload :: bytea?, payload_ref :: uuid?, payload_ref_size :: int4?, clients :: text[]
      FROM notifications
      WHERE user_id = ($1 :: uuid) AND expires_at > now()
        AND (occurred_at < ($2 :: timestamptz) OR (occurred_at = ($3 :: timestamptz) AND id < ($4 :: uuid)))
      ORDER BY occurred_at DESC, id DESC
      LIMIT ($5 :: int8)
    |]

deleteAllStatement :: Hasql.Statement UUID ()
deleteAllStatement =
  [resultlessStatement|
      DELETE FROM notifications WHERE user_id = ($1 :: uuid)
    |]

-- | RFC 4122 v1 epoch: 1582-10-15T00:00:00Z.
v1Epoch :: UTCTime
v1Epoch = UTCTime (fromGregorian 1582 10 15) 0

-- | Decode the RFC 4122 v1 timestamp from a notification id.
-- @Nothing@ when the id is not a v1 UUID.
notificationIdTime :: NotificationId -> Maybe UTCTime
notificationIdTime (Id uuid)
  | API.isValidNotificationId (Id uuid) =
      let (w1, w2, _, _) = UUID.toWords uuid
          timeLow = fromIntegral w1 :: Word64
          timeMid = fromIntegral w2 `shiftR` 16 :: Word64
          timeHi = fromIntegral w2 .&. 0xfff :: Word64
          -- 60-bit count of 100ns intervals since the v1 epoch
          ticks = (timeHi `shiftL` 48) .|. (timeMid `shiftL` 32) .|. timeLow
       in Just $!
            addUTCTime
              (fromRational (fromIntegral ticks * 100 % 1000000000))
              v1Epoch
notificationIdTime _ = Nothing

-- | 'since' read bound: notification ids are v1 by construction; the
-- fallback only matters for a caller passing a malformed id. Anchor at the
-- v1 epoch instead.
sinceTime :: NotificationId -> UTCTime
sinceTime nid = fromMaybe v1Epoch (notificationIdTime nid)
