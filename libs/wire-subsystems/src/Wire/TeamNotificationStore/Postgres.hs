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

module Wire.TeamNotificationStore.Postgres
  ( interpretTeamNotificationStoreToPostgres,
    notificationIdTime,
  )
where

import Data.Aeson qualified as JSON
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString.Lazy qualified as LBS
import Data.Id (Id (..), TeamId, toUUID)
import Data.List.NonEmpty (NonEmpty)
import Data.Range (Range, fromRange)
import Data.Ratio ((%))
import Data.Sequence qualified as Seq
import Data.Time
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import Wire.API.Internal.Notification (NotificationId, queuedNotification)
import Wire.API.Notification qualified as API
import Wire.API.PostgresMarshall (lmapPG)
import Wire.Postgres
import Wire.TeamNotificationStore

interpretTeamNotificationStoreToPostgres ::
  (PGConstraints r) =>
  Sem (TeamNotificationStore ': r) a ->
  Sem r a
interpretTeamNotificationStoreToPostgres = interpret $ \case
  CreateTeamNotification tid nid objs -> do
    insertTeamNotification tid nid objs
  GetTeamNotifications tid mnid lim -> do
    fetchTeamNotifications tid mnid lim
  MkNotificationId ->
    embed (API.mkNotificationId @IO)

-- | Keep in sync with 'notificationTTLSeconds' in
-- "Wire.TeamNotificationStore.Cassandra": 28 days.
notificationTTL :: NominalDiffTime
notificationTTL = 28 * nominalDay

-- | Insert a notification row. Shared by the runtime path and the migration
-- copy loop so they cannot drift. @occurred_at@ is decoded from the RFC 4122
-- v1 timestamp bits of the notification id (falling back to @now@ for a
-- non-v1 id, which cannot happen for ids produced by 'mkNotificationId');
-- @expires_at@ is anchored at the occurrence, so rows copied late from
-- Cassandra keep their original expiry.
insertTeamNotification :: (PGConstraints r) => TeamId -> NotificationId -> NonEmpty JSON.Object -> Sem r ()
insertTeamNotification tid nid objs = do
  now <- embed getCurrentTime
  let occurred = fromMaybe now (notificationIdTime nid)
      payload = LBS.toStrict (JSON.encode objs)
      expires = addUTCTime notificationTTL occurred
  runStatement (toUUID tid, toUUID nid, payload, occurred, expires) insertTeamNotificationStatement

insertTeamNotificationStatement :: Hasql.Statement (UUID, UUID, ByteString, UTCTime, UTCTime) ()
insertTeamNotificationStatement =
  lmapPG
    [resultlessStatement|
      INSERT INTO team_notifications (team, id, payload, occurred_at, expires_at)
      VALUES ($1 :: uuid, $2 :: uuid, $3 :: bytea, $4 :: timestamptz, $5 :: timestamptz)
      ON CONFLICT (team, id) DO NOTHING
    |]

fetchTeamNotifications :: (PGConstraints r) => TeamId -> Maybe NotificationId -> Range 1 10000 Int32 -> Sem r ResultPage
fetchTeamNotifications tid mnid (fromRange -> size0) = do
  -- Mirror the Cassandra page arithmetic: request one more row than requested
  -- (two more when a 'since' is given, which itself counts as a result row) to
  -- be able to report 'resultHasMore'.
  let size = fromIntegral size0 :: Int
      limitBound = bool (size + 1) (size + 2) (isJust mnid)
  rows <- case mnid of
    Nothing ->
      runStatement (toUUID tid, fromIntegral limitBound) fetchTeamNotificationsStartStatement
    Just since ->
      runStatement (toUUID tid, sinceTime since, toUUID since, fromIntegral limitBound) fetchTeamNotificationsSinceStatement
  -- Decode payloads in ascending order (silently skipping malformed rows,
  -- like the Cassandra reader does), then drop the extra row that determined
  -- 'resultHasMore'. The 'since' row itself is kept (inclusive semantics).
  -- Note: unlike the Cassandra driver, 'resultHasMore' is reported true when
  -- the result set exactly fills the fetched limit; clients only pay one
  -- extra empty page.
  let decoded =
        Vector.mapMaybe
          ( \(i, payload) ->
              queuedNotification (Id i) <$> JSON.decode' (LBS.fromStrict payload)
          )
          rows
      more = Vector.length rows == limitBound
      kept = bool decoded (Vector.take (Vector.length decoded - 1) decoded) more
  pure $ ResultPage (Seq.fromList (Vector.toList kept)) more
  where
    -- Notification ids are v1 by construction; the fallback only matters for a
    -- caller passing a malformed id, in which case the Cassandra reader would
    -- compare raw timeuuid bytes. Anchor at the v1 epoch instead.
    sinceTime nid = fromMaybe v1Epoch (notificationIdTime nid)

fetchTeamNotificationsStartStatement :: Hasql.Statement (UUID, Int64) (Vector (UUID, ByteString))
fetchTeamNotificationsStartStatement =
  [vectorStatement|
      SELECT id :: uuid, payload :: bytea
      FROM team_notifications
      WHERE team = ($1 :: uuid) AND expires_at > now()
      ORDER BY occurred_at ASC, id ASC
      LIMIT ($2 :: int8)
    |]

fetchTeamNotificationsSinceStatement :: Hasql.Statement (UUID, UTCTime, UUID, Int64) (Vector (UUID, ByteString))
fetchTeamNotificationsSinceStatement =
  [vectorStatement|
      SELECT id :: uuid, payload :: bytea
      FROM team_notifications
      WHERE team = ($1 :: uuid)
        AND expires_at > now()
        AND (occurred_at, id) >= (($2 :: timestamptz), ($3 :: uuid))
      ORDER BY occurred_at ASC, id ASC
      LIMIT ($4 :: int8)
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
