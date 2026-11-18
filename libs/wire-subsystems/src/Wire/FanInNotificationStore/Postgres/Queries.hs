{-# LANGUAGE StrictData #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

-- | Fixed, parameterized SQL for each fan-in notification table. The generic
-- statement bundle is private to the Postgres implementation; the store effect
-- keeps separate typed operations for each scope.
module Wire.FanInNotificationStore.Postgres.Queries where

import Data.Domain (Domain, domainText)
import Data.Id
import Data.Profunctor (dimap, lmap)
import Data.Time.Clock (UTCTime)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Hasql.Statement (Statement)
import Hasql.TH
import Imports
import Wire.API.MLS.Epoch (Epoch (..))
import Wire.API.MLS.Group (GroupId (..))
import Wire.API.Notification (NotificationId)
import Wire.FanInNotificationStore

data NotificationStatements key = NotificationStatements
  { insertNotifications :: Statement (Vector (key, FanInNotification, UTCTime)) (),
    selectPage :: Statement (key, Maybe NotificationId, Int32) (Vector FanInNotification),
    selectLast :: Statement key (Maybe NotificationId),
    notificationExists :: Statement (key, NotificationId) Bool
  }

-- | The data-modifying CTE and cursor upsert form one atomic statement. Cursor
-- updates use GREATEST so retries and older events cannot move the head back.
userStatements :: NotificationStatements UserId
userStatements =
  NotificationStatements
    { insertNotifications =
        lmap
          ( \rows ->
              (V.map (\(key, _, _) -> toUUID key) rows, V.map (\(_, n, _) -> toUUID n.fanInNotificationId) rows, V.map (\(_, n, _) -> n.fanInNotificationPayload) rows, V.map (\(_, n, _) -> toUUID <$> n.fanInNotificationOrigin) rows, V.map (\(_, _, createdAt) -> createdAt) rows)
          )
          [resultlessStatement|WITH incoming (user_id, notification_id, payload, origin, created_at) AS (
          SELECT * FROM unnest($1 :: uuid[], $2 :: uuid[], $3 :: jsonb[], $4 :: uuid?[], $5 :: timestamptz[])
        ), written AS (
          INSERT INTO user_notifications (user_id, notification_id, payload, origin, created_at)
          SELECT * FROM incoming ORDER BY user_id, notification_id, created_at
          ON CONFLICT (user_id, notification_id, created_at) DO NOTHING
        )
        INSERT INTO last_user_notifications (user_id, notification_id)
        SELECT DISTINCT ON (user_id) user_id, notification_id
        FROM incoming ORDER BY user_id, notification_id DESC
        ON CONFLICT (user_id) DO UPDATE
          SET notification_id = GREATEST(last_user_notifications.notification_id, EXCLUDED.notification_id)|],
      selectPage =
        dimap
          (\(key, cursor, pageSize) -> (toUUID key, toUUID <$> cursor, pageSize))
          (V.map (\(nid, payload, origin) -> FanInNotification (Id nid) payload (Id <$> origin)))
          [vectorStatement|
        SELECT notification_id :: uuid, payload :: jsonb, origin :: uuid?
        FROM user_notifications
        WHERE user_id = ($1 :: uuid)
          AND (($2 :: uuid?) IS NULL OR notification_id > ($2 :: uuid?))
        ORDER BY notification_id
        LIMIT ($3 :: int4)
      |],
      selectLast =
        dimap
          (\key -> toUUID key)
          (fmap Id)
          [maybeStatement|
        SELECT notification_id :: uuid FROM last_user_notifications WHERE user_id = ($1 :: uuid)
      |],
      notificationExists =
        lmap
          (Data.Bifunctor.bimap toUUID toUUID)
          [singletonStatement|
        SELECT EXISTS (SELECT 1 FROM user_notifications
          WHERE user_id = ($1 :: uuid) AND notification_id = ($2 :: uuid)) :: bool
      |]
    }

-- | The data-modifying CTE and cursor upsert form one atomic statement. Cursor
-- updates use GREATEST so retries and older events cannot move the head back.
clientStatements :: NotificationStatements (UserId, ClientId)
clientStatements =
  NotificationStatements
    { insertNotifications =
        lmap
          ( \rows ->
              (V.map (\((uid, _), _, _) -> toUUID uid) rows, V.map (\((_, cid), _, _) -> fromIntegral (clientToWord64 cid)) rows, V.map (\(_, n, _) -> toUUID n.fanInNotificationId) rows, V.map (\(_, n, _) -> n.fanInNotificationPayload) rows, V.map (\(_, n, _) -> toUUID <$> n.fanInNotificationOrigin) rows, V.map (\(_, _, createdAt) -> createdAt) rows)
          )
          [resultlessStatement|WITH incoming (user_id, client_id, notification_id, payload, origin, created_at) AS (
          SELECT * FROM unnest($1 :: uuid[], $2 :: int8[], $3 :: uuid[], $4 :: jsonb[], $5 :: uuid?[], $6 :: timestamptz[])
        ), written AS (
          INSERT INTO client_notifications (user_id, client_id, notification_id, payload, origin, created_at)
          SELECT * FROM incoming ORDER BY user_id, client_id, notification_id, created_at
          ON CONFLICT (user_id, client_id, notification_id, created_at) DO NOTHING
        )
        INSERT INTO last_client_notifications (user_id, client_id, notification_id)
        SELECT DISTINCT ON (user_id, client_id) user_id, client_id, notification_id
        FROM incoming ORDER BY user_id, client_id, notification_id DESC
        ON CONFLICT (user_id, client_id) DO UPDATE
          SET notification_id = GREATEST(last_client_notifications.notification_id, EXCLUDED.notification_id)|],
      selectPage =
        dimap
          (\((uid, cid), cursor, pageSize) -> (toUUID uid, fromIntegral (clientToWord64 cid), toUUID <$> cursor, pageSize))
          (V.map (\(nid, payload, origin) -> FanInNotification (Id nid) payload (Id <$> origin)))
          [vectorStatement|
        SELECT notification_id :: uuid, payload :: jsonb, origin :: uuid?
        FROM client_notifications
        WHERE user_id = ($1 :: uuid) AND client_id = ($2 :: int8)
          AND (($3 :: uuid?) IS NULL OR notification_id > ($3 :: uuid?))
        ORDER BY notification_id
        LIMIT ($4 :: int4)
      |],
      selectLast =
        dimap
          (\(uid, cid) -> (toUUID uid, fromIntegral (clientToWord64 cid)))
          (fmap Id)
          [maybeStatement|
        SELECT notification_id :: uuid FROM last_client_notifications WHERE user_id = ($1 :: uuid) AND client_id = ($2 :: int8)
      |],
      notificationExists =
        lmap
          (\((uid, cid), nid) -> (toUUID uid, fromIntegral (clientToWord64 cid), toUUID nid))
          [singletonStatement|
        SELECT EXISTS (SELECT 1 FROM client_notifications
          WHERE user_id = ($1 :: uuid) AND client_id = ($2 :: int8) AND notification_id = ($3 :: uuid)) :: bool
      |]
    }

-- | The data-modifying CTE and cursor upsert form one atomic statement. Cursor
-- updates use GREATEST so retries and older events cannot move the head back.
teamStatements :: NotificationStatements TeamId
teamStatements =
  NotificationStatements
    { insertNotifications =
        lmap
          ( \rows ->
              (V.map (\(key, _, _) -> toUUID key) rows, V.map (\(_, n, _) -> toUUID n.fanInNotificationId) rows, V.map (\(_, n, _) -> n.fanInNotificationPayload) rows, V.map (\(_, n, _) -> toUUID <$> n.fanInNotificationOrigin) rows, V.map (\(_, _, createdAt) -> createdAt) rows)
          )
          [resultlessStatement|WITH incoming (team_id, notification_id, payload, origin, created_at) AS (
          SELECT * FROM unnest($1 :: uuid[], $2 :: uuid[], $3 :: jsonb[], $4 :: uuid?[], $5 :: timestamptz[])
        ), written AS (
          INSERT INTO team_notifications (team_id, notification_id, payload, origin, created_at)
          SELECT * FROM incoming ORDER BY team_id, notification_id, created_at
          ON CONFLICT (team_id, notification_id, created_at) DO NOTHING
        )
        INSERT INTO last_team_notifications (team_id, notification_id)
        SELECT DISTINCT ON (team_id) team_id, notification_id
        FROM incoming ORDER BY team_id, notification_id DESC
        ON CONFLICT (team_id) DO UPDATE
          SET notification_id = GREATEST(last_team_notifications.notification_id, EXCLUDED.notification_id)|],
      selectPage =
        dimap
          (\(key, cursor, pageSize) -> (toUUID key, toUUID <$> cursor, pageSize))
          (V.map (\(nid, payload, origin) -> FanInNotification (Id nid) payload (Id <$> origin)))
          [vectorStatement|
        SELECT notification_id :: uuid, payload :: jsonb, origin :: uuid?
        FROM team_notifications
        WHERE team_id = ($1 :: uuid)
          AND (($2 :: uuid?) IS NULL OR notification_id > ($2 :: uuid?))
        ORDER BY notification_id
        LIMIT ($3 :: int4)
      |],
      selectLast =
        dimap
          (\key -> toUUID key)
          (fmap Id)
          [maybeStatement|
        SELECT notification_id :: uuid FROM last_team_notifications WHERE team_id = ($1 :: uuid)
      |],
      notificationExists =
        lmap
          (Data.Bifunctor.bimap toUUID toUUID)
          [singletonStatement|
        SELECT EXISTS (SELECT 1 FROM team_notifications
          WHERE team_id = ($1 :: uuid) AND notification_id = ($2 :: uuid)) :: bool
      |]
    }

-- | The data-modifying CTE and cursor upsert form one atomic statement. Cursor
-- updates use GREATEST so retries and older events cannot move the head back.
epochStatements :: NotificationStatements (GroupId, Epoch)
epochStatements =
  NotificationStatements
    { insertNotifications =
        lmap
          ( \rows ->
              (V.map (\((gid, _), _, _) -> unGroupId gid) rows, V.map (\((_, ep), _, _) -> fromIntegral (epochNumber ep)) rows, V.map (\(_, n, _) -> toUUID n.fanInNotificationId) rows, V.map (\(_, n, _) -> n.fanInNotificationPayload) rows, V.map (\(_, n, _) -> toUUID <$> n.fanInNotificationOrigin) rows, V.map (\(_, _, createdAt) -> createdAt) rows)
          )
          [resultlessStatement|WITH incoming (group_id, epoch, notification_id, payload, origin, created_at) AS (
          SELECT * FROM unnest($1 :: bytea[], $2 :: int8[], $3 :: uuid[], $4 :: jsonb[], $5 :: uuid?[], $6 :: timestamptz[])
        ), written AS (
          INSERT INTO epoch_notifications (group_id, epoch, notification_id, payload, origin, created_at)
          SELECT * FROM incoming ORDER BY group_id, epoch, notification_id, created_at
          ON CONFLICT (group_id, epoch, notification_id, created_at) DO NOTHING
        )
        INSERT INTO last_epoch_notifications (group_id, epoch, notification_id)
        SELECT DISTINCT ON (group_id, epoch) group_id, epoch, notification_id
        FROM incoming ORDER BY group_id, epoch, notification_id DESC
        ON CONFLICT (group_id, epoch) DO UPDATE
          SET notification_id = GREATEST(last_epoch_notifications.notification_id, EXCLUDED.notification_id)|],
      selectPage =
        dimap
          (\((gid, ep), cursor, pageSize) -> (unGroupId gid, fromIntegral (epochNumber ep), toUUID <$> cursor, pageSize))
          (V.map (\(nid, payload, origin) -> FanInNotification (Id nid) payload (Id <$> origin)))
          [vectorStatement|
        SELECT notification_id :: uuid, payload :: jsonb, origin :: uuid?
        FROM epoch_notifications
        WHERE group_id = ($1 :: bytea) AND epoch = ($2 :: int8)
          AND (($3 :: uuid?) IS NULL OR notification_id > ($3 :: uuid?))
        ORDER BY notification_id
        LIMIT ($4 :: int4)
      |],
      selectLast =
        dimap
          (\(gid, ep) -> (unGroupId gid, fromIntegral (epochNumber ep)))
          (fmap Id)
          [maybeStatement|
        SELECT notification_id :: uuid FROM last_epoch_notifications WHERE group_id = ($1 :: bytea) AND epoch = ($2 :: int8)
      |],
      notificationExists =
        lmap
          (\((gid, ep), nid) -> (unGroupId gid, fromIntegral (epochNumber ep), toUUID nid))
          [singletonStatement|
        SELECT EXISTS (SELECT 1 FROM epoch_notifications
          WHERE group_id = ($1 :: bytea) AND epoch = ($2 :: int8) AND notification_id = ($3 :: uuid)) :: bool
      |]
    }

-- | The data-modifying CTE and cursor upsert form one atomic statement. Cursor
-- updates use GREATEST so retries and older events cannot move the head back.
localConnectionStatements :: NotificationStatements UserId
localConnectionStatements =
  NotificationStatements
    { insertNotifications =
        lmap
          ( \rows ->
              (V.map (\(key, _, _) -> toUUID key) rows, V.map (\(_, n, _) -> toUUID n.fanInNotificationId) rows, V.map (\(_, n, _) -> n.fanInNotificationPayload) rows, V.map (\(_, n, _) -> toUUID <$> n.fanInNotificationOrigin) rows, V.map (\(_, _, createdAt) -> createdAt) rows)
          )
          [resultlessStatement|WITH incoming (user_id, notification_id, payload, origin, created_at) AS (
          SELECT * FROM unnest($1 :: uuid[], $2 :: uuid[], $3 :: jsonb[], $4 :: uuid?[], $5 :: timestamptz[])
        ), written AS (
          INSERT INTO local_connection_notifications (user_id, notification_id, payload, origin, created_at)
          SELECT * FROM incoming ORDER BY user_id, notification_id, created_at
          ON CONFLICT (user_id, notification_id, created_at) DO NOTHING
        )
        INSERT INTO last_local_connection_notifications (user_id, notification_id)
        SELECT DISTINCT ON (user_id) user_id, notification_id
        FROM incoming ORDER BY user_id, notification_id DESC
        ON CONFLICT (user_id) DO UPDATE
          SET notification_id = GREATEST(last_local_connection_notifications.notification_id, EXCLUDED.notification_id)|],
      selectPage =
        dimap
          (\(key, cursor, pageSize) -> (toUUID key, toUUID <$> cursor, pageSize))
          (V.map (\(nid, payload, origin) -> FanInNotification (Id nid) payload (Id <$> origin)))
          [vectorStatement|
        SELECT notification_id :: uuid, payload :: jsonb, origin :: uuid?
        FROM local_connection_notifications
        WHERE user_id = ($1 :: uuid)
          AND (($2 :: uuid?) IS NULL OR notification_id > ($2 :: uuid?))
        ORDER BY notification_id
        LIMIT ($3 :: int4)
      |],
      selectLast =
        dimap
          (\key -> toUUID key)
          (fmap Id)
          [maybeStatement|
        SELECT notification_id :: uuid FROM last_local_connection_notifications WHERE user_id = ($1 :: uuid)
      |],
      notificationExists =
        lmap
          (Data.Bifunctor.bimap toUUID toUUID)
          [singletonStatement|
        SELECT EXISTS (SELECT 1 FROM local_connection_notifications
          WHERE user_id = ($1 :: uuid) AND notification_id = ($2 :: uuid)) :: bool
      |]
    }

-- | The data-modifying CTE and cursor upsert form one atomic statement. Cursor
-- updates use GREATEST so retries and older events cannot move the head back.
remoteConnectionStatements :: NotificationStatements (Domain, UserId)
remoteConnectionStatements =
  NotificationStatements
    { insertNotifications =
        lmap
          ( \rows ->
              (V.map (\((domain, _), _, _) -> domainText domain) rows, V.map (\((_, uid), _, _) -> toUUID uid) rows, V.map (\(_, n, _) -> toUUID n.fanInNotificationId) rows, V.map (\(_, n, _) -> n.fanInNotificationPayload) rows, V.map (\(_, n, _) -> toUUID <$> n.fanInNotificationOrigin) rows, V.map (\(_, _, createdAt) -> createdAt) rows)
          )
          [resultlessStatement|WITH incoming (user_domain, user_id, notification_id, payload, origin, created_at) AS (
          SELECT * FROM unnest($1 :: text[], $2 :: uuid[], $3 :: uuid[], $4 :: jsonb[], $5 :: uuid?[], $6 :: timestamptz[])
        ), written AS (
          INSERT INTO remote_connection_notifications (user_domain, user_id, notification_id, payload, origin, created_at)
          SELECT * FROM incoming ORDER BY user_domain, user_id, notification_id, created_at
          ON CONFLICT (user_domain, user_id, notification_id, created_at) DO NOTHING
        )
        INSERT INTO last_remote_connection_notifications (user_domain, user_id, notification_id)
        SELECT DISTINCT ON (user_domain, user_id) user_domain, user_id, notification_id
        FROM incoming ORDER BY user_domain, user_id, notification_id DESC
        ON CONFLICT (user_domain, user_id) DO UPDATE
          SET notification_id = GREATEST(last_remote_connection_notifications.notification_id, EXCLUDED.notification_id)|],
      selectPage =
        dimap
          (\((domain, uid), cursor, pageSize) -> (domainText domain, toUUID uid, toUUID <$> cursor, pageSize))
          (V.map (\(nid, payload, origin) -> FanInNotification (Id nid) payload (Id <$> origin)))
          [vectorStatement|
        SELECT notification_id :: uuid, payload :: jsonb, origin :: uuid?
        FROM remote_connection_notifications
        WHERE user_domain = ($1 :: text) AND user_id = ($2 :: uuid)
          AND (($3 :: uuid?) IS NULL OR notification_id > ($3 :: uuid?))
        ORDER BY notification_id
        LIMIT ($4 :: int4)
      |],
      selectLast =
        dimap
          (Data.Bifunctor.bimap domainText toUUID)
          (fmap Id)
          [maybeStatement|
        SELECT notification_id :: uuid FROM last_remote_connection_notifications WHERE user_domain = ($1 :: text) AND user_id = ($2 :: uuid)
      |],
      notificationExists =
        lmap
          (\((domain, uid), nid) -> (domainText domain, toUUID uid, toUUID nid))
          [singletonStatement|
        SELECT EXISTS (SELECT 1 FROM remote_connection_notifications
          WHERE user_domain = ($1 :: text) AND user_id = ($2 :: uuid) AND notification_id = ($3 :: uuid)) :: bool
      |]
    }
