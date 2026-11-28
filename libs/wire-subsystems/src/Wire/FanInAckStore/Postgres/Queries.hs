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

-- | Fixed parameterized statements for each acknowledgement table. Exact
-- resets deliberately use EXCLUDED; normal acknowledgements use GREATEST so
-- concurrent or reordered writes cannot move the cursor backwards.
module Wire.FanInAckStore.Postgres.Queries where

import Data.Domain (Domain, domainText)
import Data.Id
import Data.Profunctor (dimap, lmap)
import Hasql.Statement (Statement)
import Hasql.TH
import Imports
import Wire.API.MLS.Epoch (Epoch (..))
import Wire.API.MLS.Group (GroupId (..))
import Wire.API.Notification (NotificationId)

data AckStatements key = AckStatements
  { selectAck :: Statement key (Maybe NotificationId),
    upsertAck :: Statement (key, NotificationId) (),
    advanceAck :: Statement (key, NotificationId) (),
    deleteAck :: Statement key ()
  }

userAckStatements :: AckStatements (UserId, ClientId)
userAckStatements =
  AckStatements
    { selectAck =
        dimap
          (\(uid, cid) -> (toUUID uid, fromIntegral (clientToWord64 cid)))
          (fmap Id)
          [maybeStatement|SELECT last_notification_id :: uuid FROM user_notification_acks
        WHERE user_id = ($1 :: uuid) AND client_id = ($2 :: int8)|],
      upsertAck =
        lmap
          (\((uid, cid), nid) -> (toUUID uid, fromIntegral (clientToWord64 cid), toUUID nid))
          [resultlessStatement|INSERT INTO user_notification_acks (user_id, client_id, last_notification_id)
        VALUES ($1 :: uuid, $2 :: int8, $3 :: uuid)
        ON CONFLICT (user_id, client_id) DO UPDATE
          SET last_notification_id = EXCLUDED.last_notification_id|],
      advanceAck =
        lmap
          (\((uid, cid), nid) -> (toUUID uid, fromIntegral (clientToWord64 cid), toUUID nid))
          [resultlessStatement|INSERT INTO user_notification_acks (user_id, client_id, last_notification_id)
        VALUES ($1 :: uuid, $2 :: int8, $3 :: uuid)
        ON CONFLICT (user_id, client_id) DO UPDATE
          SET last_notification_id = GREATEST(user_notification_acks.last_notification_id, EXCLUDED.last_notification_id)|],
      deleteAck =
        lmap
          (\(uid, cid) -> (toUUID uid, fromIntegral (clientToWord64 cid)))
          [resultlessStatement|DELETE FROM user_notification_acks
        WHERE user_id = ($1 :: uuid) AND client_id = ($2 :: int8)|]
    }

clientAckStatements :: AckStatements (UserId, ClientId)
clientAckStatements =
  AckStatements
    { selectAck =
        dimap
          (\(uid, cid) -> (toUUID uid, fromIntegral (clientToWord64 cid)))
          (fmap Id)
          [maybeStatement|SELECT last_notification_id :: uuid FROM client_notification_acks
        WHERE user_id = ($1 :: uuid) AND client_id = ($2 :: int8)|],
      upsertAck =
        lmap
          (\((uid, cid), nid) -> (toUUID uid, fromIntegral (clientToWord64 cid), toUUID nid))
          [resultlessStatement|INSERT INTO client_notification_acks (user_id, client_id, last_notification_id)
        VALUES ($1 :: uuid, $2 :: int8, $3 :: uuid)
        ON CONFLICT (user_id, client_id) DO UPDATE
          SET last_notification_id = EXCLUDED.last_notification_id|],
      advanceAck =
        lmap
          (\((uid, cid), nid) -> (toUUID uid, fromIntegral (clientToWord64 cid), toUUID nid))
          [resultlessStatement|INSERT INTO client_notification_acks (user_id, client_id, last_notification_id)
        VALUES ($1 :: uuid, $2 :: int8, $3 :: uuid)
        ON CONFLICT (user_id, client_id) DO UPDATE
          SET last_notification_id = GREATEST(client_notification_acks.last_notification_id, EXCLUDED.last_notification_id)|],
      deleteAck =
        lmap
          (\(uid, cid) -> (toUUID uid, fromIntegral (clientToWord64 cid)))
          [resultlessStatement|DELETE FROM client_notification_acks
        WHERE user_id = ($1 :: uuid) AND client_id = ($2 :: int8)|]
    }

teamAckStatements :: AckStatements (UserId, ClientId, TeamId)
teamAckStatements =
  AckStatements
    { selectAck =
        dimap
          (\(uid, cid, tid) -> (toUUID uid, fromIntegral (clientToWord64 cid), toUUID tid))
          (fmap Id)
          [maybeStatement|SELECT last_notification_id :: uuid FROM team_notification_acks
        WHERE user_id = ($1 :: uuid) AND client_id = ($2 :: int8) AND team_id = ($3 :: uuid)|],
      upsertAck =
        lmap
          (\((uid, cid, tid), nid) -> (toUUID uid, fromIntegral (clientToWord64 cid), toUUID tid, toUUID nid))
          [resultlessStatement|INSERT INTO team_notification_acks (user_id, client_id, team_id, last_notification_id)
        VALUES ($1 :: uuid, $2 :: int8, $3 :: uuid, $4 :: uuid)
        ON CONFLICT (user_id, client_id, team_id) DO UPDATE
          SET last_notification_id = EXCLUDED.last_notification_id|],
      advanceAck =
        lmap
          (\((uid, cid, tid), nid) -> (toUUID uid, fromIntegral (clientToWord64 cid), toUUID tid, toUUID nid))
          [resultlessStatement|INSERT INTO team_notification_acks (user_id, client_id, team_id, last_notification_id)
        VALUES ($1 :: uuid, $2 :: int8, $3 :: uuid, $4 :: uuid)
        ON CONFLICT (user_id, client_id, team_id) DO UPDATE
          SET last_notification_id = GREATEST(team_notification_acks.last_notification_id, EXCLUDED.last_notification_id)|],
      deleteAck =
        lmap
          (\(uid, cid, tid) -> (toUUID uid, fromIntegral (clientToWord64 cid), toUUID tid))
          [resultlessStatement|DELETE FROM team_notification_acks
        WHERE user_id = ($1 :: uuid) AND client_id = ($2 :: int8) AND team_id = ($3 :: uuid)|]
    }

epochAckStatements :: AckStatements (UserId, ClientId, GroupId, Epoch)
epochAckStatements =
  AckStatements
    { selectAck =
        dimap
          (\(uid, cid, GroupId gid, Epoch epoch) -> (toUUID uid, fromIntegral (clientToWord64 cid), gid, fromIntegral epoch))
          (fmap Id)
          [maybeStatement|SELECT last_notification_id :: uuid FROM epoch_notification_acks
        WHERE user_id = ($1 :: uuid) AND client_id = ($2 :: int8) AND group_id = ($3 :: bytea) AND epoch = ($4 :: int8)|],
      upsertAck =
        lmap
          (\((uid, cid, GroupId gid, Epoch epoch), nid) -> (toUUID uid, fromIntegral (clientToWord64 cid), gid, fromIntegral epoch, toUUID nid))
          [resultlessStatement|INSERT INTO epoch_notification_acks (user_id, client_id, group_id, epoch, last_notification_id)
        VALUES ($1 :: uuid, $2 :: int8, $3 :: bytea, $4 :: int8, $5 :: uuid)
        ON CONFLICT (user_id, client_id, group_id, epoch) DO UPDATE
          SET last_notification_id = EXCLUDED.last_notification_id|],
      advanceAck =
        lmap
          (\((uid, cid, GroupId gid, Epoch epoch), nid) -> (toUUID uid, fromIntegral (clientToWord64 cid), gid, fromIntegral epoch, toUUID nid))
          [resultlessStatement|INSERT INTO epoch_notification_acks (user_id, client_id, group_id, epoch, last_notification_id)
        VALUES ($1 :: uuid, $2 :: int8, $3 :: bytea, $4 :: int8, $5 :: uuid)
        ON CONFLICT (user_id, client_id, group_id, epoch) DO UPDATE
          SET last_notification_id = GREATEST(epoch_notification_acks.last_notification_id, EXCLUDED.last_notification_id)|],
      deleteAck =
        lmap
          (\(uid, cid, GroupId gid, Epoch epoch) -> (toUUID uid, fromIntegral (clientToWord64 cid), gid, fromIntegral epoch))
          [resultlessStatement|DELETE FROM epoch_notification_acks
        WHERE user_id = ($1 :: uuid) AND client_id = ($2 :: int8) AND group_id = ($3 :: bytea) AND epoch = ($4 :: int8)|]
    }

localConnectionAckStatements :: AckStatements (UserId, UserId)
localConnectionAckStatements =
  AckStatements
    { selectAck =
        dimap
          (bimap toUUID toUUID)
          (fmap Id)
          [maybeStatement|SELECT last_notification_id :: uuid FROM local_connection_acks
        WHERE user_id = ($1 :: uuid) AND connected_user_id = ($2 :: uuid)|],
      upsertAck =
        lmap
          (\((uid, connected), nid) -> (toUUID uid, toUUID connected, toUUID nid))
          [resultlessStatement|INSERT INTO local_connection_acks (user_id, connected_user_id, last_notification_id)
        VALUES ($1 :: uuid, $2 :: uuid, $3 :: uuid)
        ON CONFLICT (user_id, connected_user_id) DO UPDATE
          SET last_notification_id = EXCLUDED.last_notification_id|],
      advanceAck =
        lmap
          (\((uid, connected), nid) -> (toUUID uid, toUUID connected, toUUID nid))
          [resultlessStatement|INSERT INTO local_connection_acks (user_id, connected_user_id, last_notification_id)
        VALUES ($1 :: uuid, $2 :: uuid, $3 :: uuid)
        ON CONFLICT (user_id, connected_user_id) DO UPDATE
          SET last_notification_id = GREATEST(local_connection_acks.last_notification_id, EXCLUDED.last_notification_id)|],
      deleteAck =
        lmap
          (bimap toUUID toUUID)
          [resultlessStatement|DELETE FROM local_connection_acks
        WHERE user_id = ($1 :: uuid) AND connected_user_id = ($2 :: uuid)|]
    }

remoteConnectionAckStatements :: AckStatements (UserId, Domain, UserId)
remoteConnectionAckStatements =
  AckStatements
    { selectAck =
        dimap
          (\(uid, domain, connected) -> (toUUID uid, domainText domain, toUUID connected))
          (fmap Id)
          [maybeStatement|SELECT last_notification_id :: uuid FROM remote_connection_acks
        WHERE user_id = ($1 :: uuid) AND connected_user_domain = ($2 :: text) AND connected_user_id = ($3 :: uuid)|],
      upsertAck =
        lmap
          (\((uid, domain, connected), nid) -> (toUUID uid, domainText domain, toUUID connected, toUUID nid))
          [resultlessStatement|INSERT INTO remote_connection_acks (user_id, connected_user_domain, connected_user_id, last_notification_id)
        VALUES ($1 :: uuid, $2 :: text, $3 :: uuid, $4 :: uuid)
        ON CONFLICT (user_id, connected_user_domain, connected_user_id) DO UPDATE
          SET last_notification_id = EXCLUDED.last_notification_id|],
      advanceAck =
        lmap
          (\((uid, domain, connected), nid) -> (toUUID uid, domainText domain, toUUID connected, toUUID nid))
          [resultlessStatement|INSERT INTO remote_connection_acks (user_id, connected_user_domain, connected_user_id, last_notification_id)
        VALUES ($1 :: uuid, $2 :: text, $3 :: uuid, $4 :: uuid)
        ON CONFLICT (user_id, connected_user_domain, connected_user_id) DO UPDATE
          SET last_notification_id = GREATEST(remote_connection_acks.last_notification_id, EXCLUDED.last_notification_id)|],
      deleteAck =
        lmap
          (\(uid, domain, connected) -> (toUUID uid, domainText domain, toUUID connected))
          [resultlessStatement|DELETE FROM remote_connection_acks
        WHERE user_id = ($1 :: uuid) AND connected_user_domain = ($2 :: text) AND connected_user_id = ($3 :: uuid)|]
    }
