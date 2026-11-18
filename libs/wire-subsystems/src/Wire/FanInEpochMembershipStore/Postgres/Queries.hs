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

module Wire.FanInEpochMembershipStore.Postgres.Queries where

import Data.Id
import Data.Profunctor (dimap, lmap)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Hasql.Statement (Statement)
import Hasql.TH
import Imports
import Wire.API.MLS.Epoch (Epoch (..))
import Wire.API.MLS.Group (GroupId (..))
import Wire.FanInEpochMembershipStore

insertHead :: Statement (GroupId, Epoch) Bool
insertHead =
  lmap
    groupEpoch
    [singletonStatement|
  WITH inserted AS (
    INSERT INTO epoch_heads (group_id, epoch) VALUES ($1 :: bytea, $2 :: int8)
    ON CONFLICT (group_id) DO NOTHING RETURNING group_id
  ) SELECT EXISTS (SELECT 1 FROM inserted) :: bool
|]

-- A separate statement after INSERT is intentional: ReadCommitted must see a
-- concurrent first writer after waiting for its unique-key conflict to resolve.
lockHead :: Statement GroupId Epoch
lockHead =
  dimap
    (\(GroupId gid) -> gid)
    (Epoch . fromIntegral)
    [singletonStatement|
  SELECT epoch :: int8 FROM epoch_heads WHERE group_id = ($1 :: bytea) FOR UPDATE
|]

updateHead :: Statement (GroupId, Epoch) ()
updateHead =
  lmap
    groupEpoch
    [resultlessStatement|
  UPDATE epoch_heads SET epoch = ($2 :: int8) WHERE group_id = ($1 :: bytea)
|]

selectCurrentMembers :: Statement (GroupId, Epoch) (Vector FanInEpochMember)
selectCurrentMembers =
  dimap
    groupEpoch
    (V.map member)
    [vectorStatement|
  SELECT user_id :: uuid, client_id :: int8 FROM epoch_members
  WHERE group_id = ($1 :: bytea) AND epoch = ($2 :: int8)
  ORDER BY user_id, client_id
|]

selectMembers :: Statement (GroupId, Epoch) (Vector FanInEpochMember)
selectMembers =
  dimap
    groupEpoch
    (V.map member)
    [vectorStatement|
  SELECT user_id :: uuid, client_id :: int8 FROM epoch_members
  WHERE group_id = ($1 :: bytea) AND epoch = ($2 :: int8)
  UNION
  SELECT user_id :: uuid, client_id :: int8 FROM epoch_history
  WHERE group_id = ($1 :: bytea) AND epoch = ($2 :: int8)
  ORDER BY user_id, client_id
|]

selectEpochsForClient :: Statement (UserId, ClientId) (Vector FanInEpochRef)
selectEpochsForClient =
  dimap
    (\(uid, cid) -> (toUUID uid, fromIntegral (clientToWord64 cid)))
    (V.map (\(gid, epoch) -> FanInEpochRef (GroupId gid) (Epoch (fromIntegral epoch))))
    [vectorStatement|
    SELECT group_id :: bytea, epoch :: int8 FROM epoch_members
    WHERE user_id = ($1 :: uuid) AND client_id = ($2 :: int8)
    UNION
    SELECT group_id :: bytea, epoch :: int8 FROM epoch_history
    WHERE user_id = ($1 :: uuid) AND client_id = ($2 :: int8)
    ORDER BY group_id, epoch
  |]

archiveMembers :: Statement (GroupId, UTCTime) ()
archiveMembers =
  lmap
    (\(GroupId gid, at) -> (gid, at))
    [resultlessStatement|
  INSERT INTO epoch_history (group_id, epoch, user_id, client_id, superseded_at)
  SELECT group_id, epoch, user_id, client_id, ($2 :: timestamptz)
  FROM epoch_members WHERE group_id = ($1 :: bytea)
|]

deleteMembers :: Statement GroupId ()
deleteMembers =
  lmap
    (\(GroupId gid) -> gid)
    [resultlessStatement|
  DELETE FROM epoch_members WHERE group_id = ($1 :: bytea)
|]

insertMembers :: Statement (GroupId, Epoch, Vector FanInEpochMember) ()
insertMembers =
  lmap
    (\(GroupId gid, Epoch epoch, members) -> (gid, fromIntegral epoch, V.map (toUUID . fanInEpochMemberUserId) members, V.map (fromIntegral . clientToWord64 . fanInEpochMemberClientId) members))
    [resultlessStatement|
    INSERT INTO epoch_members (group_id, epoch, user_id, client_id)
    SELECT ($1 :: bytea), ($2 :: int8), user_id, client_id
    FROM unnest($3 :: uuid[], $4 :: int8[]) AS incoming(user_id, client_id)
  |]

groupEpoch :: (GroupId, Epoch) -> (ByteString, Int64)
groupEpoch (GroupId gid, Epoch epoch) = (gid, fromIntegral epoch)

member :: (UUID, Int64) -> FanInEpochMember
member (uid, cid) = FanInEpochMember (Id uid) (ClientId (fromIntegral cid))
