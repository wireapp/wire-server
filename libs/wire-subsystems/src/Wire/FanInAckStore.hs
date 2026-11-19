{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

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

-- | Storage operations for fan-in acknowledgement cursors. This effect is
-- separate from both the legacy notification effects and the notification
-- stream store.
module Wire.FanInAckStore
  ( FanInAckKey (..),
    FanInAckStore (..),
    FanInAckStoreError (..),
    getFanInAck,
    createFanInEmptyCursor,
    hasFanInEmptyCursor,
    initializeFanInAck,
    upsertFanInAck,
    advanceFanInAck,
    deleteFanInAck,
  )
where

import Data.Domain (Domain)
import Data.Id
import Imports
import Polysemy
import Wire.API.MLS.Epoch (Epoch)
import Wire.API.MLS.Group (GroupId)
import Wire.API.Notification (NotificationId)

-- | Identifies one acknowledgement row across the fan-in ack tables.
data FanInAckKey
  = FanInUserAck UserId ClientId
  | FanInClientAck UserId ClientId
  | FanInTeamAck UserId ClientId TeamId
  | FanInEpochAck UserId ClientId GroupId Epoch
  | FanInLocalConnectionAck UserId UserId
  | FanInRemoteConnectionAck UserId Domain UserId
  deriving stock (Eq, Ord, Show)

data FanInAckStore m a where
  GetFanInAck :: FanInAckKey -> FanInAckStore m (Maybe NotificationId)
  -- | Retained boundary for a stream that has never had a notification. It is
  -- not an event and must never appear on the WebSocket. Expiring it permits
  -- the same conservative gap detection as expiring a real notification.
  CreateFanInEmptyCursor :: FanInAckStore m NotificationId
  HasFanInEmptyCursor :: NotificationId -> FanInAckStore m Bool
  -- | Initialize only if absent, returning the winning cursor. Concurrent opens
  -- must not overwrite acknowledgements or explicit resets.
  InitializeFanInAck :: FanInAckKey -> NotificationId -> FanInAckStore m NotificationId
  -- | Set a cursor exactly, including rewinding it for an explicit reset.
  -- Normal WebSocket acknowledgements must use AdvanceFanInAck.
  -- Write operations require UUIDv7 cursors; resetting to an expired cursor
  -- is allowed, with retention-gap handling left to the delivery subsystem.
  UpsertFanInAck :: FanInAckKey -> NotificationId -> FanInAckStore m ()
  -- | Atomically advance a cursor without regressing it under concurrent acks.
  AdvanceFanInAck :: FanInAckKey -> NotificationId -> FanInAckStore m ()
  -- | Remove ack state when its owning client or relationship is deleted.
  DeleteFanInAck :: FanInAckKey -> FanInAckStore m ()

makeSem ''FanInAckStore

-- | Invalid cursors are rejected before database access.
data FanInAckStoreError = InvalidFanInAckNotificationId NotificationId
  deriving stock (Eq, Show)
