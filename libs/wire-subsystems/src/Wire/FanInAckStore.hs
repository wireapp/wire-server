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
    getFanInAck,
    upsertFanInAck,
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
  deriving stock (Eq, Show)

data FanInAckStore m a where
  GetFanInAck :: FanInAckKey -> FanInAckStore m (Maybe NotificationId)
  -- | Persist the caller's latest successfully processed notification.
  UpsertFanInAck :: FanInAckKey -> NotificationId -> FanInAckStore m ()
  -- | Remove ack state when its owning client or relationship is deleted.
  DeleteFanInAck :: FanInAckKey -> FanInAckStore m ()

makeSem ''FanInAckStore
