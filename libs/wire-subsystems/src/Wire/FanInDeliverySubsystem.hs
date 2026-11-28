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

module Wire.FanInDeliverySubsystem where

import Data.Id
import Data.Range
import Data.UUID (UUID)
import Imports
import Polysemy
import Wire.API.Notification.FanIn

data FanInDeliverySubsystem m a where
  -- | Bind authenticated identity to a session and register Presence. An absent
  -- client id selects all clients as specified by the RFC. The interpreter owns
  -- the tag->stream/cursor map and limits outstanding deliveries per session.
  OpenFanInSession :: UserId -> Maybe ClientId -> FanInDeliverySubsystem m FanInSessionId
  -- | Read a bounded batch using session send cursors (not just durable acks).
  -- Recheck stream authorization. Emit missed/synchronization markers according
  -- to the sync phase; a wake-up requests another read through this operation.
  ReadFanInSession :: FanInSessionId -> Range 1 10000 Int32 -> FanInDeliverySubsystem m [FanInServerMessage]
  -- | Validate delivery tags; persist only contiguous acknowledged prefixes per
  -- stream. Full-sync acknowledgements reinitialize the session's cursors.
  HandleFanInClientMessage :: FanInSessionId -> FanInClientMessage -> FanInDeliverySubsystem m ()
  -- | Wake persistent sessions or forward transient payloads as appropriate.
  NotifyFanInSessions :: FanInCannonRequest -> FanInDeliverySubsystem m ()
  CloseFanInSession :: FanInSessionId -> FanInDeliverySubsystem m ()

-- | Session identity independent of legacy RabbitMQ channel delivery tags.
newtype FanInSessionId = FanInSessionId UUID
  deriving stock (Eq, Ord, Show)

makeSem ''FanInDeliverySubsystem
