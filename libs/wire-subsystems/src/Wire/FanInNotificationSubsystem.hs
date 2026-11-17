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

module Wire.FanInNotificationSubsystem where

import Data.Id
import Data.List.NonEmpty (NonEmpty)
import Data.Qualified (Qualified)
import Polysemy
import Wire.API.Notification.FanIn

data FanInNotificationSubsystem m a where
  -- | Persist idempotently and arrange background delivery; transient pushes
  -- bypass storage. Persisted notifications use UUIDv7 ids.
  PublishFanInNotifications :: [FanInPush] -> FanInNotificationSubsystem m ()
  -- | Reject stale/conflicting epochs; archive superseded membership atomically.
  UpdateFanInMembership :: FanInMembershipUpdate -> FanInNotificationSubsystem m ()
  -- | Called by the background worker: resolve recipients, send SNS pushes and
  -- bucket Presence results by Cannon. Wake-ups trigger DB reads; transient
  -- payloads are forwarded directly.
  DeliverFanInNotifications :: NonEmpty FanInPush -> FanInNotificationSubsystem m ()
  -- | Initialize new/reset clients at the current stream heads.
  InitializeFanInClient :: UserId -> ClientId -> FanInNotificationSubsystem m ()
  -- | Initialize heads when a connection becomes accepted or a collaborator
  -- joins a team. Removal is enforced by authorization during reads.
  InitializeFanInConnection :: UserId -> Qualified UserId -> FanInNotificationSubsystem m ()
  InitializeFanInTeamMember :: UserId -> TeamId -> FanInNotificationSubsystem m ()
  -- | Explicit rewind/reset, distinct from normal delivery acknowledgement.
  ResetFanInAcknowledgements :: UserId -> ClientId -> FanInAcknowledgements -> FanInNotificationSubsystem m ()

makeSem ''FanInNotificationSubsystem
