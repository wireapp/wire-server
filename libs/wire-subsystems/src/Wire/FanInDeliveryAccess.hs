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

-- | Membership and socket access needed by fan-in delivery. Implementations
-- must return current authorization data, not a cache of the original session.
module Wire.FanInDeliveryAccess where

import Data.Id
import Data.Qualified (Qualified)
import Polysemy
import Wire.API.Notification.FanIn

data FanInDeliveryAccess m a where
  GetFanInClientIds :: UserId -> FanInDeliveryAccess m [ClientId]
  GetFanInTeamIds :: UserId -> FanInDeliveryAccess m [TeamId]
  -- | Return only streams authorized in both directions. An unverified remote
  -- connection must not be returned just because the reader has Accepted it.
  GetFanInAcceptedConnections :: UserId -> FanInDeliveryAccess m [Qualified UserId]
  NotifyFanInSockets :: FanInCannonRequest -> FanInDeliveryAccess m ()

makeSem ''FanInDeliveryAccess
