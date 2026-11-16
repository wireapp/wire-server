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

-- | Storage operations for current and superseded fan-in epoch membership.
-- This effect is separate from the legacy notification stores.
module Wire.FanInEpochMembershipStore
  ( FanInEpochMember (..),
    FanInEpochRef (..),
    FanInEpochMembershipStore (..),
    getFanInEpochMembers,
    getFanInEpochsForClient,
    replaceFanInCurrentEpochMembers,
  )
where

import Data.Id
import Data.Time.Clock (UTCTime)
import Imports
import Polysemy
import Wire.API.MLS.Epoch (Epoch)
import Wire.API.MLS.Group (GroupId)

data FanInEpochMember = FanInEpochMember
  { fanInEpochMemberUserId :: UserId,
    fanInEpochMemberClientId :: ClientId
  }
  deriving stock (Eq, Show)

data FanInEpochRef = FanInEpochRef
  { fanInEpochRefGroupId :: GroupId,
    fanInEpochRefEpoch :: Epoch
  }
  deriving stock (Eq, Show)

data FanInEpochMembershipStore m a where
  GetFanInEpochMembers :: GroupId -> Epoch -> FanInEpochMembershipStore m [FanInEpochMember]
  -- | Return the current and retained historical group epochs containing this
  -- client, which lets a reader find the relevant epoch notification streams.
  GetFanInEpochsForClient :: UserId -> ClientId -> FanInEpochMembershipStore m [FanInEpochRef]
  -- | Atomically archive the previous current members at the supplied time and
  -- replace them with the members for the new current epoch.
  ReplaceFanInCurrentEpochMembers :: GroupId -> Epoch -> UTCTime -> [FanInEpochMember] -> FanInEpochMembershipStore m ()

makeSem ''FanInEpochMembershipStore
