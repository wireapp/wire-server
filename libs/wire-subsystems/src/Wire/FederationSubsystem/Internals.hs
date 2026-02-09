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

module Wire.FederationSubsystem.Internals
  ( firstConflictOrFullyConnected,
  )
where

import Control.Error (headMay)
import Data.Domain (Domain)
import Data.Qualified
import Data.Set qualified as Set
import Imports
import Wire.API.Federation.API.Brig (NonConnectedBackends (..))
import Wire.API.FederationStatus (FederationStatus (..))

-- | "conflict" here means two remote domains that we are connected to
-- but are not connected to each other.
firstConflictOrFullyConnected :: [Remote NonConnectedBackends] -> FederationStatus
firstConflictOrFullyConnected =
  maybe
    FullyConnected
    (uncurry NotConnectedDomains)
    . headMay
    . mapMaybe toMaybeConflict
  where
    toMaybeConflict :: Remote NonConnectedBackends -> Maybe (Domain, Domain)
    toMaybeConflict r =
      headMay (Set.toList (nonConnectedBackends (tUnqualified r))) <&> (tDomain r,)
