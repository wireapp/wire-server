-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Wire.API.Golden.Manual.FederationStatus where

import Data.Domain (Domain (..))
import Data.Qualified
import Data.Set qualified as Set
import Imports
import Wire.API.FederationStatus

testObject_FederationStatus_1 :: FederationStatus
testObject_FederationStatus_1 = FullyConnected

testObject_FederationStatus_2 :: FederationStatus
testObject_FederationStatus_2 = NotConnectedDomains (Domain "d.example.com") (Domain "e.example.com")

testObject_RemoteDomains_1 :: RemoteDomains
testObject_RemoteDomains_1 =
  RemoteDomains . Set.fromList $
    flip toRemoteUnsafe () <$> [Domain "a.example.com", Domain "b.example.com"]

testObject_RemoteDomains_2 :: RemoteDomains
testObject_RemoteDomains_2 = RemoteDomains mempty
