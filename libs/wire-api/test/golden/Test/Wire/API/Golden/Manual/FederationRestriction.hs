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

module Test.Wire.API.Golden.Manual.FederationRestriction where

import Data.Id
import Data.UUID qualified as UUID
import Imports
import Wire.API.Routes.FederationDomainConfig

testObject_FederationRestriction_1 :: FederationRestriction
testObject_FederationRestriction_1 = FederationRestrictionAllowAll

testObject_FederationRestriction_2 :: FederationRestriction
testObject_FederationRestriction_2 = FederationRestrictionByTeam []

testObject_FederationRestriction_3 :: FederationRestriction
testObject_FederationRestriction_3 =
  FederationRestrictionByTeam
    [ Id (fromJust (UUID.fromString "0000304a-0000-0d5e-0000-3fac00003993")),
      Id (fromJust (UUID.fromString "00003c90-0000-2207-0000-5249000018b1"))
    ]
