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

module Test.Wire.API.Golden.Generated.RequestNewLegalHoldClient_team where

import Data.Domain
import Data.Id (Id (Id))
import Data.Qualified
import Data.UUID qualified as UUID (fromString)
import Imports (fromJust)
import Wire.API.Team.LegalHold.External

testObject_RequestNewLegalHoldClientV0_team_1 :: RequestNewLegalHoldClientV0
testObject_RequestNewLegalHoldClientV0_team_1 =
  RequestNewLegalHoldClientV0
    (Id (fromJust (UUID.fromString "0000003d-0000-0049-0000-003b00000055")))
    (Id (fromJust (UUID.fromString "0000002e-0000-006e-0000-004a0000001b")))

testObject_RequestNewLegalHoldClientV0_team_2 :: RequestNewLegalHoldClientV0
testObject_RequestNewLegalHoldClientV0_team_2 =
  RequestNewLegalHoldClientV0
    (Id (fromJust (UUID.fromString "0000001c-0000-0064-0000-003a0000000b")))
    (Id (fromJust (UUID.fromString "00000049-0000-0059-0000-004e0000001f")))

testObject_RequestNewLegalHoldClient_team_1 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_1 =
  RequestNewLegalHoldClient
    (Qualified ((Id (fromJust (UUID.fromString "0000007d-0000-0054-0000-000900000018")))) (Domain "example.com"))
    (Id (fromJust (UUID.fromString "0000005d-0000-001f-0000-006300000019")))

testObject_RequestNewLegalHoldClient_team_2 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_2 =
  RequestNewLegalHoldClient
    (Qualified ((Id (fromJust (UUID.fromString "00000025-0000-0077-0000-002d00000045")))) (Domain "example.com"))
    (Id (fromJust (UUID.fromString "0000001a-0000-002c-0000-004e0000005c")))
