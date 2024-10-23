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

module Test.Wire.API.Golden.Generated.LegalHoldServiceRemove_team where

import Data.Domain (Domain (Domain))
import Data.Id (Id (Id))
import Data.Qualified (Qualified (Qualified))
import Data.UUID qualified as UUID (fromString)
import Imports (fromJust)
import Wire.API.Team.LegalHold.External

testObject_LegalHoldServiceRemoveV0_team_1 :: LegalHoldServiceRemoveV0
testObject_LegalHoldServiceRemoveV0_team_1 =
  LegalHoldServiceRemoveV0
    { lhrUserId = Id (fromJust (UUID.fromString "00000034-0000-0016-0000-003c00000024")),
      lhrTeamId = Id (fromJust (UUID.fromString "0000001e-0000-000f-0000-007100000079"))
    }

testObject_LegalHoldServiceRemoveV0_team_2 :: LegalHoldServiceRemoveV0
testObject_LegalHoldServiceRemoveV0_team_2 =
  LegalHoldServiceRemoveV0
    { lhrUserId = Id (fromJust (UUID.fromString "0000004f-0000-0076-0000-001f00000019")),
      lhrTeamId = Id (fromJust (UUID.fromString "00000050-0000-0059-0000-004d00000067"))
    }

testObject_LegalHoldServiceRemove_team_1 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_1 =
  LegalHoldServiceRemove
    { userId = Qualified (Id (fromJust (UUID.fromString "00000034-0000-0016-0000-003c00000024"))) (Domain "example.com"),
      teamId = Id (fromJust (UUID.fromString "0000001e-0000-000f-0000-007100000079"))
    }

testObject_LegalHoldServiceRemove_team_2 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_2 =
  LegalHoldServiceRemove
    { userId = Qualified (Id (fromJust (UUID.fromString "0000004f-0000-0076-0000-001f00000019"))) (Domain "example.com"),
      teamId = Id (fromJust (UUID.fromString "00000050-0000-0059-0000-004d00000067"))
    }
