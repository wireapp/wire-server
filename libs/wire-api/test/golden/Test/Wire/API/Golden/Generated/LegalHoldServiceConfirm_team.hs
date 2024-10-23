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

module Test.Wire.API.Golden.Generated.LegalHoldServiceConfirm_team where

import Data.Domain (Domain (Domain))
import Data.Id
import Data.Qualified (Qualified (Qualified))
import Data.UUID qualified as UUID (fromString)
import Imports (fromJust)
import Wire.API.Team.LegalHold.External (LegalHoldServiceConfirm (..), LegalHoldServiceConfirmV0 (..))

testObject_LegalHoldServiceConfirmV0_team_1 :: LegalHoldServiceConfirmV0
testObject_LegalHoldServiceConfirmV0_team_1 =
  LegalHoldServiceConfirmV0
    { lhcClientId = ClientId 0x1d,
      lhcUserId = Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000100000000")),
      lhcTeamId = Id (fromJust (UUID.fromString "00000007-0000-0000-0000-000600000005")),
      lhcRefreshToken = "i>\ACKO"
    }

testObject_LegalHoldServiceConfirmV0_team_2 :: LegalHoldServiceConfirmV0
testObject_LegalHoldServiceConfirmV0_team_2 =
  LegalHoldServiceConfirmV0
    { lhcClientId = ClientId 0x15,
      lhcUserId = Id (fromJust (UUID.fromString "00000002-0000-0008-0000-000200000007")),
      lhcTeamId = Id (fromJust (UUID.fromString "00000007-0000-0004-0000-000600000002")),
      lhcRefreshToken = "\\i"
    }

testObject_LegalHoldServiceConfirm_team_1 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_team_1 =
  LegalHoldServiceConfirm
    { clientId = ClientId 4,
      userId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000600000005"))) (Domain "example.com"),
      teamId = Id (fromJust (UUID.fromString "00000003-0000-0005-0000-000100000001")),
      refreshToken = ")"
    }

testObject_LegalHoldServiceConfirm_team_2 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_team_2 =
  LegalHoldServiceConfirm
    { clientId = ClientId 0x1b,
      userId = Qualified (Id (fromJust (UUID.fromString "00000008-0000-0002-0000-000300000001"))) (Domain "example.com"),
      teamId = Id (fromJust (UUID.fromString "00000004-0000-0008-0000-000300000004")),
      refreshToken = "W"
    }
