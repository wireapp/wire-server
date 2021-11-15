{-# LANGUAGE OverloadedLists #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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

import Data.Id (ClientId (ClientId, client), Id (Id))
import qualified Data.UUID as UUID (fromString)
import Imports (fromJust)
import Wire.API.Team.LegalHold.External (LegalHoldServiceConfirm (..))

testObject_LegalHoldServiceConfirm_team_1 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_team_1 =
  LegalHoldServiceConfirm
    { lhcClientId = ClientId {client = "1d"},
      lhcUserId = (Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000100000000"))),
      lhcTeamId = (Id (fromJust (UUID.fromString "00000007-0000-0000-0000-000600000005"))),
      lhcRefreshToken = "i>\ACKO"
    }

testObject_LegalHoldServiceConfirm_team_2 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_team_2 =
  LegalHoldServiceConfirm
    { lhcClientId = ClientId {client = "15"},
      lhcUserId = (Id (fromJust (UUID.fromString "00000002-0000-0008-0000-000200000007"))),
      lhcTeamId = (Id (fromJust (UUID.fromString "00000007-0000-0004-0000-000600000002"))),
      lhcRefreshToken = "\\i"
    }

testObject_LegalHoldServiceConfirm_team_3 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_team_3 =
  LegalHoldServiceConfirm
    { lhcClientId = ClientId {client = "4"},
      lhcUserId = (Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000600000005"))),
      lhcTeamId = (Id (fromJust (UUID.fromString "00000003-0000-0005-0000-000100000001"))),
      lhcRefreshToken = ")"
    }

testObject_LegalHoldServiceConfirm_team_4 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_team_4 =
  LegalHoldServiceConfirm
    { lhcClientId = ClientId {client = "1b"},
      lhcUserId = (Id (fromJust (UUID.fromString "00000008-0000-0002-0000-000300000001"))),
      lhcTeamId = (Id (fromJust (UUID.fromString "00000004-0000-0008-0000-000300000004"))),
      lhcRefreshToken = "W"
    }

testObject_LegalHoldServiceConfirm_team_5 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_team_5 =
  LegalHoldServiceConfirm
    { lhcClientId = ClientId {client = "12"},
      lhcUserId = (Id (fromJust (UUID.fromString "00000000-0000-0005-0000-000300000006"))),
      lhcTeamId = (Id (fromJust (UUID.fromString "00000002-0000-0008-0000-000400000007"))),
      lhcRefreshToken = "\1021908hL\1101997\23856\180103"
    }

testObject_LegalHoldServiceConfirm_team_6 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_team_6 =
  LegalHoldServiceConfirm
    { lhcClientId = ClientId {client = "1"},
      lhcUserId = (Id (fromJust (UUID.fromString "00000005-0000-0002-0000-000300000003"))),
      lhcTeamId = (Id (fromJust (UUID.fromString "00000004-0000-0008-0000-000200000006"))),
      lhcRefreshToken = "\1089885\983521b"
    }

testObject_LegalHoldServiceConfirm_team_7 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_team_7 =
  LegalHoldServiceConfirm
    { lhcClientId = ClientId {client = "1c"},
      lhcUserId = (Id (fromJust (UUID.fromString "00000005-0000-0001-0000-000600000001"))),
      lhcTeamId = (Id (fromJust (UUID.fromString "00000006-0000-0004-0000-000500000003"))),
      lhcRefreshToken = "\1048812[\ETBu\r"
    }

testObject_LegalHoldServiceConfirm_team_8 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_team_8 =
  LegalHoldServiceConfirm
    { lhcClientId = ClientId {client = "1f"},
      lhcUserId = (Id (fromJust (UUID.fromString "00000003-0000-0008-0000-000200000001"))),
      lhcTeamId = (Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000500000004"))),
      lhcRefreshToken = "ZU\990363;\US\ESC"
    }

testObject_LegalHoldServiceConfirm_team_9 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_team_9 =
  LegalHoldServiceConfirm
    { lhcClientId = ClientId {client = "3"},
      lhcUserId = (Id (fromJust (UUID.fromString "00000003-0000-0008-0000-000100000003"))),
      lhcTeamId = (Id (fromJust (UUID.fromString "00000008-0000-0006-0000-000000000006"))),
      lhcRefreshToken = "Y\1088702"
    }

testObject_LegalHoldServiceConfirm_team_10 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_team_10 =
  LegalHoldServiceConfirm
    { lhcClientId = ClientId {client = "20"},
      lhcUserId = (Id (fromJust (UUID.fromString "00000006-0000-0005-0000-000500000006"))),
      lhcTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0005-0000-000700000001"))),
      lhcRefreshToken = ""
    }

testObject_LegalHoldServiceConfirm_team_11 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_team_11 =
  LegalHoldServiceConfirm
    { lhcClientId = ClientId {client = "0"},
      lhcUserId = (Id (fromJust (UUID.fromString "00000006-0000-0002-0000-000700000007"))),
      lhcTeamId = (Id (fromJust (UUID.fromString "00000002-0000-0005-0000-000400000007"))),
      lhcRefreshToken = "\153567@-c\ENQ"
    }

testObject_LegalHoldServiceConfirm_team_12 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_team_12 =
  LegalHoldServiceConfirm
    { lhcClientId = ClientId {client = "0"},
      lhcUserId = (Id (fromJust (UUID.fromString "00000005-0000-0006-0000-000500000004"))),
      lhcTeamId = (Id (fromJust (UUID.fromString "00000007-0000-0008-0000-000600000006"))),
      lhcRefreshToken = ""
    }

testObject_LegalHoldServiceConfirm_team_13 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_team_13 =
  LegalHoldServiceConfirm
    { lhcClientId = ClientId {client = "c"},
      lhcUserId = (Id (fromJust (UUID.fromString "00000002-0000-0005-0000-000600000005"))),
      lhcTeamId = (Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000100000007"))),
      lhcRefreshToken = "DXD["
    }

testObject_LegalHoldServiceConfirm_team_14 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_team_14 =
  LegalHoldServiceConfirm
    { lhcClientId = ClientId {client = "2"},
      lhcUserId = (Id (fromJust (UUID.fromString "00000007-0000-0003-0000-000200000003"))),
      lhcTeamId = (Id (fromJust (UUID.fromString "00000004-0000-0001-0000-000400000003"))),
      lhcRefreshToken = "T\1068224\DC3\177787\STX"
    }

testObject_LegalHoldServiceConfirm_team_15 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_team_15 =
  LegalHoldServiceConfirm
    { lhcClientId = ClientId {client = "1a"},
      lhcUserId = (Id (fromJust (UUID.fromString "00000005-0000-0005-0000-000300000007"))),
      lhcTeamId = (Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000100000004"))),
      lhcRefreshToken = "\n' \FS~\137351)"
    }

testObject_LegalHoldServiceConfirm_team_16 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_team_16 =
  LegalHoldServiceConfirm
    { lhcClientId = ClientId {client = "e"},
      lhcUserId = (Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000000000000"))),
      lhcTeamId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000300000000"))),
      lhcRefreshToken = "\65915\163144\n"
    }

testObject_LegalHoldServiceConfirm_team_17 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_team_17 =
  LegalHoldServiceConfirm
    { lhcClientId = ClientId {client = "e"},
      lhcUserId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000600000004"))),
      lhcTeamId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000400000008"))),
      lhcRefreshToken = ""
    }

testObject_LegalHoldServiceConfirm_team_18 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_team_18 =
  LegalHoldServiceConfirm
    { lhcClientId = ClientId {client = "11"},
      lhcUserId = (Id (fromJust (UUID.fromString "00000006-0000-0000-0000-000800000004"))),
      lhcTeamId = (Id (fromJust (UUID.fromString "00000006-0000-0003-0000-000100000005"))),
      lhcRefreshToken = "Y\1029262"
    }

testObject_LegalHoldServiceConfirm_team_19 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_team_19 =
  LegalHoldServiceConfirm
    { lhcClientId = ClientId {client = "1c"},
      lhcUserId = (Id (fromJust (UUID.fromString "00000003-0000-0006-0000-000700000002"))),
      lhcTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000600000000"))),
      lhcRefreshToken = "["
    }

testObject_LegalHoldServiceConfirm_team_20 :: LegalHoldServiceConfirm
testObject_LegalHoldServiceConfirm_team_20 =
  LegalHoldServiceConfirm
    { lhcClientId = ClientId {client = "1"},
      lhcUserId = (Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000600000005"))),
      lhcTeamId = (Id (fromJust (UUID.fromString "00000006-0000-0001-0000-000500000008"))),
      lhcRefreshToken = "i\FS"
    }
