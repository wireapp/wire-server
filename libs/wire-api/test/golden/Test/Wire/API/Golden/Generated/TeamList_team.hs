{-# LANGUAGE OverloadedLists #-}

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

module Test.Wire.API.Golden.Generated.TeamList_team where

import Control.Lens ((.~), (?~))
import Data.Id (Id (Id))
import qualified Data.UUID as UUID (fromString)
import Imports (Bool (False, True), Maybe (..), fromJust, (&))
import Wire.API.Asset
import Wire.API.Team (Icon (..), TeamBinding (Binding, NonBinding), TeamList (..), newTeam, teamIconKey)

testObject_TeamList_team_1 :: TeamList
testObject_TeamList_team_1 =
  TeamList
    { _teamListTeams =
        [ newTeam
            (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))
            (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))
            ""
            (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
            NonBinding
            & teamIconKey ?~ "",
          newTeam
            (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))
            (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))
            ""
            DefaultIcon
            NonBinding
            & teamIconKey ?~ "",
          newTeam
            (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))
            (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))
            ""
            (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
            NonBinding
            & teamIconKey .~ Nothing
        ],
      _teamListHasMore = False
    }

testObject_TeamList_team_2 :: TeamList
testObject_TeamList_team_2 =
  TeamList
    { _teamListTeams =
        [ newTeam
            (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))
            (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))
            "7"
            DefaultIcon
            Binding
            & teamIconKey ?~ "@",
          newTeam
            (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))
            (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))
            ""
            (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
            Binding
            & teamIconKey ?~ ""
        ],
      _teamListHasMore = False
    }

testObject_TeamList_team_3 :: TeamList
testObject_TeamList_team_3 =
  TeamList
    { _teamListTeams =
        [ newTeam
            (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000000")))
            (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000002")))
            ""
            (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
            NonBinding
            & teamIconKey .~ Nothing
        ],
      _teamListHasMore = False
    }

testObject_TeamList_team_4 :: TeamList
testObject_TeamList_team_4 =
  TeamList
    { _teamListTeams =
        [ newTeam
            (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))
            (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))
            "\1065164"
            (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
            NonBinding
            & teamIconKey ?~ "",
          newTeam
            (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))
            (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))
            ""
            (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
            NonBinding
            & teamIconKey ?~ ""
        ],
      _teamListHasMore = False
    }

testObject_TeamList_team_5 :: TeamList
testObject_TeamList_team_5 =
  TeamList
    { _teamListTeams =
        [ newTeam
            (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))
            (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))
            ""
            (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
            NonBinding
            & teamIconKey ?~ "",
          newTeam
            (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))
            (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))
            ""
            (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
            Binding
            & teamIconKey ?~ "",
          newTeam
            (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))
            (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))
            ""
            (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
            NonBinding
            & teamIconKey ?~ "",
          newTeam
            (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))
            (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))
            ""
            (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
            NonBinding
            & teamIconKey ?~ "",
          newTeam
            (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))
            (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))
            ""
            (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
            Binding
            & teamIconKey ?~ "",
          newTeam
            (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))
            (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))
            ""
            (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
            NonBinding
            & teamIconKey .~ Nothing,
          newTeam
            (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))
            (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))
            ""
            (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
            NonBinding
            & teamIconKey ?~ ""
        ],
      _teamListHasMore = True
    }

testObject_TeamList_team_6 :: TeamList
testObject_TeamList_team_6 =
  TeamList
    { _teamListTeams =
        [ newTeam
            (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))
            (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))
            " "
            (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
            NonBinding
            & teamIconKey .~ Nothing
        ],
      _teamListHasMore = True
    }

testObject_TeamList_team_7 :: TeamList
testObject_TeamList_team_7 =
  TeamList
    { _teamListTeams =
        [ newTeam
            (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))
            (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))
            ""
            (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
            NonBinding
            & teamIconKey .~ Nothing,
          newTeam
            (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))
            (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))
            ""
            (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
            Binding
            & teamIconKey ?~ ""
        ],
      _teamListHasMore = False
    }

testObject_TeamList_team_8 :: TeamList
testObject_TeamList_team_8 = TeamList {_teamListTeams = [], _teamListHasMore = True}

testObject_TeamList_team_9 :: TeamList
testObject_TeamList_team_9 =
  TeamList
    { _teamListTeams =
        [ newTeam
            (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))
            (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))
            ""
            (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
            NonBinding
            & teamIconKey ?~ "",
          newTeam
            (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))
            (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))
            ""
            (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
            NonBinding
            & teamIconKey ?~ "",
          newTeam
            (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))
            (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))
            ""
            (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
            NonBinding
            & teamIconKey .~ Nothing,
          newTeam
            (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))
            (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))
            ""
            (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
            Binding
            & teamIconKey ?~ "",
          newTeam
            (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))
            (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))
            ""
            (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
            NonBinding
            & teamIconKey ?~ "",
          newTeam
            (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))
            (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))
            ""
            (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
            NonBinding
            & teamIconKey ?~ ""
        ],
      _teamListHasMore = True
    }

testObject_TeamList_team_10 :: TeamList
testObject_TeamList_team_10 = TeamList {_teamListTeams = [], _teamListHasMore = False}

testObject_TeamList_team_11 :: TeamList
testObject_TeamList_team_11 = TeamList {_teamListTeams = [], _teamListHasMore = False}

testObject_TeamList_team_12 :: TeamList
testObject_TeamList_team_12 =
  TeamList
    { _teamListTeams =
        [ newTeam
            (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000001")))
            (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000000")))
            "/\38175"
            (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
            NonBinding
            & teamIconKey ?~ ""
        ],
      _teamListHasMore = True
    }

testObject_TeamList_team_13 :: TeamList
testObject_TeamList_team_13 = TeamList {_teamListTeams = [], _teamListHasMore = True}

testObject_TeamList_team_14 :: TeamList
testObject_TeamList_team_14 =
  TeamList
    { _teamListTeams =
        [ newTeam
            (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))
            (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))
            ""
            (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
            NonBinding
            & teamIconKey ?~ "",
          newTeam
            (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))
            (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))
            ""
            DefaultIcon
            NonBinding
            & teamIconKey ?~ "",
          newTeam
            (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))
            (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))
            ""
            DefaultIcon
            NonBinding
            & teamIconKey ?~ "",
          newTeam
            (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))
            (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))
            ""
            DefaultIcon
            NonBinding
            & teamIconKey ?~ "",
          newTeam
            (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))
            (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))
            ""
            DefaultIcon
            NonBinding
            & teamIconKey ?~ "",
          newTeam
            (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))
            (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))
            ""
            (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
            NonBinding
            & teamIconKey .~ Nothing
        ],
      _teamListHasMore = True
    }

testObject_TeamList_team_15 :: TeamList
testObject_TeamList_team_15 =
  TeamList
    { _teamListTeams =
        [ newTeam
            (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))
            (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))
            ""
            (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
            NonBinding
            & teamIconKey .~ Nothing,
          newTeam
            (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))
            (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))
            ""
            (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
            NonBinding
            & teamIconKey ?~ "",
          newTeam
            (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))
            (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))
            ""
            (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
            Binding
            & teamIconKey .~ Nothing,
          newTeam
            (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))
            (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))
            ""
            (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
            NonBinding
            & teamIconKey ?~ "",
          newTeam
            (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))
            (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))
            ""
            (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
            Binding
            & teamIconKey .~ Nothing
        ],
      _teamListHasMore = False
    }

testObject_TeamList_team_16 :: TeamList
testObject_TeamList_team_16 =
  TeamList
    { _teamListTeams =
        [ newTeam
            (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000002")))
            (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000000")))
            "\170783"
            (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
            Binding
            & teamIconKey ?~ "\1113463("
        ],
      _teamListHasMore = True
    }

testObject_TeamList_team_17 :: TeamList
testObject_TeamList_team_17 = TeamList {_teamListTeams = [], _teamListHasMore = True}

testObject_TeamList_team_18 :: TeamList
testObject_TeamList_team_18 =
  TeamList
    { _teamListTeams =
        [ newTeam
            (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002")))
            (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000000")))
            "W1"
            (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
            NonBinding
            & teamIconKey .~ Nothing
        ],
      _teamListHasMore = True
    }

testObject_TeamList_team_19 :: TeamList
testObject_TeamList_team_19 =
  TeamList
    { _teamListTeams =
        [ newTeam
            (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000000")))
            (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000002")))
            ""
            (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
            Binding
            & teamIconKey ?~ "\189413("
        ],
      _teamListHasMore = False
    }

testObject_TeamList_team_20 :: TeamList
testObject_TeamList_team_20 =
  TeamList
    { _teamListTeams =
        [ newTeam
            (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))
            (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))
            ""
            (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
            Binding
            & teamIconKey .~ Nothing,
          newTeam
            (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))
            (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))
            ""
            (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
            Binding
            & teamIconKey ?~ "",
          newTeam
            (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))
            (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))
            ""
            (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
            Binding
            & teamIconKey ?~ "",
          newTeam
            (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))
            (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))
            ""
            (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
            NonBinding
            & teamIconKey .~ Nothing
        ],
      _teamListHasMore = False
    }
