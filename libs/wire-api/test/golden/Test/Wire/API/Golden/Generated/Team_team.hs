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

module Test.Wire.API.Golden.Generated.Team_team where

import Control.Lens ((.~))
import Data.Id (Id (Id))
import qualified Data.UUID as UUID (fromString)
import Imports (Maybe (Just, Nothing), fromJust, (&))
import Wire.API.Asset
import Wire.API.Team (Icon (..), Team, TeamBinding (Binding, NonBinding), newTeam, teamIconKey, teamSplashScreen)

testObject_Team_team_1 :: Team
testObject_Team_team_1 =
  ( newTeam
      ((Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000200000000"))))
      ((Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000100000002"))))
      ("TJ\EOT")
      (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
      (Binding)
      & teamIconKey .~ (Just "\1040673V")
  )

testObject_Team_team_2 :: Team
testObject_Team_team_2 =
  ( newTeam
      ((Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000000000004"))))
      ((Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000000000001"))))
      ("Yc\5828")
      (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
      (NonBinding)
      & teamIconKey .~ (Just "\34417R3q")
      & teamSplashScreen .~ (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
  )

testObject_Team_team_3 :: Team
testObject_Team_team_3 =
  ( newTeam
      ((Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000000000003"))))
      ((Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000100000000"))))
      ("2E\1092885")
      DefaultIcon
      (NonBinding)
      & teamIconKey .~ (Just "s\1056436")
  )

testObject_Team_team_4 :: Team
testObject_Team_team_4 =
  ( newTeam
      ((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000004"))))
      ((Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000100000003"))))
      ("\177218\bk")
      (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
      (NonBinding)
      & teamIconKey .~ (Just "X")
  )

testObject_Team_team_5 :: Team
testObject_Team_team_5 =
  ( newTeam
      ((Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000000000004"))))
      ((Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000200000002"))))
      ("\ACK\99388\20164")
      (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
      (Binding)
      & teamIconKey .~ (Just "?&\ESC")
  )

testObject_Team_team_6 :: Team
testObject_Team_team_6 =
  ( newTeam
      ((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000001"))))
      ((Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000000000003"))))
      ("\1018732x\1035024]\15985")
      (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
      (NonBinding)
      & teamIconKey .~ (Nothing)
  )

testObject_Team_team_7 :: Team
testObject_Team_team_7 =
  ( newTeam
      ((Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000000000002"))))
      ((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000400000000"))))
      ("\9929\1053910\1017456\&7\1059453")
      (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
      (Binding)
      & teamIconKey .~ (Just "\96549")
  )

testObject_Team_team_8 :: Team
testObject_Team_team_8 =
  ( newTeam
      ((Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000000000001"))))
      ((Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000400000001"))))
      ("\r\37334{\DC3\\")
      (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
      (NonBinding)
      & teamIconKey .~ (Nothing)
  )

testObject_Team_team_9 :: Team
testObject_Team_team_9 =
  ( newTeam
      ((Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000200000003"))))
      ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000004"))))
      ("G[Hu{")
      (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
      (NonBinding)
      & teamIconKey .~ (Nothing)
  )

testObject_Team_team_10 :: Team
testObject_Team_team_10 =
  ( newTeam
      ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000300000004"))))
      ((Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000300000000"))))
      ("\1043846")
      (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
      (Binding)
      & teamIconKey .~ (Just "\1107305")
  )

testObject_Team_team_11 :: Team
testObject_Team_team_11 =
  ( newTeam
      ((Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000300000003"))))
      ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000003"))))
      ("")
      (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
      (NonBinding)
      & teamIconKey .~ (Nothing)
  )

testObject_Team_team_12 :: Team
testObject_Team_team_12 =
  ( newTeam
      ((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000001"))))
      ((Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000200000001"))))
      ("yR\EOTU}")
      (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
      (Binding)
      & teamIconKey .~ (Just "J\SI`\1074001\DEL")
  )

testObject_Team_team_13 :: Team
testObject_Team_team_13 =
  ( newTeam
      ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000002"))))
      ((Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000200000004"))))
      ("E\ESC")
      (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
      (NonBinding)
      & teamIconKey .~ (Nothing)
  )

testObject_Team_team_14 :: Team
testObject_Team_team_14 =
  ( newTeam
      ((Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000100000004"))))
      ((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000003"))))
      (".\27232,")
      DefaultIcon
      (NonBinding)
      & teamIconKey .~ (Just "N\EM\ETX")
  )

testObject_Team_team_15 :: Team
testObject_Team_team_15 =
  ( newTeam
      ((Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000000000003"))))
      ((Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000400000002"))))
      ("#k\NUL,;")
      (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
      (Binding)
      & teamIconKey .~ (Just "T\f)\tR")
  )

testObject_Team_team_16 :: Team
testObject_Team_team_16 =
  ( newTeam
      ((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000000"))))
      ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000400000004"))))
      ("")
      (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
      (Binding)
      & teamIconKey .~ (Just "\SOHC")
  )

testObject_Team_team_17 :: Team
testObject_Team_team_17 =
  ( newTeam
      ((Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000400000004"))))
      ((Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000000000004"))))
      ("\t\b ")
      (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
      (Binding)
      & teamIconKey .~ (Nothing)
  )

testObject_Team_team_18 :: Team
testObject_Team_team_18 =
  ( newTeam
      ((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000002"))))
      ((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000002"))))
      ("\23385\1046442")
      (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
      (NonBinding)
      & teamIconKey .~ (Just "x:\40938L")
  )

testObject_Team_team_19 :: Team
testObject_Team_team_19 =
  ( newTeam
      ((Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000100000001"))))
      ((Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000200000004"))))
      ("P\187859;gi")
      (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
      (Binding)
      & teamIconKey .~ (Just "V>A")
  )

testObject_Team_team_20 :: Team
testObject_Team_team_20 =
  ( newTeam
      ((Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000400000003"))))
      ((Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000000000004"))))
      ("\191094c")
      (Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal))
      (NonBinding)
      & teamIconKey .~ (Just "v0\1099892\&3")
  )
