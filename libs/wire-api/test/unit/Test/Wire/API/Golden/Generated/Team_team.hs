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

module Test.Wire.API.Golden.Generated.Team_team where

import Control.Lens ((.~))
import Data.Id (Id (Id))
import qualified Data.UUID as UUID (fromString)
import Imports (Maybe (Just, Nothing), fromJust, (&))
import Wire.API.Team
  ( Team,
    TeamBinding (Binding, NonBinding),
    newTeam,
    teamIconKey,
  )

testObject_Team_team_1 :: Team
testObject_Team_team_1 = (newTeam ((Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000200000000")))) ((Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000100000002")))) ("TJ\EOT") ("Jw\USTB") (Binding) & teamIconKey .~ (Just "\1040673V"))

testObject_Team_team_2 :: Team
testObject_Team_team_2 = (newTeam ((Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000000000004")))) ((Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000000000001")))) ("Yc\5828") ("\1104693\t5") (NonBinding) & teamIconKey .~ (Just "\34417R3q"))

testObject_Team_team_3 :: Team
testObject_Team_team_3 = (newTeam ((Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000000000003")))) ((Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000100000000")))) ("2E\1092885") ("") (NonBinding) & teamIconKey .~ (Just "s\1056436"))

testObject_Team_team_4 :: Team
testObject_Team_team_4 = (newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000004")))) ((Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000100000003")))) ("\177218\bk") ("\1078494u\FSC\SOH") (NonBinding) & teamIconKey .~ (Just "X"))

testObject_Team_team_5 :: Team
testObject_Team_team_5 = (newTeam ((Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000000000004")))) ((Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000200000002")))) ("\ACK\99388\20164") ("\1073797") (Binding) & teamIconKey .~ (Just "?&\ESC"))

testObject_Team_team_6 :: Team
testObject_Team_team_6 = (newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000001")))) ((Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000000000003")))) ("\1018732x\1035024]\15985") ("_'\DC1\STX") (NonBinding) & teamIconKey .~ (Nothing))

testObject_Team_team_7 :: Team
testObject_Team_team_7 = (newTeam ((Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000000000002")))) ((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000400000000")))) ("\9929\1053910\1017456\&7\1059453") ("X\n|\1041562") (Binding) & teamIconKey .~ (Just "\96549"))

testObject_Team_team_8 :: Team
testObject_Team_team_8 = (newTeam ((Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000000000001")))) ((Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000400000001")))) ("\r\37334{\DC3\\") ("\57585\1029014") (NonBinding) & teamIconKey .~ (Nothing))

testObject_Team_team_9 :: Team
testObject_Team_team_9 = (newTeam ((Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000200000003")))) ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000004")))) ("G[Hu{") ("d\ETXU") (NonBinding) & teamIconKey .~ (Nothing))

testObject_Team_team_10 :: Team
testObject_Team_team_10 = (newTeam ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000300000004")))) ((Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000300000000")))) ("\1043846") (" ") (Binding) & teamIconKey .~ (Just "\1107305"))

testObject_Team_team_11 :: Team
testObject_Team_team_11 = (newTeam ((Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000300000003")))) ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000003")))) ("") ("b@\STX\47358") (NonBinding) & teamIconKey .~ (Nothing))

testObject_Team_team_12 :: Team
testObject_Team_team_12 = (newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000001")))) ((Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000200000001")))) ("yR\EOTU}") ("P\185409") (Binding) & teamIconKey .~ (Just "J\SI`\1074001\DEL"))

testObject_Team_team_13 :: Team
testObject_Team_team_13 = (newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000002")))) ((Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000200000004")))) ("E\ESC") ("") (NonBinding) & teamIconKey .~ (Nothing))

testObject_Team_team_14 :: Team
testObject_Team_team_14 = (newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000100000004")))) ((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000003")))) (".\27232,") ("") (NonBinding) & teamIconKey .~ (Just "N\EM\ETX"))

testObject_Team_team_15 :: Team
testObject_Team_team_15 = (newTeam ((Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000000000003")))) ((Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000400000002")))) ("#k\NUL,;") ("yM\RS\ENQ") (Binding) & teamIconKey .~ (Just "T\f)\tR"))

testObject_Team_team_16 :: Team
testObject_Team_team_16 = (newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000000")))) ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000400000004")))) ("") ("Se") (Binding) & teamIconKey .~ (Just "\SOHC"))

testObject_Team_team_17 :: Team
testObject_Team_team_17 = (newTeam ((Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000400000004")))) ((Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000000000004")))) ("\t\b ") ("A\1029674'W") (Binding) & teamIconKey .~ (Nothing))

testObject_Team_team_18 :: Team
testObject_Team_team_18 = (newTeam ((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000002")))) ((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000002")))) ("\23385\1046442") ("_\1029329\170131") (NonBinding) & teamIconKey .~ (Just "x:\40938L"))

testObject_Team_team_19 :: Team
testObject_Team_team_19 = (newTeam ((Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000100000001")))) ((Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000200000004")))) ("P\187859;gi") (")\ETB\ENQ") (Binding) & teamIconKey .~ (Just "V>A"))

testObject_Team_team_20 :: Team
testObject_Team_team_20 = (newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000400000003")))) ((Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000000000004")))) ("\191094c") ("\1019354I\STX\ETX") (NonBinding) & teamIconKey .~ (Just "v0\1099892\&3"))
