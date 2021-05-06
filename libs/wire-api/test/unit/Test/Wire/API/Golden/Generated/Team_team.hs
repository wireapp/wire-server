{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Team_team where
import Control.Lens ( (.~) )
import Data.Id ( Id(Id) )
import Imports ( Maybe(Nothing, Just), (&), fromJust )
import qualified Data.UUID as UUID ( fromString )
import Wire.API.Team
    ( Team, newTeam, teamIconKey, TeamBinding(Binding, NonBinding) )

testObject_Team_team_1 :: Team
testObject_Team_team_1 = (newTeam ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000002")))) ((Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000000000001")))) ("\SI!7{") ("X^\1114057") (Binding) & teamIconKey .~ (Nothing))
testObject_Team_team_2 :: Team
testObject_Team_team_2 = (newTeam ((Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000200000000")))) ((Id (fromJust (UUID.fromString "00000004-0000-0001-0000-000200000001")))) ("y") ("") (NonBinding) & teamIconKey .~ (Just "6\DC4\RSH"))
testObject_Team_team_3 :: Team
testObject_Team_team_3 = (newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000100000001")))) ((Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000100000002")))) ("B") ("\ETX\996896\&7\174272") (Binding) & teamIconKey .~ (Just ""))
testObject_Team_team_4 :: Team
testObject_Team_team_4 = (newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000000")))) ((Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000100000004")))) ("\DC2\1060281)") ("C") (NonBinding) & teamIconKey .~ (Just ""))
testObject_Team_team_5 :: Team
testObject_Team_team_5 = (newTeam ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000004")))) ((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000003")))) ("W:") ("\SYN13") (Binding) & teamIconKey .~ (Nothing))
testObject_Team_team_6 :: Team
testObject_Team_team_6 = (newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000002")))) ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000003")))) ("\RSd\CAN5") ("iQ\FS\f") (NonBinding) & teamIconKey .~ (Nothing))
testObject_Team_team_7 :: Team
testObject_Team_team_7 = (newTeam ((Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000000000000")))) ((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000001")))) ("\992743") (" e") (Binding) & teamIconKey .~ (Nothing))
testObject_Team_team_8 :: Team
testObject_Team_team_8 = (newTeam ((Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000100000003")))) ((Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000400000000")))) ("\SUBw*") ("r\CAN\1015204t&") (NonBinding) & teamIconKey .~ (Nothing))
testObject_Team_team_9 :: Team
testObject_Team_team_9 = (newTeam ((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000300000001")))) ((Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000400000003")))) ("f") ("") (Binding) & teamIconKey .~ (Nothing))
testObject_Team_team_10 :: Team
testObject_Team_team_10 = (newTeam ((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000400000003")))) ((Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000200000001")))) ("\v") ("\1026515\140299zE") (Binding) & teamIconKey .~ (Nothing))
testObject_Team_team_11 :: Team
testObject_Team_team_11 = (newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000200000003")))) ((Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000000000004")))) ("I") ("S0") (Binding) & teamIconKey .~ (Just "By"))
testObject_Team_team_12 :: Team
testObject_Team_team_12 = (newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000100000004")))) ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000003")))) ("\1101824\1049038") ("\SOHX\68485") (NonBinding) & teamIconKey .~ (Nothing))
testObject_Team_team_13 :: Team
testObject_Team_team_13 = (newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000300000001")))) ((Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000200000002")))) ("S\DEL\988683\ETX") ("\1054289\1111423$n") (NonBinding) & teamIconKey .~ (Just "\1100550"))
testObject_Team_team_14 :: Team
testObject_Team_team_14 = (newTeam ((Id (fromJust (UUID.fromString "00000004-0000-0001-0000-000000000004")))) ((Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000300000004")))) ("_n\EM\NUL\r") ("\a") (NonBinding) & teamIconKey .~ (Nothing))
testObject_Team_team_15 :: Team
testObject_Team_team_15 = (newTeam ((Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000300000003")))) ((Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000400000000")))) ("\191279") ("\ENQ\2540\RS\1026619!") (NonBinding) & teamIconKey .~ (Just ""))
testObject_Team_team_16 :: Team
testObject_Team_team_16 = (newTeam ((Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000200000003")))) ((Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000200000001")))) ("`j\994927") ("") (NonBinding) & teamIconKey .~ (Just "\180793"))
testObject_Team_team_17 :: Team
testObject_Team_team_17 = (newTeam ((Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000400000000")))) ((Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000400000001")))) ("") ("~") (NonBinding) & teamIconKey .~ (Nothing))
testObject_Team_team_18 :: Team
testObject_Team_team_18 = (newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000000")))) ((Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000000000000")))) ("y") ("$*") (NonBinding) & teamIconKey .~ (Nothing))
testObject_Team_team_19 :: Team
testObject_Team_team_19 = (newTeam ((Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000300000002")))) ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000400000002")))) ("_I") (";\1046187\t|'") (NonBinding) & teamIconKey .~ (Just ""))
testObject_Team_team_20 :: Team
testObject_Team_team_20 = (newTeam ((Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000000000001")))) ((Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000300000002")))) ("") ("#i%\1089103v") (Binding) & teamIconKey .~ (Nothing))
