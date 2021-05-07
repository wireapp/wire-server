{-# LANGUAGE OverloadedLists #-}

module Test.Wire.API.Golden.Generated.TeamList_team where

import Control.Lens ((.~))
import Data.Id (Id (Id))
import qualified Data.UUID as UUID (fromString)
import Imports
  ( Bool (False, True),
    Maybe (Just, Nothing),
    fromJust,
    (&),
  )
import Wire.API.Team
  ( TeamBinding (Binding, NonBinding),
    TeamList (..),
    newTeam,
    teamIconKey,
  )

testObject_TeamList_team_1 :: TeamList
testObject_TeamList_team_1 = TeamList {_teamListTeams = [(newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))) ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))) ("") ("") (NonBinding) & teamIconKey .~ (Just "")), (newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))) ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))) ("") ("") (NonBinding) & teamIconKey .~ (Just "")), (newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))) ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))) ("") ("") (NonBinding) & teamIconKey .~ (Nothing))], _teamListHasMore = False}

testObject_TeamList_team_2 :: TeamList
testObject_TeamList_team_2 = TeamList {_teamListTeams = [(newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))) ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))) ("7") ("\174380") (Binding) & teamIconKey .~ (Just "@")), (newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))) ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))) ("") ("") (Binding) & teamIconKey .~ (Just ""))], _teamListHasMore = False}

testObject_TeamList_team_3 :: TeamList
testObject_TeamList_team_3 = TeamList {_teamListTeams = [(newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000000")))) ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000002")))) ("") ("") (NonBinding) & teamIconKey .~ (Nothing))], _teamListHasMore = False}

testObject_TeamList_team_4 :: TeamList
testObject_TeamList_team_4 = TeamList {_teamListTeams = [(newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))) ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))) ("\1065164") ("") (NonBinding) & teamIconKey .~ (Just "")), (newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))) ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))) ("") ("") (NonBinding) & teamIconKey .~ (Just ""))], _teamListHasMore = False}

testObject_TeamList_team_5 :: TeamList
testObject_TeamList_team_5 = TeamList {_teamListTeams = [(newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))) ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))) ("") ("") (NonBinding) & teamIconKey .~ (Just "")), (newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))) ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))) ("") ("") (Binding) & teamIconKey .~ (Just "")), (newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))) ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))) ("") ("") (NonBinding) & teamIconKey .~ (Just "")), (newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))) ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))) ("") ("") (NonBinding) & teamIconKey .~ (Just "")), (newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))) ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))) ("") ("") (Binding) & teamIconKey .~ (Just "")), (newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))) ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))) ("") ("") (NonBinding) & teamIconKey .~ (Nothing)), (newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))) ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))) ("") ("") (NonBinding) & teamIconKey .~ (Just ""))], _teamListHasMore = True}

testObject_TeamList_team_6 :: TeamList
testObject_TeamList_team_6 = TeamList {_teamListTeams = [(newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))) ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))) (" ") ("\1027039") (NonBinding) & teamIconKey .~ (Nothing))], _teamListHasMore = True}

testObject_TeamList_team_7 :: TeamList
testObject_TeamList_team_7 = TeamList {_teamListTeams = [(newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))) ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))) ("") ("\DC1") (NonBinding) & teamIconKey .~ (Nothing)), (newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))) ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))) ("") ("") (Binding) & teamIconKey .~ (Just ""))], _teamListHasMore = False}

testObject_TeamList_team_8 :: TeamList
testObject_TeamList_team_8 = TeamList {_teamListTeams = [], _teamListHasMore = True}

testObject_TeamList_team_9 :: TeamList
testObject_TeamList_team_9 = TeamList {_teamListTeams = [(newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))) ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))) ("") ("") (NonBinding) & teamIconKey .~ (Just "")), (newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))) ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))) ("") ("") (NonBinding) & teamIconKey .~ (Just "")), (newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))) ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))) ("") ("") (NonBinding) & teamIconKey .~ (Nothing)), (newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))) ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))) ("") ("") (Binding) & teamIconKey .~ (Just "")), (newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))) ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))) ("") ("") (NonBinding) & teamIconKey .~ (Just "")), (newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))) ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))) ("") ("") (NonBinding) & teamIconKey .~ (Just ""))], _teamListHasMore = True}

testObject_TeamList_team_10 :: TeamList
testObject_TeamList_team_10 = TeamList {_teamListTeams = [], _teamListHasMore = False}

testObject_TeamList_team_11 :: TeamList
testObject_TeamList_team_11 = TeamList {_teamListTeams = [], _teamListHasMore = False}

testObject_TeamList_team_12 :: TeamList
testObject_TeamList_team_12 = TeamList {_teamListTeams = [(newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000001")))) ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000000")))) ("/\38175") ("bi") (NonBinding) & teamIconKey .~ (Just ""))], _teamListHasMore = True}

testObject_TeamList_team_13 :: TeamList
testObject_TeamList_team_13 = TeamList {_teamListTeams = [], _teamListHasMore = True}

testObject_TeamList_team_14 :: TeamList
testObject_TeamList_team_14 = TeamList {_teamListTeams = [(newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))) ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))) ("") ("") (NonBinding) & teamIconKey .~ (Just "")), (newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))) ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))) ("") ("") (NonBinding) & teamIconKey .~ (Just "")), (newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))) ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))) ("") ("") (NonBinding) & teamIconKey .~ (Just "")), (newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))) ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))) ("") ("") (NonBinding) & teamIconKey .~ (Just "")), (newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))) ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))) ("") ("") (NonBinding) & teamIconKey .~ (Just "")), (newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))) ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))) ("") ("") (NonBinding) & teamIconKey .~ (Nothing))], _teamListHasMore = True}

testObject_TeamList_team_15 :: TeamList
testObject_TeamList_team_15 = TeamList {_teamListTeams = [(newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))) ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))) ("") ("") (NonBinding) & teamIconKey .~ (Nothing)), (newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))) ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))) ("") ("") (NonBinding) & teamIconKey .~ (Just "")), (newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))) ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))) ("") ("") (Binding) & teamIconKey .~ (Nothing)), (newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))) ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))) ("") ("") (NonBinding) & teamIconKey .~ (Just "")), (newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))) ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))) ("") ("") (Binding) & teamIconKey .~ (Nothing))], _teamListHasMore = False}

testObject_TeamList_team_16 :: TeamList
testObject_TeamList_team_16 = TeamList {_teamListTeams = [(newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000002")))) ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000000")))) ("\170783") ("e\20069") (Binding) & teamIconKey .~ (Just "\1113463("))], _teamListHasMore = True}

testObject_TeamList_team_17 :: TeamList
testObject_TeamList_team_17 = TeamList {_teamListTeams = [], _teamListHasMore = True}

testObject_TeamList_team_18 :: TeamList
testObject_TeamList_team_18 = TeamList {_teamListTeams = [(newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002")))) ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000000")))) ("W1") ("!") (NonBinding) & teamIconKey .~ (Nothing))], _teamListHasMore = True}

testObject_TeamList_team_19 :: TeamList
testObject_TeamList_team_19 = TeamList {_teamListTeams = [(newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000000")))) ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000002")))) ("") ("7") (Binding) & teamIconKey .~ (Just "\189413("))], _teamListHasMore = False}

testObject_TeamList_team_20 :: TeamList
testObject_TeamList_team_20 = TeamList {_teamListTeams = [(newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))) ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))) ("") ("") (Binding) & teamIconKey .~ (Nothing)), (newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))) ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))) ("") ("") (Binding) & teamIconKey .~ (Just "")), (newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))) ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))) ("") ("") (Binding) & teamIconKey .~ (Just "")), (newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))) ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))) ("") ("") (NonBinding) & teamIconKey .~ (Nothing))], _teamListHasMore = False}
