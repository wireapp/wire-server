{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.TeamList_team where

import Codec.MIME.Type (Type(..))
import qualified Codec.MIME.Type as MIME
import Control.Lens ((.~))
import Data.Code
import Data.Coerce
import Data.Currency
import Data.Domain
import Data.Handle
import Data.Id
import Data.ISO3166_CountryCodes
import Data.Json.Util
import Data.List1
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty (..))
import Data.Misc
import Data.PEM
import Data.Qualified
import Data.Range (unsafeRange)
import qualified Data.Set as Set
import Data.Text.Ascii
import Data.Time (secondsToNominalDiffTime)
import Imports hiding (LT, GT)
import qualified Data.LanguageCodes
import qualified Data.UUID as UUID
import Test.Tasty (testGroup, TestTree)
import URI.ByteString
import qualified Wire.API.Call.Config as CallConfig
import qualified Wire.API.User.Profile as User.Profile
import qualified Wire.API.Team.Conversation as Team.Conversation
import qualified Wire.API.Provider as Provider
import qualified Wire.API.Provider.Bot as Provider
import qualified Wire.API.Provider.External as Provider
import qualified Wire.API.Provider.Service as Provider
import qualified Wire.API.Provider.Service.Tag as Provider
import Data.Aeson
import GHC.Exts
import Data.LegalHold
import Wire.API.Conversation.Role
import Wire.API.Event.Team
import Wire.API.Provider.Service
import Wire.API.Team
import Wire.API.Team.Conversation
import Wire.API.Team.Feature
import Wire.API.Team.Invitation
import Wire.API.Team.LegalHold
import Wire.API.Team.LegalHold.External
import Wire.API.Team.Member
import Wire.API.Team.Permission
import Wire.API.Team.Role
import Wire.API.Team.SearchVisibility
import Wire.API.User.Client.Prekey
import Wire.API.User.Identity
import Wire.API.User.Profile
testObject_TeamList_team_1 :: TeamList
testObject_TeamList_team_1 = TeamList {_teamListTeams = [(newTeam ((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000001")))) ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000002")))) ("") ("g") (Binding) & teamIconKey .~ (Just "\1105620"))], _teamListHasMore = False}
testObject_TeamList_team_2 :: TeamList
testObject_TeamList_team_2 = TeamList {_teamListTeams = [(newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))) ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))) ("") ("") (NonBinding) & teamIconKey .~ (Nothing)),(newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))) ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))) ("") ("") (Binding) & teamIconKey .~ (Just "")),(newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))) ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))) ("") ("") (Binding) & teamIconKey .~ (Just "")),(newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))) ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))) ("") ("") (NonBinding) & teamIconKey .~ (Just "")),(newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))) ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))) ("") ("") (NonBinding) & teamIconKey .~ (Just "")),(newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))) ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))) ("") ("") (NonBinding) & teamIconKey .~ (Just "")),(newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))) ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))) ("") ("") (NonBinding) & teamIconKey .~ (Just "")),(newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))) ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))) ("") ("") (NonBinding) & teamIconKey .~ (Just "")),(newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))) ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))) ("") ("") (NonBinding) & teamIconKey .~ (Just "")),(newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))) ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))) ("") ("") (NonBinding) & teamIconKey .~ (Just "")),(newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))) ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))) ("") ("") (Binding) & teamIconKey .~ (Just "")),(newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))) ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))) ("") ("") (Binding) & teamIconKey .~ (Nothing)),(newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))) ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))) ("") ("") (NonBinding) & teamIconKey .~ (Just "")),(newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))) ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))) ("") ("") (NonBinding) & teamIconKey .~ (Just "")),(newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))) ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))) ("") ("") (NonBinding) & teamIconKey .~ (Just "")),(newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))) ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))) ("") ("") (Binding) & teamIconKey .~ (Nothing)),(newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))) ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))) ("") ("") (NonBinding) & teamIconKey .~ (Just "")),(newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))) ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))) ("") ("") (Binding) & teamIconKey .~ (Just "")),(newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))) ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))) ("") ("") (NonBinding) & teamIconKey .~ (Just "")),(newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))) ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))) ("") ("") (Binding) & teamIconKey .~ (Just "")),(newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))) ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))) ("") ("") (Binding) & teamIconKey .~ (Just "")),(newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))) ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))) ("") ("") (Binding) & teamIconKey .~ (Just "")),(newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))) ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))) ("") ("") (NonBinding) & teamIconKey .~ (Nothing)),(newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))) ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))) ("") ("") (Binding) & teamIconKey .~ (Just ""))], _teamListHasMore = True}
testObject_TeamList_team_3 :: TeamList
testObject_TeamList_team_3 = TeamList {_teamListTeams = [], _teamListHasMore = True}
testObject_TeamList_team_4 :: TeamList
testObject_TeamList_team_4 = TeamList {_teamListTeams = [(newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))) ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))) ("") ("") (Binding) & teamIconKey .~ (Nothing)),(newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))) ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))) ("") ("") (Binding) & teamIconKey .~ (Just "")),(newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))) ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))) ("") ("") (Binding) & teamIconKey .~ (Just ""))], _teamListHasMore = False}
testObject_TeamList_team_5 :: TeamList
testObject_TeamList_team_5 = TeamList {_teamListTeams = [(newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))) ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))) ("") ("") (Binding) & teamIconKey .~ (Just "")),(newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))) ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))) ("") ("") (NonBinding) & teamIconKey .~ (Just "")),(newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))) ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))) ("") ("") (Binding) & teamIconKey .~ (Nothing)),(newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))) ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))) ("") ("") (NonBinding) & teamIconKey .~ (Just "")),(newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))) ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))) ("") ("") (NonBinding) & teamIconKey .~ (Nothing)),(newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))) ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))) ("") ("") (NonBinding) & teamIconKey .~ (Just ""))], _teamListHasMore = False}
testObject_TeamList_team_6 :: TeamList
testObject_TeamList_team_6 = TeamList {_teamListTeams = [], _teamListHasMore = True}
testObject_TeamList_team_7 :: TeamList
testObject_TeamList_team_7 = TeamList {_teamListTeams = [(newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))) ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))) ("") ("") (NonBinding) & teamIconKey .~ (Nothing)),(newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))) ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))) ("") ("") (NonBinding) & teamIconKey .~ (Just "")),(newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))) ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))) ("") ("") (NonBinding) & teamIconKey .~ (Just "")),(newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))) ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))) ("") ("") (Binding) & teamIconKey .~ (Nothing)),(newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))) ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))) ("") ("") (NonBinding) & teamIconKey .~ (Just ""))], _teamListHasMore = False}
testObject_TeamList_team_8 :: TeamList
testObject_TeamList_team_8 = TeamList {_teamListTeams = [(newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))) ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))) ("") ("") (NonBinding) & teamIconKey .~ (Just "")),(newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))) ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))) ("") ("") (NonBinding) & teamIconKey .~ (Just "")),(newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))) ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))) ("") ("") (NonBinding) & teamIconKey .~ (Just "")),(newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))) ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))) ("") ("") (Binding) & teamIconKey .~ (Just ""))], _teamListHasMore = True}
testObject_TeamList_team_9 :: TeamList
testObject_TeamList_team_9 = TeamList {_teamListTeams = [], _teamListHasMore = False}
testObject_TeamList_team_10 :: TeamList
testObject_TeamList_team_10 = TeamList {_teamListTeams = [(newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))) ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))) ("") ("") (Binding) & teamIconKey .~ (Just "")),(newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))) ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))) ("") ("") (NonBinding) & teamIconKey .~ (Nothing)),(newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))) ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))) ("") ("") (NonBinding) & teamIconKey .~ (Just "")),(newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))) ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))) ("") ("") (NonBinding) & teamIconKey .~ (Just ""))], _teamListHasMore = True}
testObject_TeamList_team_11 :: TeamList
testObject_TeamList_team_11 = TeamList {_teamListTeams = [(newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))) ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))) ("") ("") (Binding) & teamIconKey .~ (Nothing)),(newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))) ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))) ("") ("") (Binding) & teamIconKey .~ (Just "")),(newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))) ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))) ("") ("") (NonBinding) & teamIconKey .~ (Just "")),(newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))) ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))) ("") ("") (NonBinding) & teamIconKey .~ (Nothing)),(newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))) ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))) ("") ("") (NonBinding) & teamIconKey .~ (Nothing))], _teamListHasMore = True}
testObject_TeamList_team_12 :: TeamList
testObject_TeamList_team_12 = TeamList {_teamListTeams = [(newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))) ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))) ("") ("?") (NonBinding) & teamIconKey .~ (Just "\f")),(newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))) ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))) ("") ("") (Binding) & teamIconKey .~ (Just "\ENQ"))], _teamListHasMore = True}
testObject_TeamList_team_13 :: TeamList
testObject_TeamList_team_13 = TeamList {_teamListTeams = [(newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))) ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))) ("") ("") (NonBinding) & teamIconKey .~ (Just "")),(newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))) ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))) ("") ("") (Binding) & teamIconKey .~ (Just "")),(newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))) ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))) ("") ("") (NonBinding) & teamIconKey .~ (Just ""))], _teamListHasMore = False}
testObject_TeamList_team_14 :: TeamList
testObject_TeamList_team_14 = TeamList {_teamListTeams = [], _teamListHasMore = True}
testObject_TeamList_team_15 :: TeamList
testObject_TeamList_team_15 = TeamList {_teamListTeams = [(newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))) ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))) ("") ("") (NonBinding) & teamIconKey .~ (Just "")),(newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))) ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))) ("") ("") (NonBinding) & teamIconKey .~ (Just "")),(newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))) ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))) ("") ("") (Binding) & teamIconKey .~ (Nothing))], _teamListHasMore = False}
testObject_TeamList_team_16 :: TeamList
testObject_TeamList_team_16 = TeamList {_teamListTeams = [(newTeam ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000001")))) ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000002")))) ("") ("\STX&") (Binding) & teamIconKey .~ (Just ""))], _teamListHasMore = False}
testObject_TeamList_team_17 :: TeamList
testObject_TeamList_team_17 = TeamList {_teamListTeams = [], _teamListHasMore = False}
testObject_TeamList_team_18 :: TeamList
testObject_TeamList_team_18 = TeamList {_teamListTeams = [], _teamListHasMore = False}
testObject_TeamList_team_19 :: TeamList
testObject_TeamList_team_19 = TeamList {_teamListTeams = [(newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000002")))) ((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000002")))) ("{\SOH") (">") (NonBinding) & teamIconKey .~ (Just ""))], _teamListHasMore = True}
testObject_TeamList_team_20 :: TeamList
testObject_TeamList_team_20 = TeamList {_teamListTeams = [(newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000002")))) ((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000001")))) ("") ("h\SI") (NonBinding) & teamIconKey .~ (Nothing))], _teamListHasMore = False}
