{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.TeamConversation_team where

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
testObject_TeamConversation_team_1 :: TeamConversation
testObject_TeamConversation_team_1 = (newTeamConversation ((Id (fromJust (UUID.fromString "0000001e-0000-006f-0000-001e00000051")))) (False))
testObject_TeamConversation_team_2 :: TeamConversation
testObject_TeamConversation_team_2 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000001-0000-0075-0000-00640000000c")))) (True))
testObject_TeamConversation_team_3 :: TeamConversation
testObject_TeamConversation_team_3 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000047-0000-0008-0000-000c0000004d")))) (True))
testObject_TeamConversation_team_4 :: TeamConversation
testObject_TeamConversation_team_4 = (newTeamConversation ((Id (fromJust (UUID.fromString "0000006d-0000-0049-0000-003d00000062")))) (True))
testObject_TeamConversation_team_5 :: TeamConversation
testObject_TeamConversation_team_5 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000059-0000-0017-0000-007100000077")))) (False))
testObject_TeamConversation_team_6 :: TeamConversation
testObject_TeamConversation_team_6 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000006-0000-0064-0000-004e00000027")))) (True))
testObject_TeamConversation_team_7 :: TeamConversation
testObject_TeamConversation_team_7 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000074-0000-001d-0000-000a0000000c")))) (True))
testObject_TeamConversation_team_8 :: TeamConversation
testObject_TeamConversation_team_8 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000069-0000-0015-0000-006200000061")))) (True))
testObject_TeamConversation_team_9 :: TeamConversation
testObject_TeamConversation_team_9 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000061-0000-0069-0000-003800000036")))) (True))
testObject_TeamConversation_team_10 :: TeamConversation
testObject_TeamConversation_team_10 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000014-0000-0009-0000-000f0000007a")))) (False))
testObject_TeamConversation_team_11 :: TeamConversation
testObject_TeamConversation_team_11 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000077-0000-006f-0000-005f0000006b")))) (True))
testObject_TeamConversation_team_12 :: TeamConversation
testObject_TeamConversation_team_12 = (newTeamConversation ((Id (fromJust (UUID.fromString "0000005a-0000-000a-0000-006e0000005a")))) (False))
testObject_TeamConversation_team_13 :: TeamConversation
testObject_TeamConversation_team_13 = (newTeamConversation ((Id (fromJust (UUID.fromString "0000007b-0000-0018-0000-005a0000005d")))) (False))
testObject_TeamConversation_team_14 :: TeamConversation
testObject_TeamConversation_team_14 = (newTeamConversation ((Id (fromJust (UUID.fromString "0000006d-0000-000c-0000-001d00000002")))) (False))
testObject_TeamConversation_team_15 :: TeamConversation
testObject_TeamConversation_team_15 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000000-0000-007e-0000-003b0000007c")))) (False))
testObject_TeamConversation_team_16 :: TeamConversation
testObject_TeamConversation_team_16 = (newTeamConversation ((Id (fromJust (UUID.fromString "0000002e-0000-0009-0000-000100000051")))) (False))
testObject_TeamConversation_team_17 :: TeamConversation
testObject_TeamConversation_team_17 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000023-0000-0075-0000-005000000019")))) (True))
testObject_TeamConversation_team_18 :: TeamConversation
testObject_TeamConversation_team_18 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000054-0000-005c-0000-004000000019")))) (True))
testObject_TeamConversation_team_19 :: TeamConversation
testObject_TeamConversation_team_19 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000075-0000-002a-0000-007400000046")))) (True))
testObject_TeamConversation_team_20 :: TeamConversation
testObject_TeamConversation_team_20 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000041-0000-004d-0000-005700000027")))) (False))
