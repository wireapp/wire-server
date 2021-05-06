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
testObject_TeamConversation_team_1 = (newTeamConversation ((Id (fromJust (UUID.fromString "0000002c-0000-0080-0000-006f0000003c")))) (False))
testObject_TeamConversation_team_2 :: TeamConversation
testObject_TeamConversation_team_2 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000038-0000-0033-0000-002400000036")))) (False))
testObject_TeamConversation_team_3 :: TeamConversation
testObject_TeamConversation_team_3 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000015-0000-0064-0000-007800000052")))) (False))
testObject_TeamConversation_team_4 :: TeamConversation
testObject_TeamConversation_team_4 = (newTeamConversation ((Id (fromJust (UUID.fromString "0000004a-0000-0014-0000-003f0000004b")))) (True))
testObject_TeamConversation_team_5 :: TeamConversation
testObject_TeamConversation_team_5 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000024-0000-0010-0000-00770000002c")))) (False))
testObject_TeamConversation_team_6 :: TeamConversation
testObject_TeamConversation_team_6 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000032-0000-0035-0000-004c0000001a")))) (False))
testObject_TeamConversation_team_7 :: TeamConversation
testObject_TeamConversation_team_7 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000013-0000-003c-0000-002600000037")))) (False))
testObject_TeamConversation_team_8 :: TeamConversation
testObject_TeamConversation_team_8 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000017-0000-0060-0000-006100000002")))) (True))
testObject_TeamConversation_team_9 :: TeamConversation
testObject_TeamConversation_team_9 = (newTeamConversation ((Id (fromJust (UUID.fromString "0000001c-0000-000d-0000-001c00000032")))) (True))
testObject_TeamConversation_team_10 :: TeamConversation
testObject_TeamConversation_team_10 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000056-0000-0043-0000-00040000001d")))) (False))
testObject_TeamConversation_team_11 :: TeamConversation
testObject_TeamConversation_team_11 = (newTeamConversation ((Id (fromJust (UUID.fromString "0000007c-0000-000d-0000-004800000075")))) (False))
testObject_TeamConversation_team_12 :: TeamConversation
testObject_TeamConversation_team_12 = (newTeamConversation ((Id (fromJust (UUID.fromString "0000006b-0000-0014-0000-000300000013")))) (True))
testObject_TeamConversation_team_13 :: TeamConversation
testObject_TeamConversation_team_13 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000080-0000-006c-0000-004100000070")))) (True))
testObject_TeamConversation_team_14 :: TeamConversation
testObject_TeamConversation_team_14 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000046-0000-0024-0000-000300000026")))) (True))
testObject_TeamConversation_team_15 :: TeamConversation
testObject_TeamConversation_team_15 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000010-0000-0060-0000-003100000039")))) (False))
testObject_TeamConversation_team_16 :: TeamConversation
testObject_TeamConversation_team_16 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000066-0000-0028-0000-00000000005a")))) (False))
testObject_TeamConversation_team_17 :: TeamConversation
testObject_TeamConversation_team_17 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000011-0000-005a-0000-000d0000006f")))) (True))
testObject_TeamConversation_team_18 :: TeamConversation
testObject_TeamConversation_team_18 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000001-0000-0027-0000-001900000001")))) (True))
testObject_TeamConversation_team_19 :: TeamConversation
testObject_TeamConversation_team_19 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000070-0000-0045-0000-001700000010")))) (True))
testObject_TeamConversation_team_20 :: TeamConversation
testObject_TeamConversation_team_20 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000007-0000-0038-0000-002200000036")))) (True))
