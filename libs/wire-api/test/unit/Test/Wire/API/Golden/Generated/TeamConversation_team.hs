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
testObject_TeamConversation_team_1 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000018-0000-007a-0000-003a00000064")))) (False))
testObject_TeamConversation_team_2 :: TeamConversation
testObject_TeamConversation_team_2 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000055-0000-0063-0000-000000000031")))) (False))
testObject_TeamConversation_team_3 :: TeamConversation
testObject_TeamConversation_team_3 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000049-0000-002c-0000-00130000004f")))) (True))
testObject_TeamConversation_team_4 :: TeamConversation
testObject_TeamConversation_team_4 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000028-0000-001f-0000-001300000073")))) (True))
testObject_TeamConversation_team_5 :: TeamConversation
testObject_TeamConversation_team_5 = (newTeamConversation ((Id (fromJust (UUID.fromString "0000007a-0000-0065-0000-00020000007c")))) (False))
testObject_TeamConversation_team_6 :: TeamConversation
testObject_TeamConversation_team_6 = (newTeamConversation ((Id (fromJust (UUID.fromString "0000005e-0000-0068-0000-004800000071")))) (False))
testObject_TeamConversation_team_7 :: TeamConversation
testObject_TeamConversation_team_7 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000034-0000-0044-0000-003b00000005")))) (True))
testObject_TeamConversation_team_8 :: TeamConversation
testObject_TeamConversation_team_8 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000014-0000-006a-0000-00530000001d")))) (True))
testObject_TeamConversation_team_9 :: TeamConversation
testObject_TeamConversation_team_9 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000042-0000-0078-0000-007700000045")))) (True))
testObject_TeamConversation_team_10 :: TeamConversation
testObject_TeamConversation_team_10 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000079-0000-0071-0000-005d00000018")))) (True))
testObject_TeamConversation_team_11 :: TeamConversation
testObject_TeamConversation_team_11 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000003-0000-0002-0000-003400000020")))) (True))
testObject_TeamConversation_team_12 :: TeamConversation
testObject_TeamConversation_team_12 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000070-0000-000d-0000-006400000052")))) (True))
testObject_TeamConversation_team_13 :: TeamConversation
testObject_TeamConversation_team_13 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000051-0000-0039-0000-002c00000048")))) (False))
testObject_TeamConversation_team_14 :: TeamConversation
testObject_TeamConversation_team_14 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000044-0000-0050-0000-000100000016")))) (True))
testObject_TeamConversation_team_15 :: TeamConversation
testObject_TeamConversation_team_15 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000043-0000-006a-0000-00000000007f")))) (False))
testObject_TeamConversation_team_16 :: TeamConversation
testObject_TeamConversation_team_16 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000047-0000-004d-0000-006e0000001d")))) (False))
testObject_TeamConversation_team_17 :: TeamConversation
testObject_TeamConversation_team_17 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000053-0000-004e-0000-003a0000004a")))) (True))
testObject_TeamConversation_team_18 :: TeamConversation
testObject_TeamConversation_team_18 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000059-0000-0002-0000-002b00000036")))) (False))
testObject_TeamConversation_team_19 :: TeamConversation
testObject_TeamConversation_team_19 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000034-0000-007c-0000-006c00000079")))) (False))
testObject_TeamConversation_team_20 :: TeamConversation
testObject_TeamConversation_team_20 = (newTeamConversation ((Id (fromJust (UUID.fromString "0000005f-0000-005e-0000-001700000056")))) (True))
