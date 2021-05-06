{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.TeamConversationList_team where

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
testObject_TeamConversationList_team_1 :: TeamConversationList
testObject_TeamConversationList_team_1 = (newTeamConversationList [(newTeamConversation ((Id (fromJust (UUID.fromString "0000002a-0000-0019-0000-00440000001a")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000007e-0000-0046-0000-003a0000004c")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000034-0000-0040-0000-004900000036")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000060-0000-006c-0000-005e00000009")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000004d-0000-0047-0000-004100000048")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000005f-0000-0001-0000-001c00000037")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000006b-0000-0040-0000-00740000000c")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000006-0000-0024-0000-000c0000006f")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000059-0000-000d-0000-000e00000018")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000012-0000-000b-0000-00130000006a")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000050-0000-001c-0000-007500000027")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000078-0000-0001-0000-001900000000")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000009-0000-0075-0000-000d00000001")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000000d-0000-000d-0000-003400000057")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000074-0000-0013-0000-007a00000063")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000059-0000-0077-0000-002b0000004e")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000007d-0000-0003-0000-002600000066")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000003f-0000-000e-0000-003800000009")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000043-0000-001f-0000-00240000000e")))) (True))])
testObject_TeamConversationList_team_2 :: TeamConversationList
testObject_TeamConversationList_team_2 = (newTeamConversationList [(newTeamConversation ((Id (fromJust (UUID.fromString "00000038-0000-0014-0000-00150000004c")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000060-0000-002a-0000-00460000003a")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000036-0000-003a-0000-003300000037")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000041-0000-0066-0000-00550000002c")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000006-0000-0041-0000-007200000040")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000046-0000-007c-0000-006400000029")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000042-0000-004a-0000-006800000011")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000023-0000-0033-0000-003b00000015")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000044-0000-005b-0000-00580000006c")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000004f-0000-001d-0000-00310000003b")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000007d-0000-0017-0000-006f0000003f")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000023-0000-0066-0000-00430000004e")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000005-0000-0078-0000-003a00000074")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000002e-0000-0022-0000-00220000000e")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000013-0000-0007-0000-00500000004d")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000047-0000-0012-0000-00070000001a")))) (False))])
testObject_TeamConversationList_team_3 :: TeamConversationList
testObject_TeamConversationList_team_3 = (newTeamConversationList [(newTeamConversation ((Id (fromJust (UUID.fromString "00000030-0000-0059-0000-001500000048")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000013-0000-004c-0000-007000000076")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000061-0000-0010-0000-000000000060")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000071-0000-0004-0000-001800000054")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000060-0000-0003-0000-001200000052")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000031-0000-0058-0000-000e00000069")))) (True))])
testObject_TeamConversationList_team_4 :: TeamConversationList
testObject_TeamConversationList_team_4 = (newTeamConversationList [(newTeamConversation ((Id (fromJust (UUID.fromString "00000020-0000-0014-0000-001a00000034")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000009-0000-0061-0000-004500000011")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000013-0000-007d-0000-006d0000003d")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000074-0000-002d-0000-00490000001a")))) (True))])
testObject_TeamConversationList_team_5 :: TeamConversationList
testObject_TeamConversationList_team_5 = (newTeamConversationList [(newTeamConversation ((Id (fromJust (UUID.fromString "00000043-0000-0056-0000-002c0000006f")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000054-0000-0002-0000-004200000022")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000012-0000-0019-0000-002400000009")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000009-0000-0052-0000-000700000071")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000031-0000-0047-0000-005b0000000c")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000066-0000-004c-0000-00600000003c")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000015-0000-000b-0000-003100000009")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000073-0000-0043-0000-006d0000003d")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000001c-0000-0080-0000-001000000051")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000001d-0000-0009-0000-006f00000029")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000005-0000-006f-0000-000900000068")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000002a-0000-001b-0000-005d00000030")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000057-0000-0011-0000-000a0000005d")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000037-0000-006c-0000-002700000056")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000001a-0000-0035-0000-003f00000043")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000000-0000-000f-0000-004200000068")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000049-0000-0041-0000-00630000000b")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000002f-0000-004e-0000-006f0000000e")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000068-0000-0078-0000-004c00000028")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000051-0000-0079-0000-003900000048")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000055-0000-006f-0000-003300000012")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000006-0000-003a-0000-00070000002e")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000010-0000-0013-0000-006400000025")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000005f-0000-002f-0000-005300000023")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000042-0000-000d-0000-004200000030")))) (False))])
