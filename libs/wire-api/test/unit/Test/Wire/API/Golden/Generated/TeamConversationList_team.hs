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
testObject_TeamConversationList_team_1 = (newTeamConversationList [(newTeamConversation ((Id (fromJust (UUID.fromString "0000005f-0000-001e-0000-003f00000045")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000030-0000-001d-0000-003c00000054")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000071-0000-003a-0000-001e00000022")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000061-0000-005d-0000-00630000005f")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000004-0000-0004-0000-00270000000c")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000043-0000-0067-0000-003d00000031")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000036-0000-0021-0000-004500000068")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000014-0000-0053-0000-000300000003")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000007c-0000-0022-0000-004500000056")))) (True))])
testObject_TeamConversationList_team_2 :: TeamConversationList
testObject_TeamConversationList_team_2 = (newTeamConversationList [(newTeamConversation ((Id (fromJust (UUID.fromString "00000024-0000-006c-0000-005500000008")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000018-0000-0068-0000-000c00000046")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000005d-0000-004c-0000-005b00000009")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000001d-0000-004f-0000-004a0000001b")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000001d-0000-001a-0000-00470000000a")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000001-0000-0050-0000-00090000007f")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000001a-0000-0016-0000-003f00000054")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000005d-0000-0076-0000-00120000002c")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000028-0000-0067-0000-005300000041")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000028-0000-0055-0000-00760000003b")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000060-0000-0040-0000-004300000004")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000004-0000-0038-0000-00790000005b")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000005a-0000-0080-0000-004a00000074")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000031-0000-004a-0000-001900000002")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000002-0000-0077-0000-007400000008")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000023-0000-0030-0000-00240000005f")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000032-0000-0006-0000-007300000062")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000000e-0000-0006-0000-00430000000b")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000028-0000-0079-0000-00020000003a")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000000c-0000-0050-0000-004b00000018")))) (True))])
testObject_TeamConversationList_team_3 :: TeamConversationList
testObject_TeamConversationList_team_3 = (newTeamConversationList [(newTeamConversation ((Id (fromJust (UUID.fromString "00000068-0000-002d-0000-002700000080")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000005b-0000-001a-0000-004200000077")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000056-0000-000d-0000-006900000009")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000078-0000-0008-0000-002500000044")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000006c-0000-0072-0000-002c00000010")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000013-0000-0001-0000-002d0000007e")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000000b-0000-0075-0000-00190000004c")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000032-0000-0010-0000-007a0000005f")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000045-0000-005b-0000-002e00000073")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000074-0000-0014-0000-002000000040")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000002d-0000-007a-0000-005b00000012")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000004d-0000-0058-0000-005f0000002a")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000006b-0000-0062-0000-006000000064")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000035-0000-000d-0000-00690000005f")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000000e-0000-0068-0000-005e00000051")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000001-0000-006f-0000-001700000079")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000004b-0000-0080-0000-003700000032")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000006c-0000-0014-0000-005700000036")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000000a-0000-0075-0000-001b0000002e")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000077-0000-0042-0000-00000000000f")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000005c-0000-0002-0000-00530000000e")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000005b-0000-0042-0000-005900000027")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000007-0000-003d-0000-004600000016")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000001e-0000-003b-0000-00160000003a")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000005-0000-0015-0000-004b00000039")))) (False))])
testObject_TeamConversationList_team_4 :: TeamConversationList
testObject_TeamConversationList_team_4 = (newTeamConversationList [(newTeamConversation ((Id (fromJust (UUID.fromString "0000004a-0000-0028-0000-007c00000057")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000066-0000-000a-0000-001000000080")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000002d-0000-001f-0000-002100000035")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000002f-0000-0007-0000-005400000076")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000006d-0000-005e-0000-001f0000004f")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000054-0000-0032-0000-000600000061")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000017-0000-0018-0000-004200000001")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000054-0000-0076-0000-004b0000006a")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000052-0000-0001-0000-000000000023")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000000a-0000-006f-0000-001f00000055")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000037-0000-0077-0000-005000000019")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000020-0000-006b-0000-003000000021")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000064-0000-0066-0000-00390000007e")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000048-0000-003e-0000-00380000003c")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000048-0000-001d-0000-006800000048")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000007b-0000-0042-0000-003e00000015")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000003a-0000-006f-0000-006400000073")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000075-0000-002a-0000-001c00000076")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000007e-0000-0052-0000-004a0000004e")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000000d-0000-007f-0000-00120000005f")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000024-0000-0022-0000-006600000042")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000051-0000-0054-0000-007600000051")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000064-0000-006c-0000-005e0000004c")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000004e-0000-005b-0000-006400000014")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000028-0000-005e-0000-001600000035")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000072-0000-001d-0000-000d00000078")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000041-0000-0050-0000-005200000057")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000029-0000-0036-0000-001d00000032")))) (False))])
testObject_TeamConversationList_team_5 :: TeamConversationList
testObject_TeamConversationList_team_5 = (newTeamConversationList [(newTeamConversation ((Id (fromJust (UUID.fromString "0000003d-0000-007a-0000-004000000072")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000036-0000-0046-0000-001800000023")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000035-0000-0011-0000-004000000035")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000002d-0000-0040-0000-006c00000005")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000020-0000-0023-0000-003100000021")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000027-0000-0073-0000-002500000007")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000023-0000-0056-0000-005b0000003a")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000007d-0000-0034-0000-001200000028")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000057-0000-0076-0000-000c00000032")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000018-0000-0055-0000-002600000005")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000031-0000-005d-0000-007100000051")))) (False))])
