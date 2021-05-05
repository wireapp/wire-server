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
testObject_TeamConversationList_1 :: TeamConversationList
testObject_TeamConversationList_1 = (newTeamConversationList [(newTeamConversation ((Id (fromJust (UUID.fromString "00000045-0000-0042-0000-003c0000001b")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000000e-0000-0054-0000-006200000075")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000014-0000-0018-0000-00100000007a")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000069-0000-006e-0000-004a0000002e")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000001c-0000-0047-0000-007e00000054")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000002-0000-007b-0000-006200000040")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000066-0000-0075-0000-001600000020")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000045-0000-005f-0000-00790000007f")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000078-0000-0016-0000-003700000052")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000006b-0000-0079-0000-005200000000")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000004b-0000-003e-0000-00220000007d")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000007-0000-0039-0000-001100000073")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000005c-0000-0026-0000-003000000006")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000055-0000-0032-0000-001f00000044")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000037-0000-0011-0000-004500000027")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000004f-0000-0053-0000-003c00000075")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000000b-0000-003e-0000-002900000003")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000006d-0000-0065-0000-001d0000005a")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000024-0000-006b-0000-005500000012")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000030-0000-0048-0000-003b0000005b")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000007c-0000-0067-0000-004c0000004c")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000003b-0000-0052-0000-00720000005b")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000007c-0000-0055-0000-00290000001e")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000035-0000-002f-0000-000700000008")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000001d-0000-0065-0000-006a0000001e")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000068-0000-0061-0000-005800000017")))) (True))])
testObject_TeamConversationList_2 :: TeamConversationList
testObject_TeamConversationList_2 = (newTeamConversationList [])
testObject_TeamConversationList_3 :: TeamConversationList
testObject_TeamConversationList_3 = (newTeamConversationList [(newTeamConversation ((Id (fromJust (UUID.fromString "00000040-0000-0010-0000-004000000032")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000059-0000-003f-0000-006e0000003a")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000065-0000-0015-0000-004500000076")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000058-0000-001c-0000-003000000001")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000045-0000-0043-0000-006100000038")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000019-0000-003d-0000-00010000007c")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000072-0000-005c-0000-00320000004c")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000039-0000-0061-0000-002c0000002e")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000002d-0000-004d-0000-007c00000055")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000010-0000-0056-0000-007000000058")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000006e-0000-0036-0000-005300000064")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000068-0000-0038-0000-006a00000038")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000046-0000-004d-0000-00600000004e")))) (False))])
testObject_TeamConversationList_4 :: TeamConversationList
testObject_TeamConversationList_4 = (newTeamConversationList [(newTeamConversation ((Id (fromJust (UUID.fromString "00000002-0000-006b-0000-005200000078")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000075-0000-006f-0000-000c0000005e")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000051-0000-006e-0000-000f00000003")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000052-0000-0006-0000-000c00000004")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000049-0000-001a-0000-003a00000001")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000078-0000-003c-0000-00070000006d")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000006f-0000-005e-0000-002b00000062")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000035-0000-0059-0000-003000000053")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000007f-0000-0074-0000-001300000027")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000039-0000-0006-0000-001400000052")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000056-0000-0012-0000-00450000004a")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000000c-0000-004c-0000-00060000001a")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000077-0000-005e-0000-00020000004d")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000006-0000-007e-0000-005b0000007d")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000003e-0000-0003-0000-007b00000021")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000009-0000-0063-0000-001300000058")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000053-0000-0014-0000-002d00000034")))) (False))])
testObject_TeamConversationList_5 :: TeamConversationList
testObject_TeamConversationList_5 = (newTeamConversationList [(newTeamConversation ((Id (fromJust (UUID.fromString "0000004b-0000-000d-0000-005b00000048")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000000c-0000-000b-0000-006d00000059")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000037-0000-0032-0000-006600000007")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000036-0000-0027-0000-002a00000049")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000007a-0000-0072-0000-004300000055")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000004b-0000-0043-0000-00620000004a")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000005-0000-002e-0000-00220000004a")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000006e-0000-0068-0000-00120000002f")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000002b-0000-0069-0000-001500000016")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000007c-0000-003f-0000-00590000006a")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000002e-0000-0059-0000-000400000046")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000003d-0000-0025-0000-004400000039")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000032-0000-003e-0000-001d0000004f")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000001-0000-001d-0000-006100000013")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000004d-0000-004f-0000-00370000000d")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000000c-0000-007e-0000-004400000042")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000020-0000-006b-0000-002a0000006f")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000059-0000-0039-0000-000800000064")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000027-0000-0059-0000-006e00000037")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000080-0000-005b-0000-00170000002f")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000069-0000-0054-0000-004d0000001d")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000003d-0000-007a-0000-000d00000078")))) (False)),(newTeamConversation ((Id (fromJust (UUID.fromString "00000053-0000-0055-0000-007000000064")))) (True)),(newTeamConversation ((Id (fromJust (UUID.fromString "0000000b-0000-0070-0000-006a0000005d")))) (True))])
