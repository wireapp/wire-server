{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.LegalHoldServiceRemove_team where

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
testObject_LegalHoldServiceRemove_team_1 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_1 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000064-0000-0020-0000-003a0000005a"))), lhrTeamId = (Id (fromJust (UUID.fromString "0000003b-0000-0069-0000-007700000074")))}
testObject_LegalHoldServiceRemove_team_2 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_2 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000003-0000-0041-0000-001e00000042"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000027-0000-0076-0000-000100000016")))}
testObject_LegalHoldServiceRemove_team_3 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_3 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000017-0000-0054-0000-001800000026"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000018-0000-002d-0000-00090000006d")))}
testObject_LegalHoldServiceRemove_team_4 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_4 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "0000005d-0000-0023-0000-006d00000014"))), lhrTeamId = (Id (fromJust (UUID.fromString "0000003f-0000-0059-0000-003200000013")))}
testObject_LegalHoldServiceRemove_team_5 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_5 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000052-0000-0044-0000-00060000003d"))), lhrTeamId = (Id (fromJust (UUID.fromString "0000003e-0000-002c-0000-004e00000014")))}
testObject_LegalHoldServiceRemove_team_6 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_6 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000042-0000-003d-0000-00210000004e"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000066-0000-0004-0000-006c0000003a")))}
testObject_LegalHoldServiceRemove_team_7 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_7 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "0000004b-0000-006c-0000-004f00000006"))), lhrTeamId = (Id (fromJust (UUID.fromString "0000000e-0000-000f-0000-004400000060")))}
testObject_LegalHoldServiceRemove_team_8 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_8 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000045-0000-0022-0000-000700000064"))), lhrTeamId = (Id (fromJust (UUID.fromString "0000003d-0000-0006-0000-00080000002f")))}
testObject_LegalHoldServiceRemove_team_9 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_9 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "0000002d-0000-0042-0000-00660000001a"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000014-0000-0079-0000-004d00000021")))}
testObject_LegalHoldServiceRemove_team_10 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_10 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000038-0000-003d-0000-006200000024"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000013-0000-004b-0000-005300000011")))}
testObject_LegalHoldServiceRemove_team_11 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_11 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000b00000048"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000019-0000-0065-0000-005200000012")))}
testObject_LegalHoldServiceRemove_team_12 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_12 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000014-0000-0074-0000-002d00000053"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000071-0000-0023-0000-005a00000069")))}
testObject_LegalHoldServiceRemove_team_13 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_13 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "0000004d-0000-0039-0000-005b00000064"))), lhrTeamId = (Id (fromJust (UUID.fromString "0000006a-0000-0074-0000-005c0000007e")))}
testObject_LegalHoldServiceRemove_team_14 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_14 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000047-0000-0074-0000-007a0000002b"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000011-0000-0024-0000-003500000053")))}
testObject_LegalHoldServiceRemove_team_15 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_15 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "0000007a-0000-0074-0000-001d00000045"))), lhrTeamId = (Id (fromJust (UUID.fromString "0000001c-0000-003b-0000-007800000054")))}
testObject_LegalHoldServiceRemove_team_16 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_16 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000034-0000-001a-0000-007800000029"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000009-0000-001f-0000-007700000037")))}
testObject_LegalHoldServiceRemove_team_17 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_17 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "0000003d-0000-0055-0000-004100000002"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000011-0000-0057-0000-005100000037")))}
testObject_LegalHoldServiceRemove_team_18 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_18 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "0000007c-0000-0019-0000-006500000054"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0071-0000-00580000000c")))}
testObject_LegalHoldServiceRemove_team_19 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_19 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000075-0000-0067-0000-001b00000007"))), lhrTeamId = (Id (fromJust (UUID.fromString "0000002f-0000-002d-0000-00380000007e")))}
testObject_LegalHoldServiceRemove_team_20 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_20 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "0000003c-0000-002e-0000-006d0000002d"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000020-0000-0008-0000-004000000032")))}
