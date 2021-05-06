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
testObject_LegalHoldServiceRemove_team_1 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "0000000d-0000-000e-0000-005300000031"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000065-0000-000a-0000-000800000071")))}
testObject_LegalHoldServiceRemove_team_2 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_2 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "0000006d-0000-0055-0000-002200000020"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000007-0000-000b-0000-002800000039")))}
testObject_LegalHoldServiceRemove_team_3 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_3 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000012-0000-0003-0000-00760000001e"))), lhrTeamId = (Id (fromJust (UUID.fromString "0000001c-0000-000c-0000-007300000048")))}
testObject_LegalHoldServiceRemove_team_4 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_4 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "0000002a-0000-001a-0000-001400000010"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000080-0000-0009-0000-006800000014")))}
testObject_LegalHoldServiceRemove_team_5 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_5 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000032-0000-0038-0000-004600000061"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000066-0000-006c-0000-003700000061")))}
testObject_LegalHoldServiceRemove_team_6 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_6 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000005-0000-0028-0000-001000000073"))), lhrTeamId = (Id (fromJust (UUID.fromString "0000006f-0000-0069-0000-000200000059")))}
testObject_LegalHoldServiceRemove_team_7 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_7 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000008-0000-0078-0000-000e0000005c"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000036-0000-0069-0000-007d00000031")))}
testObject_LegalHoldServiceRemove_team_8 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_8 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000009-0000-002e-0000-00540000003a"))), lhrTeamId = (Id (fromJust (UUID.fromString "0000002e-0000-005b-0000-001a00000004")))}
testObject_LegalHoldServiceRemove_team_9 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_9 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "0000007e-0000-0012-0000-005f00000025"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000071-0000-0024-0000-002200000063")))}
testObject_LegalHoldServiceRemove_team_10 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_10 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "0000002f-0000-003d-0000-005d0000001d"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000028-0000-0064-0000-005600000066")))}
testObject_LegalHoldServiceRemove_team_11 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_11 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "0000000c-0000-0018-0000-003100000065"))), lhrTeamId = (Id (fromJust (UUID.fromString "0000001a-0000-0016-0000-00160000003e")))}
testObject_LegalHoldServiceRemove_team_12 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_12 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000031-0000-0040-0000-00310000001a"))), lhrTeamId = (Id (fromJust (UUID.fromString "0000003c-0000-005a-0000-00510000000b")))}
testObject_LegalHoldServiceRemove_team_13 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_13 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000027-0000-0076-0000-001f00000067"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000034-0000-0002-0000-003700000007")))}
testObject_LegalHoldServiceRemove_team_14 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_14 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "0000005b-0000-0019-0000-00220000001f"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000017-0000-007f-0000-004100000047")))}
testObject_LegalHoldServiceRemove_team_15 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_15 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000048-0000-0077-0000-002b00000013"))), lhrTeamId = (Id (fromJust (UUID.fromString "0000000b-0000-0011-0000-005d0000006c")))}
testObject_LegalHoldServiceRemove_team_16 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_16 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000062-0000-0055-0000-00720000005d"))), lhrTeamId = (Id (fromJust (UUID.fromString "0000005f-0000-0014-0000-000e00000043")))}
testObject_LegalHoldServiceRemove_team_17 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_17 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000060-0000-0004-0000-003600000041"))), lhrTeamId = (Id (fromJust (UUID.fromString "0000000e-0000-001c-0000-006f0000005a")))}
testObject_LegalHoldServiceRemove_team_18 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_18 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000037-0000-0011-0000-007300000048"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000030-0000-0052-0000-007000000022")))}
testObject_LegalHoldServiceRemove_team_19 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_19 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000071-0000-006c-0000-002600000043"))), lhrTeamId = (Id (fromJust (UUID.fromString "0000004b-0000-0035-0000-004f00000007")))}
testObject_LegalHoldServiceRemove_team_20 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_20 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000026-0000-0033-0000-002100000050"))), lhrTeamId = (Id (fromJust (UUID.fromString "0000006d-0000-003b-0000-002700000036")))}
