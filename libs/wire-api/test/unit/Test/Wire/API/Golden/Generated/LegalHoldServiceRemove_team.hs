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
testObject_LegalHoldServiceRemove_team_1 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000071-0000-002e-0000-003b00000056"))), lhrTeamId = (Id (fromJust (UUID.fromString "0000007f-0000-0012-0000-00350000000b")))}
testObject_LegalHoldServiceRemove_team_2 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_2 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "0000001a-0000-0066-0000-00120000003f"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000070-0000-0063-0000-00190000006b")))}
testObject_LegalHoldServiceRemove_team_3 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_3 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000073-0000-0055-0000-00730000007f"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000011-0000-0009-0000-002300000057")))}
testObject_LegalHoldServiceRemove_team_4 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_4 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "0000002f-0000-0054-0000-005e0000005e"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000022-0000-005a-0000-000800000009")))}
testObject_LegalHoldServiceRemove_team_5 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_5 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "0000001f-0000-006c-0000-00420000004b"))), lhrTeamId = (Id (fromJust (UUID.fromString "0000007b-0000-007a-0000-00770000002d")))}
testObject_LegalHoldServiceRemove_team_6 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_6 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000056-0000-005d-0000-003e00000023"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000042-0000-004a-0000-001c00000026")))}
testObject_LegalHoldServiceRemove_team_7 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_7 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000056-0000-0033-0000-005700000064"))), lhrTeamId = (Id (fromJust (UUID.fromString "0000004a-0000-001d-0000-00660000003c")))}
testObject_LegalHoldServiceRemove_team_8 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_8 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "0000001d-0000-002d-0000-006700000033"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000033-0000-006a-0000-004800000028")))}
testObject_LegalHoldServiceRemove_team_9 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_9 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000070-0000-0008-0000-005c00000051"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000039-0000-0059-0000-000e0000004c")))}
testObject_LegalHoldServiceRemove_team_10 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_10 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000029-0000-003c-0000-002f0000007d"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000055-0000-0069-0000-00550000005a")))}
testObject_LegalHoldServiceRemove_team_11 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_11 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000028-0000-0054-0000-001a00000026"))), lhrTeamId = (Id (fromJust (UUID.fromString "0000000e-0000-0028-0000-004100000039")))}
testObject_LegalHoldServiceRemove_team_12 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_12 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000040-0000-0037-0000-004300000065"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000064-0000-0078-0000-00480000003d")))}
testObject_LegalHoldServiceRemove_team_13 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_13 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000061-0000-0026-0000-00140000000b"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000074-0000-006e-0000-000700000006")))}
testObject_LegalHoldServiceRemove_team_14 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_14 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000059-0000-0019-0000-00460000000c"))), lhrTeamId = (Id (fromJust (UUID.fromString "0000000e-0000-0068-0000-004400000072")))}
testObject_LegalHoldServiceRemove_team_15 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_15 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000024-0000-0033-0000-007c00000023"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0073-0000-007200000047")))}
testObject_LegalHoldServiceRemove_team_16 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_16 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000009-0000-004c-0000-00640000001a"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000019-0000-0007-0000-007d00000030")))}
testObject_LegalHoldServiceRemove_team_17 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_17 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000072-0000-0002-0000-006200000007"))), lhrTeamId = (Id (fromJust (UUID.fromString "0000007f-0000-0045-0000-000100000076")))}
testObject_LegalHoldServiceRemove_team_18 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_18 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000031-0000-0077-0000-00490000001c"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000064-0000-0037-0000-001000000064")))}
testObject_LegalHoldServiceRemove_team_19 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_19 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "0000000c-0000-0055-0000-00540000002f"))), lhrTeamId = (Id (fromJust (UUID.fromString "00000080-0000-007a-0000-004a00000003")))}
testObject_LegalHoldServiceRemove_team_20 :: LegalHoldServiceRemove
testObject_LegalHoldServiceRemove_team_20 = LegalHoldServiceRemove {lhrUserId = (Id (fromJust (UUID.fromString "00000023-0000-004f-0000-004300000056"))), lhrTeamId = (Id (fromJust (UUID.fromString "0000007b-0000-0049-0000-00470000007f")))}
