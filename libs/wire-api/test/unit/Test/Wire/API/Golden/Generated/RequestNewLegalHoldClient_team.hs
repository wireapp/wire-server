{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.RequestNewLegalHoldClient_team where

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
testObject_RequestNewLegalHoldClient_team_1 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_1 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000071-0000-0040-0000-004500000031")))) ((Id (fromJust (UUID.fromString "0000004c-0000-0027-0000-007500000011")))))
testObject_RequestNewLegalHoldClient_team_2 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_2 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000033-0000-001b-0000-006c0000002b")))) ((Id (fromJust (UUID.fromString "00000068-0000-001b-0000-006800000003")))))
testObject_RequestNewLegalHoldClient_team_3 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_3 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000033-0000-004c-0000-005f00000061")))) ((Id (fromJust (UUID.fromString "00000075-0000-0016-0000-000a00000066")))))
testObject_RequestNewLegalHoldClient_team_4 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_4 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000059-0000-007e-0000-001500000042")))) ((Id (fromJust (UUID.fromString "00000064-0000-0072-0000-001f00000064")))))
testObject_RequestNewLegalHoldClient_team_5 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_5 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "0000005d-0000-0073-0000-002100000063")))) ((Id (fromJust (UUID.fromString "00000042-0000-0003-0000-000d0000007e")))))
testObject_RequestNewLegalHoldClient_team_6 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_6 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "0000003f-0000-007f-0000-003500000018")))) ((Id (fromJust (UUID.fromString "00000018-0000-0041-0000-001c00000076")))))
testObject_RequestNewLegalHoldClient_team_7 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_7 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "0000003a-0000-003d-0000-008000000080")))) ((Id (fromJust (UUID.fromString "00000035-0000-0065-0000-004300000010")))))
testObject_RequestNewLegalHoldClient_team_8 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_8 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "0000003b-0000-0016-0000-002500000020")))) ((Id (fromJust (UUID.fromString "00000005-0000-0019-0000-00190000003f")))))
testObject_RequestNewLegalHoldClient_team_9 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_9 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000022-0000-001b-0000-00260000004b")))) ((Id (fromJust (UUID.fromString "00000039-0000-0055-0000-007a00000036")))))
testObject_RequestNewLegalHoldClient_team_10 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_10 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000012-0000-001c-0000-001000000028")))) ((Id (fromJust (UUID.fromString "0000006e-0000-0045-0000-004e00000027")))))
testObject_RequestNewLegalHoldClient_team_11 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_11 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000069-0000-0060-0000-000f00000071")))) ((Id (fromJust (UUID.fromString "0000006d-0000-0048-0000-002b00000040")))))
testObject_RequestNewLegalHoldClient_team_12 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_12 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "0000000f-0000-0034-0000-00530000007e")))) ((Id (fromJust (UUID.fromString "00000027-0000-005e-0000-002800000063")))))
testObject_RequestNewLegalHoldClient_team_13 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_13 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000058-0000-0070-0000-007c00000068")))) ((Id (fromJust (UUID.fromString "00000080-0000-007f-0000-006b00000074")))))
testObject_RequestNewLegalHoldClient_team_14 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_14 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "0000005f-0000-0034-0000-00030000000e")))) ((Id (fromJust (UUID.fromString "0000001a-0000-0043-0000-00710000000e")))))
testObject_RequestNewLegalHoldClient_team_15 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_15 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "0000006b-0000-0018-0000-006900000019")))) ((Id (fromJust (UUID.fromString "00000019-0000-000e-0000-002c00000057")))))
testObject_RequestNewLegalHoldClient_team_16 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_16 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "0000001f-0000-0049-0000-006b0000002c")))) ((Id (fromJust (UUID.fromString "0000007b-0000-001c-0000-00530000006e")))))
testObject_RequestNewLegalHoldClient_team_17 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_17 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000030-0000-005b-0000-002400000057")))) ((Id (fromJust (UUID.fromString "0000003b-0000-002c-0000-005c0000000b")))))
testObject_RequestNewLegalHoldClient_team_18 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_18 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000007-0000-0032-0000-006b0000000e")))) ((Id (fromJust (UUID.fromString "0000005a-0000-001c-0000-002300000036")))))
testObject_RequestNewLegalHoldClient_team_19 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_19 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "0000003e-0000-0012-0000-00540000000f")))) ((Id (fromJust (UUID.fromString "0000004d-0000-0053-0000-006a0000002b")))))
testObject_RequestNewLegalHoldClient_team_20 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_20 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000017-0000-0061-0000-004d00000035")))) ((Id (fromJust (UUID.fromString "00000029-0000-0010-0000-003d00000077")))))
