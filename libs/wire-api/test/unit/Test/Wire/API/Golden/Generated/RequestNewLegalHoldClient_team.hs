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
testObject_RequestNewLegalHoldClient_team_1 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000059-0000-0024-0000-002800000012")))) ((Id (fromJust (UUID.fromString "00000069-0000-007d-0000-003700000072")))))
testObject_RequestNewLegalHoldClient_team_2 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_2 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "0000003e-0000-006f-0000-001700000042")))) ((Id (fromJust (UUID.fromString "00000078-0000-000e-0000-000900000038")))))
testObject_RequestNewLegalHoldClient_team_3 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_3 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000027-0000-0000-0000-003600000026")))) ((Id (fromJust (UUID.fromString "0000006b-0000-003c-0000-002d0000006e")))))
testObject_RequestNewLegalHoldClient_team_4 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_4 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000075-0000-0000-0000-004e0000007f")))) ((Id (fromJust (UUID.fromString "00000060-0000-005d-0000-00700000006e")))))
testObject_RequestNewLegalHoldClient_team_5 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_5 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "0000003f-0000-0034-0000-00320000006e")))) ((Id (fromJust (UUID.fromString "0000006f-0000-0058-0000-00290000000a")))))
testObject_RequestNewLegalHoldClient_team_6 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_6 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "0000003c-0000-0034-0000-005a0000003f")))) ((Id (fromJust (UUID.fromString "00000053-0000-0069-0000-00630000005b")))))
testObject_RequestNewLegalHoldClient_team_7 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_7 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000048-0000-002d-0000-007200000011")))) ((Id (fromJust (UUID.fromString "0000006b-0000-0063-0000-001b00000064")))))
testObject_RequestNewLegalHoldClient_team_8 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_8 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000036-0000-007b-0000-001c0000001c")))) ((Id (fromJust (UUID.fromString "0000003f-0000-007c-0000-001b00000014")))))
testObject_RequestNewLegalHoldClient_team_9 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_9 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000048-0000-0002-0000-00650000003d")))) ((Id (fromJust (UUID.fromString "00000000-0000-0036-0000-00460000007d")))))
testObject_RequestNewLegalHoldClient_team_10 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_10 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "0000006d-0000-0051-0000-003400000008")))) ((Id (fromJust (UUID.fromString "00000079-0000-003e-0000-00340000005d")))))
testObject_RequestNewLegalHoldClient_team_11 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_11 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000026-0000-0072-0000-007d00000020")))) ((Id (fromJust (UUID.fromString "0000002c-0000-007d-0000-005400000043")))))
testObject_RequestNewLegalHoldClient_team_12 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_12 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000059-0000-0043-0000-00140000004d")))) ((Id (fromJust (UUID.fromString "00000078-0000-000b-0000-007a00000069")))))
testObject_RequestNewLegalHoldClient_team_13 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_13 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000029-0000-0041-0000-007200000077")))) ((Id (fromJust (UUID.fromString "00000015-0000-0008-0000-000f00000067")))))
testObject_RequestNewLegalHoldClient_team_14 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_14 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000015-0000-0003-0000-007100000011")))) ((Id (fromJust (UUID.fromString "0000001a-0000-0004-0000-007a00000010")))))
testObject_RequestNewLegalHoldClient_team_15 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_15 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000067-0000-002a-0000-003600000003")))) ((Id (fromJust (UUID.fromString "0000000d-0000-003a-0000-00550000005c")))))
testObject_RequestNewLegalHoldClient_team_16 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_16 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000078-0000-0025-0000-00660000003c")))) ((Id (fromJust (UUID.fromString "0000001e-0000-000f-0000-005f00000072")))))
testObject_RequestNewLegalHoldClient_team_17 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_17 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000051-0000-0004-0000-00060000001a")))) ((Id (fromJust (UUID.fromString "0000005a-0000-0075-0000-001d00000054")))))
testObject_RequestNewLegalHoldClient_team_18 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_18 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "0000006a-0000-0030-0000-00430000003a")))) ((Id (fromJust (UUID.fromString "00000004-0000-0026-0000-00640000006c")))))
testObject_RequestNewLegalHoldClient_team_19 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_19 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000054-0000-0048-0000-002d00000023")))) ((Id (fromJust (UUID.fromString "0000003a-0000-007a-0000-006200000010")))))
testObject_RequestNewLegalHoldClient_team_20 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_20 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000022-0000-0050-0000-00190000003a")))) ((Id (fromJust (UUID.fromString "00000058-0000-001d-0000-00270000007c")))))
