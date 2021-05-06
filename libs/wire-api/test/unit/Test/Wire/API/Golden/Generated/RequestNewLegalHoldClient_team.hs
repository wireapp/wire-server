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
testObject_RequestNewLegalHoldClient_team_1 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "0000006c-0000-0016-0000-000e00000042")))) ((Id (fromJust (UUID.fromString "00000000-0000-0010-0000-005300000064")))))
testObject_RequestNewLegalHoldClient_team_2 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_2 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "0000003b-0000-0065-0000-002500000022")))) ((Id (fromJust (UUID.fromString "0000006a-0000-0029-0000-00740000004e")))))
testObject_RequestNewLegalHoldClient_team_3 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_3 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000003-0000-004c-0000-00490000006a")))) ((Id (fromJust (UUID.fromString "00000054-0000-005c-0000-00750000001e")))))
testObject_RequestNewLegalHoldClient_team_4 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_4 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "0000003d-0000-0006-0000-00730000001c")))) ((Id (fromJust (UUID.fromString "00000042-0000-0065-0000-002100000007")))))
testObject_RequestNewLegalHoldClient_team_5 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_5 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "0000001f-0000-001f-0000-00220000004c")))) ((Id (fromJust (UUID.fromString "00000027-0000-003d-0000-00170000001d")))))
testObject_RequestNewLegalHoldClient_team_6 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_6 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000063-0000-002c-0000-003800000068")))) ((Id (fromJust (UUID.fromString "00000041-0000-0012-0000-00740000004c")))))
testObject_RequestNewLegalHoldClient_team_7 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_7 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000078-0000-001f-0000-007000000026")))) ((Id (fromJust (UUID.fromString "0000002b-0000-0044-0000-004700000005")))))
testObject_RequestNewLegalHoldClient_team_8 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_8 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000075-0000-002a-0000-003f00000042")))) ((Id (fromJust (UUID.fromString "00000030-0000-0026-0000-001800000074")))))
testObject_RequestNewLegalHoldClient_team_9 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_9 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000066-0000-0047-0000-003d0000004e")))) ((Id (fromJust (UUID.fromString "00000073-0000-0000-0000-007900000074")))))
testObject_RequestNewLegalHoldClient_team_10 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_10 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000080-0000-0034-0000-005200000049")))) ((Id (fromJust (UUID.fromString "0000002c-0000-004e-0000-005f0000002e")))))
testObject_RequestNewLegalHoldClient_team_11 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_11 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000044-0000-0071-0000-005500000032")))) ((Id (fromJust (UUID.fromString "00000069-0000-006c-0000-000b0000000f")))))
testObject_RequestNewLegalHoldClient_team_12 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_12 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000043-0000-006c-0000-003c00000030")))) ((Id (fromJust (UUID.fromString "0000003f-0000-0057-0000-001200000073")))))
testObject_RequestNewLegalHoldClient_team_13 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_13 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000028-0000-0037-0000-002d00000077")))) ((Id (fromJust (UUID.fromString "00000022-0000-0068-0000-000100000029")))))
testObject_RequestNewLegalHoldClient_team_14 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_14 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "0000005b-0000-007a-0000-007f00000015")))) ((Id (fromJust (UUID.fromString "0000005e-0000-0005-0000-00140000005f")))))
testObject_RequestNewLegalHoldClient_team_15 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_15 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000003-0000-0058-0000-002d00000046")))) ((Id (fromJust (UUID.fromString "00000070-0000-0070-0000-003d0000007c")))))
testObject_RequestNewLegalHoldClient_team_16 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_16 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000076-0000-001f-0000-002b00000029")))) ((Id (fromJust (UUID.fromString "00000051-0000-0007-0000-004e0000003e")))))
testObject_RequestNewLegalHoldClient_team_17 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_17 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000056-0000-007b-0000-002000000059")))) ((Id (fromJust (UUID.fromString "00000056-0000-0040-0000-002a00000050")))))
testObject_RequestNewLegalHoldClient_team_18 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_18 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000021-0000-005b-0000-00420000003a")))) ((Id (fromJust (UUID.fromString "00000017-0000-0051-0000-006300000003")))))
testObject_RequestNewLegalHoldClient_team_19 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_19 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "00000080-0000-0062-0000-004200000002")))) ((Id (fromJust (UUID.fromString "0000004c-0000-004d-0000-006e00000034")))))
testObject_RequestNewLegalHoldClient_team_20 :: RequestNewLegalHoldClient
testObject_RequestNewLegalHoldClient_team_20 = (RequestNewLegalHoldClient ((Id (fromJust (UUID.fromString "0000001f-0000-002d-0000-004b0000007b")))) ((Id (fromJust (UUID.fromString "00000049-0000-003c-0000-00030000000d")))))
