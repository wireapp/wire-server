{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ServiceRef_provider where

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
import Wire.API.Conversation.Member
import Wire.API.Conversation.Role
import Wire.API.Provider
import Wire.API.Provider.Bot
import Wire.API.Provider.External
import Wire.API.Provider.Service
import Wire.API.Provider.Service.Tag
import Wire.API.User.Client.Prekey
import Wire.API.User.Identity
import Wire.API.User.Profile
testObject_ServiceRef_provider_1 :: ServiceRef
testObject_ServiceRef_provider_1 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000010-0000-0057-0000-003d00000036"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000035-0000-0038-0000-002a00000038")))}
testObject_ServiceRef_provider_2 :: ServiceRef
testObject_ServiceRef_provider_2 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "0000002e-0000-0004-0000-005100000045"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000035-0000-0027-0000-00730000006b")))}
testObject_ServiceRef_provider_3 :: ServiceRef
testObject_ServiceRef_provider_3 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000055-0000-0060-0000-007e0000005a"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "0000000d-0000-0036-0000-005300000062")))}
testObject_ServiceRef_provider_4 :: ServiceRef
testObject_ServiceRef_provider_4 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "0000003d-0000-003e-0000-001500000043"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000033-0000-0063-0000-005a00000022")))}
testObject_ServiceRef_provider_5 :: ServiceRef
testObject_ServiceRef_provider_5 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000056-0000-004a-0000-006d0000001a"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000036-0000-007b-0000-002e0000004c")))}
testObject_ServiceRef_provider_6 :: ServiceRef
testObject_ServiceRef_provider_6 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000057-0000-003e-0000-00710000007b"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000036-0000-0020-0000-005f0000007a")))}
testObject_ServiceRef_provider_7 :: ServiceRef
testObject_ServiceRef_provider_7 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000074-0000-002a-0000-00620000003c"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000076-0000-007b-0000-00270000000e")))}
testObject_ServiceRef_provider_8 :: ServiceRef
testObject_ServiceRef_provider_8 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000031-0000-0033-0000-004900000026"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "0000004c-0000-005a-0000-001d00000054")))}
testObject_ServiceRef_provider_9 :: ServiceRef
testObject_ServiceRef_provider_9 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "0000006e-0000-006c-0000-00270000005f"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "0000002d-0000-0055-0000-001e0000000c")))}
testObject_ServiceRef_provider_10 :: ServiceRef
testObject_ServiceRef_provider_10 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000051-0000-003d-0000-00190000003b"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000058-0000-0068-0000-004900000005")))}
testObject_ServiceRef_provider_11 :: ServiceRef
testObject_ServiceRef_provider_11 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000042-0000-0072-0000-004300000058"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000072-0000-004a-0000-00200000007a")))}
testObject_ServiceRef_provider_12 :: ServiceRef
testObject_ServiceRef_provider_12 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "0000002d-0000-0041-0000-00160000001a"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000030-0000-0028-0000-002f00000004")))}
testObject_ServiceRef_provider_13 :: ServiceRef
testObject_ServiceRef_provider_13 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "0000007b-0000-003d-0000-002b00000033"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000032-0000-0079-0000-003500000069")))}
testObject_ServiceRef_provider_14 :: ServiceRef
testObject_ServiceRef_provider_14 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "0000007a-0000-0060-0000-006a0000000e"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "0000000d-0000-0021-0000-000c00000021")))}
testObject_ServiceRef_provider_15 :: ServiceRef
testObject_ServiceRef_provider_15 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "0000004f-0000-000c-0000-007200000055"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000029-0000-007f-0000-00800000000e")))}
testObject_ServiceRef_provider_16 :: ServiceRef
testObject_ServiceRef_provider_16 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "0000003c-0000-0079-0000-006b00000076"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000032-0000-0063-0000-005300000076")))}
testObject_ServiceRef_provider_17 :: ServiceRef
testObject_ServiceRef_provider_17 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "0000004d-0000-0015-0000-000c0000004c"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000031-0000-0010-0000-006f00000025")))}
testObject_ServiceRef_provider_18 :: ServiceRef
testObject_ServiceRef_provider_18 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000078-0000-0002-0000-005f00000009"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000039-0000-0061-0000-00760000000d")))}
testObject_ServiceRef_provider_19 :: ServiceRef
testObject_ServiceRef_provider_19 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-001e-0000-002200000050"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000007-0000-0012-0000-000e00000037")))}
testObject_ServiceRef_provider_20 :: ServiceRef
testObject_ServiceRef_provider_20 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "0000003e-0000-005e-0000-007300000037"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000079-0000-005e-0000-001200000059")))}
