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
testObject_ServiceRef_provider_1 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000026-0000-0032-0000-000000000011"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000021-0000-0077-0000-00730000002e")))}
testObject_ServiceRef_provider_2 :: ServiceRef
testObject_ServiceRef_provider_2 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "0000006a-0000-0020-0000-002600000077"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "0000003a-0000-005d-0000-000000000024")))}
testObject_ServiceRef_provider_3 :: ServiceRef
testObject_ServiceRef_provider_3 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000074-0000-0006-0000-00200000006a"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000067-0000-0080-0000-006700000058")))}
testObject_ServiceRef_provider_4 :: ServiceRef
testObject_ServiceRef_provider_4 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000039-0000-002d-0000-000b00000071"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000054-0000-003d-0000-00030000006b")))}
testObject_ServiceRef_provider_5 :: ServiceRef
testObject_ServiceRef_provider_5 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000035-0000-0022-0000-004f0000003d"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000050-0000-0030-0000-003a0000003f")))}
testObject_ServiceRef_provider_6 :: ServiceRef
testObject_ServiceRef_provider_6 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000008-0000-004d-0000-005300000019"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000033-0000-0019-0000-00740000002d")))}
testObject_ServiceRef_provider_7 :: ServiceRef
testObject_ServiceRef_provider_7 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "0000000d-0000-0073-0000-005800000058"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000016-0000-0022-0000-005200000072")))}
testObject_ServiceRef_provider_8 :: ServiceRef
testObject_ServiceRef_provider_8 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000025-0000-0020-0000-006900000077"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000064-0000-0024-0000-000500000064")))}
testObject_ServiceRef_provider_9 :: ServiceRef
testObject_ServiceRef_provider_9 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "0000004d-0000-0075-0000-007500000031"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000064-0000-003e-0000-005e00000016")))}
testObject_ServiceRef_provider_10 :: ServiceRef
testObject_ServiceRef_provider_10 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000056-0000-0011-0000-004e00000046"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0010-0000-001c0000002e")))}
testObject_ServiceRef_provider_11 :: ServiceRef
testObject_ServiceRef_provider_11 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000079-0000-002c-0000-001a00000043"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000054-0000-0006-0000-004d00000071")))}
testObject_ServiceRef_provider_12 :: ServiceRef
testObject_ServiceRef_provider_12 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000003-0000-0011-0000-007e00000036"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "0000006c-0000-0018-0000-003d00000007")))}
testObject_ServiceRef_provider_13 :: ServiceRef
testObject_ServiceRef_provider_13 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "0000002e-0000-0039-0000-00490000000d"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "0000006c-0000-0050-0000-000a00000003")))}
testObject_ServiceRef_provider_14 :: ServiceRef
testObject_ServiceRef_provider_14 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "0000004c-0000-0040-0000-006500000075"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000017-0000-0032-0000-006d0000006a")))}
testObject_ServiceRef_provider_15 :: ServiceRef
testObject_ServiceRef_provider_15 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000076-0000-0047-0000-000100000080"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "0000004f-0000-0046-0000-006000000077")))}
testObject_ServiceRef_provider_16 :: ServiceRef
testObject_ServiceRef_provider_16 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "0000007f-0000-007c-0000-00130000001c"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000070-0000-001c-0000-001a00000052")))}
testObject_ServiceRef_provider_17 :: ServiceRef
testObject_ServiceRef_provider_17 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "0000005b-0000-0002-0000-001d00000024"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "0000004c-0000-000d-0000-00370000004c")))}
testObject_ServiceRef_provider_18 :: ServiceRef
testObject_ServiceRef_provider_18 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "0000007c-0000-0063-0000-00230000006d"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "0000003a-0000-005c-0000-00700000001f")))}
testObject_ServiceRef_provider_19 :: ServiceRef
testObject_ServiceRef_provider_19 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000007-0000-0027-0000-004600000041"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "0000007f-0000-004c-0000-002d0000007a")))}
testObject_ServiceRef_provider_20 :: ServiceRef
testObject_ServiceRef_provider_20 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000052-0000-0077-0000-00060000003e"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "0000006e-0000-0007-0000-006a00000024")))}
