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
testObject_ServiceRef_provider_1 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000043-0000-0060-0000-003d0000000e"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "0000005b-0000-007e-0000-006b00000011")))}
testObject_ServiceRef_provider_2 :: ServiceRef
testObject_ServiceRef_provider_2 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000004-0000-0051-0000-00410000002c"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000059-0000-0006-0000-002b0000004f")))}
testObject_ServiceRef_provider_3 :: ServiceRef
testObject_ServiceRef_provider_3 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "0000005d-0000-002c-0000-007d00000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000016-0000-0032-0000-003800000044")))}
testObject_ServiceRef_provider_4 :: ServiceRef
testObject_ServiceRef_provider_4 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000068-0000-0037-0000-004f0000006a"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "0000005a-0000-0005-0000-007100000049")))}
testObject_ServiceRef_provider_5 :: ServiceRef
testObject_ServiceRef_provider_5 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000046-0000-004a-0000-00250000004f"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000061-0000-001d-0000-00510000007e")))}
testObject_ServiceRef_provider_6 :: ServiceRef
testObject_ServiceRef_provider_6 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000037-0000-002b-0000-002700000050"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "0000007b-0000-0047-0000-007100000051")))}
testObject_ServiceRef_provider_7 :: ServiceRef
testObject_ServiceRef_provider_7 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000049-0000-003b-0000-005800000017"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "0000003b-0000-0027-0000-000a00000067")))}
testObject_ServiceRef_provider_8 :: ServiceRef
testObject_ServiceRef_provider_8 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000021-0000-0055-0000-003000000077"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "0000007a-0000-0047-0000-004000000044")))}
testObject_ServiceRef_provider_9 :: ServiceRef
testObject_ServiceRef_provider_9 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000021-0000-000a-0000-008000000071"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000034-0000-003f-0000-005f00000042")))}
testObject_ServiceRef_provider_10 :: ServiceRef
testObject_ServiceRef_provider_10 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000016-0000-0075-0000-001b00000012"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000032-0000-0054-0000-004600000059")))}
testObject_ServiceRef_provider_11 :: ServiceRef
testObject_ServiceRef_provider_11 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000024-0000-0033-0000-000700000007"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0071-0000-00400000006b")))}
testObject_ServiceRef_provider_12 :: ServiceRef
testObject_ServiceRef_provider_12 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000047-0000-0023-0000-00460000005e"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000034-0000-001b-0000-001300000044")))}
testObject_ServiceRef_provider_13 :: ServiceRef
testObject_ServiceRef_provider_13 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000059-0000-0014-0000-000e0000004f"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0037-0000-00690000000f")))}
testObject_ServiceRef_provider_14 :: ServiceRef
testObject_ServiceRef_provider_14 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000049-0000-002c-0000-006f00000035"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0005-0000-007c00000007")))}
testObject_ServiceRef_provider_15 :: ServiceRef
testObject_ServiceRef_provider_15 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000028-0000-006d-0000-000300000021"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0055-0000-000400000055")))}
testObject_ServiceRef_provider_16 :: ServiceRef
testObject_ServiceRef_provider_16 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000043-0000-001c-0000-003d0000004c"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000073-0000-0080-0000-001800000029")))}
testObject_ServiceRef_provider_17 :: ServiceRef
testObject_ServiceRef_provider_17 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000021-0000-007b-0000-00620000005b"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "0000002e-0000-0067-0000-001a00000013")))}
testObject_ServiceRef_provider_18 :: ServiceRef
testObject_ServiceRef_provider_18 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000043-0000-0027-0000-006300000061"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "0000001a-0000-0046-0000-005b0000003f")))}
testObject_ServiceRef_provider_19 :: ServiceRef
testObject_ServiceRef_provider_19 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000026-0000-005c-0000-004600000064"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000043-0000-006c-0000-002d00000014")))}
testObject_ServiceRef_provider_20 :: ServiceRef
testObject_ServiceRef_provider_20 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000019-0000-0011-0000-007800000075"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000035-0000-0074-0000-00260000005b")))}
