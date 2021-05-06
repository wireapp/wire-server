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
testObject_ServiceRef_provider_1 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "0000006c-0000-0054-0000-004500000021"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "0000004a-0000-000a-0000-006c00000024")))}
testObject_ServiceRef_provider_2 :: ServiceRef
testObject_ServiceRef_provider_2 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000065-0000-0004-0000-002b00000005"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000012-0000-004e-0000-004400000024")))}
testObject_ServiceRef_provider_3 :: ServiceRef
testObject_ServiceRef_provider_3 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000023-0000-007d-0000-00350000005e"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "0000001d-0000-007f-0000-005000000074")))}
testObject_ServiceRef_provider_4 :: ServiceRef
testObject_ServiceRef_provider_4 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000023-0000-0050-0000-003000000044"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000057-0000-004c-0000-004b00000068")))}
testObject_ServiceRef_provider_5 :: ServiceRef
testObject_ServiceRef_provider_5 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000032-0000-0013-0000-001600000030"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "0000002d-0000-002b-0000-004400000018")))}
