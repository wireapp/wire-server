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
testObject_ServiceRef_1 :: ServiceRef
testObject_ServiceRef_1 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "0000004d-0000-0001-0000-00170000002f"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "0000004d-0000-0033-0000-006500000029")))}
testObject_ServiceRef_2 :: ServiceRef
testObject_ServiceRef_2 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000055-0000-0015-0000-00670000007b"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "0000002e-0000-005b-0000-004d00000023")))}
testObject_ServiceRef_3 :: ServiceRef
testObject_ServiceRef_3 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "0000000c-0000-0037-0000-006500000057"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "0000003a-0000-0080-0000-00120000003d")))}
testObject_ServiceRef_4 :: ServiceRef
testObject_ServiceRef_4 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000052-0000-0000-0000-00650000003e"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "0000000c-0000-0018-0000-001200000070")))}
testObject_ServiceRef_5 :: ServiceRef
testObject_ServiceRef_5 = ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000011-0000-0013-0000-001e0000002f"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000028-0000-0013-0000-004c00000047")))}
