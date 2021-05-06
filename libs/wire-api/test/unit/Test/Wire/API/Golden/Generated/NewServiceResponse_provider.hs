{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.NewServiceResponse_provider where

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
testObject_NewServiceResponse_provider_1 :: NewServiceResponse
testObject_NewServiceResponse_provider_1 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "00000062-0000-0022-0000-002600000066"))), rsNewServiceToken = Nothing}
testObject_NewServiceResponse_provider_2 :: NewServiceResponse
testObject_NewServiceResponse_provider_2 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "0000000b-0000-0060-0000-000500000005"))), rsNewServiceToken = Nothing}
testObject_NewServiceResponse_provider_3 :: NewServiceResponse
testObject_NewServiceResponse_provider_3 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "00000004-0000-0055-0000-00070000001a"))), rsNewServiceToken = Nothing}
testObject_NewServiceResponse_provider_4 :: NewServiceResponse
testObject_NewServiceResponse_provider_4 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "0000006f-0000-007c-0000-00650000001b"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("Uw=="))))}
testObject_NewServiceResponse_provider_5 :: NewServiceResponse
testObject_NewServiceResponse_provider_5 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "0000000b-0000-004c-0000-006c0000000d"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("hwz3hCfxEbXJCQ=="))))}
testObject_NewServiceResponse_provider_6 :: NewServiceResponse
testObject_NewServiceResponse_provider_6 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "0000000e-0000-0024-0000-00220000006d"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("qQ=="))))}
testObject_NewServiceResponse_provider_7 :: NewServiceResponse
testObject_NewServiceResponse_provider_7 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "00000066-0000-001e-0000-002900000016"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("P4LvUOaogg=="))))}
testObject_NewServiceResponse_provider_8 :: NewServiceResponse
testObject_NewServiceResponse_provider_8 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "00000018-0000-0000-0000-00000000007f"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate (""))))}
testObject_NewServiceResponse_provider_9 :: NewServiceResponse
testObject_NewServiceResponse_provider_9 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "00000008-0000-002b-0000-004300000077"))), rsNewServiceToken = Nothing}
testObject_NewServiceResponse_provider_10 :: NewServiceResponse
testObject_NewServiceResponse_provider_10 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "00000065-0000-0013-0000-006b00000062"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("vX2rBUgrM33Wy8D13Q=="))))}
testObject_NewServiceResponse_provider_11 :: NewServiceResponse
testObject_NewServiceResponse_provider_11 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "0000004e-0000-005e-0000-00530000007b"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate (""))))}
testObject_NewServiceResponse_provider_12 :: NewServiceResponse
testObject_NewServiceResponse_provider_12 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "00000076-0000-0001-0000-00310000006d"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("DH93DdqLadnpH1M="))))}
testObject_NewServiceResponse_provider_13 :: NewServiceResponse
testObject_NewServiceResponse_provider_13 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "0000003d-0000-0004-0000-006b0000007a"))), rsNewServiceToken = Nothing}
testObject_NewServiceResponse_provider_14 :: NewServiceResponse
testObject_NewServiceResponse_provider_14 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "00000076-0000-0002-0000-001f0000002d"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("Z262M8xzL4ZYbedLqQ=="))))}
testObject_NewServiceResponse_provider_15 :: NewServiceResponse
testObject_NewServiceResponse_provider_15 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-001e00000035"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("m08yTcjFxUU="))))}
testObject_NewServiceResponse_provider_16 :: NewServiceResponse
testObject_NewServiceResponse_provider_16 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "00000014-0000-0032-0000-00710000007f"))), rsNewServiceToken = Nothing}
testObject_NewServiceResponse_provider_17 :: NewServiceResponse
testObject_NewServiceResponse_provider_17 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "00000067-0000-0054-0000-007600000009"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("vczybg=="))))}
testObject_NewServiceResponse_provider_18 :: NewServiceResponse
testObject_NewServiceResponse_provider_18 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "0000007e-0000-002c-0000-005200000012"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate (""))))}
testObject_NewServiceResponse_provider_19 :: NewServiceResponse
testObject_NewServiceResponse_provider_19 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "00000039-0000-0048-0000-000400000038"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("vGU="))))}
testObject_NewServiceResponse_provider_20 :: NewServiceResponse
testObject_NewServiceResponse_provider_20 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "0000007a-0000-0078-0000-000c00000059"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("jSnwGHQBtt0x2g=="))))}
