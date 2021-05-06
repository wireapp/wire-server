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
testObject_NewServiceResponse_provider_1 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "00000008-0000-0038-0000-006f0000000b"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("IaTwZnS2PDYUzMiT"))))}
testObject_NewServiceResponse_provider_2 :: NewServiceResponse
testObject_NewServiceResponse_provider_2 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "00000074-0000-0020-0000-007f0000007f"))), rsNewServiceToken = Nothing}
testObject_NewServiceResponse_provider_3 :: NewServiceResponse
testObject_NewServiceResponse_provider_3 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "00000040-0000-0077-0000-005e00000028"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("LKIdS1GdkQ=="))))}
testObject_NewServiceResponse_provider_4 :: NewServiceResponse
testObject_NewServiceResponse_provider_4 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "00000065-0000-0012-0000-005100000004"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("sQjjkmkOMYCc"))))}
testObject_NewServiceResponse_provider_5 :: NewServiceResponse
testObject_NewServiceResponse_provider_5 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "0000000a-0000-0000-0000-003c0000001f"))), rsNewServiceToken = Nothing}
testObject_NewServiceResponse_provider_6 :: NewServiceResponse
testObject_NewServiceResponse_provider_6 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "0000007a-0000-0022-0000-00510000006d"))), rsNewServiceToken = Nothing}
testObject_NewServiceResponse_provider_7 :: NewServiceResponse
testObject_NewServiceResponse_provider_7 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "0000004a-0000-001e-0000-004e00000038"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("RBVV6ak="))))}
testObject_NewServiceResponse_provider_8 :: NewServiceResponse
testObject_NewServiceResponse_provider_8 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "00000058-0000-005e-0000-00740000002f"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("OIzTgu0="))))}
testObject_NewServiceResponse_provider_9 :: NewServiceResponse
testObject_NewServiceResponse_provider_9 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "0000007a-0000-005d-0000-005e00000060"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("1_05ewSZ6-9Jabec"))))}
testObject_NewServiceResponse_provider_10 :: NewServiceResponse
testObject_NewServiceResponse_provider_10 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "0000005b-0000-0002-0000-002800000076"))), rsNewServiceToken = Nothing}
testObject_NewServiceResponse_provider_11 :: NewServiceResponse
testObject_NewServiceResponse_provider_11 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "0000000a-0000-0021-0000-005600000062"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("VlEHSho="))))}
testObject_NewServiceResponse_provider_12 :: NewServiceResponse
testObject_NewServiceResponse_provider_12 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "00000013-0000-007f-0000-007000000049"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("Nc2bXna1fwbrBQ=="))))}
testObject_NewServiceResponse_provider_13 :: NewServiceResponse
testObject_NewServiceResponse_provider_13 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "00000036-0000-0078-0000-003b00000076"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate (""))))}
testObject_NewServiceResponse_provider_14 :: NewServiceResponse
testObject_NewServiceResponse_provider_14 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "00000037-0000-0051-0000-002f0000002c"))), rsNewServiceToken = Nothing}
testObject_NewServiceResponse_provider_15 :: NewServiceResponse
testObject_NewServiceResponse_provider_15 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "00000014-0000-0012-0000-007d00000031"))), rsNewServiceToken = Nothing}
testObject_NewServiceResponse_provider_16 :: NewServiceResponse
testObject_NewServiceResponse_provider_16 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "0000007f-0000-0036-0000-00420000000a"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate (""))))}
testObject_NewServiceResponse_provider_17 :: NewServiceResponse
testObject_NewServiceResponse_provider_17 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "00000022-0000-0015-0000-002c00000029"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("0fMSE-UR"))))}
testObject_NewServiceResponse_provider_18 :: NewServiceResponse
testObject_NewServiceResponse_provider_18 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "00000016-0000-0051-0000-007200000079"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate (""))))}
testObject_NewServiceResponse_provider_19 :: NewServiceResponse
testObject_NewServiceResponse_provider_19 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "00000002-0000-0073-0000-000800000074"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("Pg=="))))}
testObject_NewServiceResponse_provider_20 :: NewServiceResponse
testObject_NewServiceResponse_provider_20 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "0000001d-0000-002e-0000-001300000004"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("cd-OW57tLx2RIMJkaA=="))))}
