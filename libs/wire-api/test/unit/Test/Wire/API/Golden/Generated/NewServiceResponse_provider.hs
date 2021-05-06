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
testObject_NewServiceResponse_provider_1 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "00000051-0000-000d-0000-003b00000078"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("Sg=="))))}
testObject_NewServiceResponse_provider_2 :: NewServiceResponse
testObject_NewServiceResponse_provider_2 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "00000041-0000-0030-0000-005b00000025"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("FMLe93AvwTkdD64boQ=="))))}
testObject_NewServiceResponse_provider_3 :: NewServiceResponse
testObject_NewServiceResponse_provider_3 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "0000007c-0000-005d-0000-002300000049"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("ooo="))))}
testObject_NewServiceResponse_provider_4 :: NewServiceResponse
testObject_NewServiceResponse_provider_4 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "0000000e-0000-006f-0000-003600000074"))), rsNewServiceToken = Nothing}
testObject_NewServiceResponse_provider_5 :: NewServiceResponse
testObject_NewServiceResponse_provider_5 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "00000034-0000-0007-0000-005300000037"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("4XKa0NHs"))))}
testObject_NewServiceResponse_provider_6 :: NewServiceResponse
testObject_NewServiceResponse_provider_6 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "00000024-0000-0036-0000-00800000001d"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("W0F3IfBnuHJASA=="))))}
testObject_NewServiceResponse_provider_7 :: NewServiceResponse
testObject_NewServiceResponse_provider_7 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "00000022-0000-005c-0000-005900000000"))), rsNewServiceToken = Nothing}
testObject_NewServiceResponse_provider_8 :: NewServiceResponse
testObject_NewServiceResponse_provider_8 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "00000046-0000-006c-0000-00070000002b"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("9bzZL1TCndw="))))}
testObject_NewServiceResponse_provider_9 :: NewServiceResponse
testObject_NewServiceResponse_provider_9 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "0000000c-0000-006e-0000-002b0000003a"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("JQ=="))))}
testObject_NewServiceResponse_provider_10 :: NewServiceResponse
testObject_NewServiceResponse_provider_10 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "00000007-0000-0006-0000-006700000019"))), rsNewServiceToken = Nothing}
testObject_NewServiceResponse_provider_11 :: NewServiceResponse
testObject_NewServiceResponse_provider_11 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "00000016-0000-0033-0000-005f0000004d"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("eLrLIcg="))))}
testObject_NewServiceResponse_provider_12 :: NewServiceResponse
testObject_NewServiceResponse_provider_12 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "0000000a-0000-0060-0000-000100000055"))), rsNewServiceToken = Nothing}
testObject_NewServiceResponse_provider_13 :: NewServiceResponse
testObject_NewServiceResponse_provider_13 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "00000001-0000-0031-0000-007100000080"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("Gt2QOtJfdaqo"))))}
testObject_NewServiceResponse_provider_14 :: NewServiceResponse
testObject_NewServiceResponse_provider_14 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "00000054-0000-006a-0000-00100000000f"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("Dcqm"))))}
testObject_NewServiceResponse_provider_15 :: NewServiceResponse
testObject_NewServiceResponse_provider_15 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "0000005d-0000-0052-0000-006b00000062"))), rsNewServiceToken = Nothing}
testObject_NewServiceResponse_provider_16 :: NewServiceResponse
testObject_NewServiceResponse_provider_16 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "00000011-0000-006b-0000-006e0000000d"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("oRg1uNPa"))))}
testObject_NewServiceResponse_provider_17 :: NewServiceResponse
testObject_NewServiceResponse_provider_17 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "00000043-0000-0073-0000-003b00000011"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("Z6M="))))}
testObject_NewServiceResponse_provider_18 :: NewServiceResponse
testObject_NewServiceResponse_provider_18 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "00000004-0000-0061-0000-006e0000000d"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate (""))))}
testObject_NewServiceResponse_provider_19 :: NewServiceResponse
testObject_NewServiceResponse_provider_19 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "00000051-0000-0003-0000-00310000000c"))), rsNewServiceToken = Nothing}
testObject_NewServiceResponse_provider_20 :: NewServiceResponse
testObject_NewServiceResponse_provider_20 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "0000007d-0000-004c-0000-002700000015"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("BttOhEczBUM="))))}
