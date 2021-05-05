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
testObject_NewServiceResponse_1 :: NewServiceResponse
testObject_NewServiceResponse_1 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "0000007b-0000-0023-0000-00730000000b"))), rsNewServiceToken = Nothing}
testObject_NewServiceResponse_2 :: NewServiceResponse
testObject_NewServiceResponse_2 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "00000033-0000-0027-0000-006400000070"))), rsNewServiceToken = Nothing}
testObject_NewServiceResponse_3 :: NewServiceResponse
testObject_NewServiceResponse_3 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "00000062-0000-0075-0000-000100000047"))), rsNewServiceToken = Nothing}
testObject_NewServiceResponse_4 :: NewServiceResponse
testObject_NewServiceResponse_4 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "0000001a-0000-0043-0000-000500000055"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("L8C3YUlEzPNcQ4m3eQ=="))))}
testObject_NewServiceResponse_5 :: NewServiceResponse
testObject_NewServiceResponse_5 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "00000048-0000-0022-0000-000e00000032"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("jd64BdE="))))}
