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
testObject_NewServiceResponse_provider_1 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "0000000e-0000-003f-0000-00090000003e"))), rsNewServiceToken = Nothing}
testObject_NewServiceResponse_provider_2 :: NewServiceResponse
testObject_NewServiceResponse_provider_2 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "0000006e-0000-0056-0000-003200000064"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("deyDKw=="))))}
testObject_NewServiceResponse_provider_3 :: NewServiceResponse
testObject_NewServiceResponse_provider_3 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "0000004d-0000-0033-0000-007a00000061"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("blA6sXMxBgrFMVXsTEw="))))}
testObject_NewServiceResponse_provider_4 :: NewServiceResponse
testObject_NewServiceResponse_provider_4 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "00000044-0000-004d-0000-003400000044"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("y50I"))))}
testObject_NewServiceResponse_provider_5 :: NewServiceResponse
testObject_NewServiceResponse_provider_5 = NewServiceResponse {rsNewServiceId = (Id (fromJust (UUID.fromString "0000001f-0000-0004-0000-004600000026"))), rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("w1_n"))))}
