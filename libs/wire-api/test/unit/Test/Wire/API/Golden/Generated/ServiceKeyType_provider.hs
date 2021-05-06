{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ServiceKeyType_provider where

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
testObject_ServiceKeyType_provider_1 :: ServiceKeyType
testObject_ServiceKeyType_provider_1 = RsaServiceKey
testObject_ServiceKeyType_provider_2 :: ServiceKeyType
testObject_ServiceKeyType_provider_2 = RsaServiceKey
testObject_ServiceKeyType_provider_3 :: ServiceKeyType
testObject_ServiceKeyType_provider_3 = RsaServiceKey
testObject_ServiceKeyType_provider_4 :: ServiceKeyType
testObject_ServiceKeyType_provider_4 = RsaServiceKey
testObject_ServiceKeyType_provider_5 :: ServiceKeyType
testObject_ServiceKeyType_provider_5 = RsaServiceKey
testObject_ServiceKeyType_provider_6 :: ServiceKeyType
testObject_ServiceKeyType_provider_6 = RsaServiceKey
testObject_ServiceKeyType_provider_7 :: ServiceKeyType
testObject_ServiceKeyType_provider_7 = RsaServiceKey
testObject_ServiceKeyType_provider_8 :: ServiceKeyType
testObject_ServiceKeyType_provider_8 = RsaServiceKey
testObject_ServiceKeyType_provider_9 :: ServiceKeyType
testObject_ServiceKeyType_provider_9 = RsaServiceKey
testObject_ServiceKeyType_provider_10 :: ServiceKeyType
testObject_ServiceKeyType_provider_10 = RsaServiceKey
testObject_ServiceKeyType_provider_11 :: ServiceKeyType
testObject_ServiceKeyType_provider_11 = RsaServiceKey
testObject_ServiceKeyType_provider_12 :: ServiceKeyType
testObject_ServiceKeyType_provider_12 = RsaServiceKey
testObject_ServiceKeyType_provider_13 :: ServiceKeyType
testObject_ServiceKeyType_provider_13 = RsaServiceKey
testObject_ServiceKeyType_provider_14 :: ServiceKeyType
testObject_ServiceKeyType_provider_14 = RsaServiceKey
testObject_ServiceKeyType_provider_15 :: ServiceKeyType
testObject_ServiceKeyType_provider_15 = RsaServiceKey
testObject_ServiceKeyType_provider_16 :: ServiceKeyType
testObject_ServiceKeyType_provider_16 = RsaServiceKey
testObject_ServiceKeyType_provider_17 :: ServiceKeyType
testObject_ServiceKeyType_provider_17 = RsaServiceKey
testObject_ServiceKeyType_provider_18 :: ServiceKeyType
testObject_ServiceKeyType_provider_18 = RsaServiceKey
testObject_ServiceKeyType_provider_19 :: ServiceKeyType
testObject_ServiceKeyType_provider_19 = RsaServiceKey
testObject_ServiceKeyType_provider_20 :: ServiceKeyType
testObject_ServiceKeyType_provider_20 = RsaServiceKey
