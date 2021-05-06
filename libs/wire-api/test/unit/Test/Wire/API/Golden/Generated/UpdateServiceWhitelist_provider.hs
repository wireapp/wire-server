{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.UpdateServiceWhitelist_provider where

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
testObject_UpdateServiceWhitelist_provider_1 :: UpdateServiceWhitelist
testObject_UpdateServiceWhitelist_provider_1 = UpdateServiceWhitelist {updateServiceWhitelistProvider = (Id (fromJust (UUID.fromString "0000001f-0000-0013-0000-001e00000019"))), updateServiceWhitelistService = (Id (fromJust (UUID.fromString "0000000b-0000-0018-0000-00200000000f"))), updateServiceWhitelistStatus = True}
testObject_UpdateServiceWhitelist_provider_2 :: UpdateServiceWhitelist
testObject_UpdateServiceWhitelist_provider_2 = UpdateServiceWhitelist {updateServiceWhitelistProvider = (Id (fromJust (UUID.fromString "00000018-0000-0001-0000-000f00000006"))), updateServiceWhitelistService = (Id (fromJust (UUID.fromString "00000002-0000-0020-0000-001c0000000d"))), updateServiceWhitelistStatus = True}
testObject_UpdateServiceWhitelist_provider_3 :: UpdateServiceWhitelist
testObject_UpdateServiceWhitelist_provider_3 = UpdateServiceWhitelist {updateServiceWhitelistProvider = (Id (fromJust (UUID.fromString "0000001f-0000-000c-0000-000b0000000d"))), updateServiceWhitelistService = (Id (fromJust (UUID.fromString "00000001-0000-0014-0000-000b0000001c"))), updateServiceWhitelistStatus = False}
testObject_UpdateServiceWhitelist_provider_4 :: UpdateServiceWhitelist
testObject_UpdateServiceWhitelist_provider_4 = UpdateServiceWhitelist {updateServiceWhitelistProvider = (Id (fromJust (UUID.fromString "00000003-0000-0000-0000-00170000000c"))), updateServiceWhitelistService = (Id (fromJust (UUID.fromString "00000001-0000-0006-0000-001c00000004"))), updateServiceWhitelistStatus = True}
testObject_UpdateServiceWhitelist_provider_5 :: UpdateServiceWhitelist
testObject_UpdateServiceWhitelist_provider_5 = UpdateServiceWhitelist {updateServiceWhitelistProvider = (Id (fromJust (UUID.fromString "00000017-0000-0014-0000-00030000001d"))), updateServiceWhitelistService = (Id (fromJust (UUID.fromString "00000017-0000-000f-0000-00150000000f"))), updateServiceWhitelistStatus = True}
