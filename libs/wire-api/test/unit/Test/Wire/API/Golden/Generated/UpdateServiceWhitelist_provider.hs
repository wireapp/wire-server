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
testObject_UpdateServiceWhitelist_1 :: UpdateServiceWhitelist
testObject_UpdateServiceWhitelist_1 = UpdateServiceWhitelist {updateServiceWhitelistProvider = (Id (fromJust (UUID.fromString "00000006-0000-001a-0000-000000000002"))), updateServiceWhitelistService = (Id (fromJust (UUID.fromString "00000009-0000-001c-0000-00080000000e"))), updateServiceWhitelistStatus = True}
testObject_UpdateServiceWhitelist_2 :: UpdateServiceWhitelist
testObject_UpdateServiceWhitelist_2 = UpdateServiceWhitelist {updateServiceWhitelistProvider = (Id (fromJust (UUID.fromString "00000008-0000-0002-0000-000300000001"))), updateServiceWhitelistService = (Id (fromJust (UUID.fromString "00000003-0000-001c-0000-001b00000015"))), updateServiceWhitelistStatus = True}
testObject_UpdateServiceWhitelist_3 :: UpdateServiceWhitelist
testObject_UpdateServiceWhitelist_3 = UpdateServiceWhitelist {updateServiceWhitelistProvider = (Id (fromJust (UUID.fromString "00000011-0000-0018-0000-00160000001e"))), updateServiceWhitelistService = (Id (fromJust (UUID.fromString "0000000d-0000-000a-0000-00090000000a"))), updateServiceWhitelistStatus = True}
testObject_UpdateServiceWhitelist_4 :: UpdateServiceWhitelist
testObject_UpdateServiceWhitelist_4 = UpdateServiceWhitelist {updateServiceWhitelistProvider = (Id (fromJust (UUID.fromString "00000003-0000-0001-0000-00140000001c"))), updateServiceWhitelistService = (Id (fromJust (UUID.fromString "00000017-0000-001c-0000-00130000000d"))), updateServiceWhitelistStatus = True}
testObject_UpdateServiceWhitelist_5 :: UpdateServiceWhitelist
testObject_UpdateServiceWhitelist_5 = UpdateServiceWhitelist {updateServiceWhitelistProvider = (Id (fromJust (UUID.fromString "00000016-0000-0008-0000-001900000020"))), updateServiceWhitelistService = (Id (fromJust (UUID.fromString "00000009-0000-000f-0000-001e0000001d"))), updateServiceWhitelistStatus = False}
