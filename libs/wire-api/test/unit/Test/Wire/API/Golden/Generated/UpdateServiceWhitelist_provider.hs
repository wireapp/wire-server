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
testObject_UpdateServiceWhitelist_provider_1 = UpdateServiceWhitelist {updateServiceWhitelistProvider = (Id (fromJust (UUID.fromString "00000003-0000-0008-0000-001f00000017"))), updateServiceWhitelistService = (Id (fromJust (UUID.fromString "00000003-0000-001d-0000-001000000020"))), updateServiceWhitelistStatus = False}
testObject_UpdateServiceWhitelist_provider_2 :: UpdateServiceWhitelist
testObject_UpdateServiceWhitelist_provider_2 = UpdateServiceWhitelist {updateServiceWhitelistProvider = (Id (fromJust (UUID.fromString "00000016-0000-0017-0000-00030000001a"))), updateServiceWhitelistService = (Id (fromJust (UUID.fromString "0000001c-0000-001c-0000-00000000001c"))), updateServiceWhitelistStatus = False}
testObject_UpdateServiceWhitelist_provider_3 :: UpdateServiceWhitelist
testObject_UpdateServiceWhitelist_provider_3 = UpdateServiceWhitelist {updateServiceWhitelistProvider = (Id (fromJust (UUID.fromString "00000010-0000-000d-0000-000e00000009"))), updateServiceWhitelistService = (Id (fromJust (UUID.fromString "0000000d-0000-000f-0000-001100000017"))), updateServiceWhitelistStatus = True}
testObject_UpdateServiceWhitelist_provider_4 :: UpdateServiceWhitelist
testObject_UpdateServiceWhitelist_provider_4 = UpdateServiceWhitelist {updateServiceWhitelistProvider = (Id (fromJust (UUID.fromString "00000017-0000-001e-0000-000b00000017"))), updateServiceWhitelistService = (Id (fromJust (UUID.fromString "00000019-0000-0011-0000-001800000016"))), updateServiceWhitelistStatus = False}
testObject_UpdateServiceWhitelist_provider_5 :: UpdateServiceWhitelist
testObject_UpdateServiceWhitelist_provider_5 = UpdateServiceWhitelist {updateServiceWhitelistProvider = (Id (fromJust (UUID.fromString "0000000a-0000-0013-0000-000900000003"))), updateServiceWhitelistService = (Id (fromJust (UUID.fromString "00000019-0000-000e-0000-001d00000002"))), updateServiceWhitelistStatus = False}
testObject_UpdateServiceWhitelist_provider_6 :: UpdateServiceWhitelist
testObject_UpdateServiceWhitelist_provider_6 = UpdateServiceWhitelist {updateServiceWhitelistProvider = (Id (fromJust (UUID.fromString "0000000d-0000-0018-0000-000a0000000e"))), updateServiceWhitelistService = (Id (fromJust (UUID.fromString "0000001c-0000-0017-0000-002000000008"))), updateServiceWhitelistStatus = False}
testObject_UpdateServiceWhitelist_provider_7 :: UpdateServiceWhitelist
testObject_UpdateServiceWhitelist_provider_7 = UpdateServiceWhitelist {updateServiceWhitelistProvider = (Id (fromJust (UUID.fromString "00000002-0000-000e-0000-000700000002"))), updateServiceWhitelistService = (Id (fromJust (UUID.fromString "00000008-0000-0003-0000-000300000011"))), updateServiceWhitelistStatus = False}
testObject_UpdateServiceWhitelist_provider_8 :: UpdateServiceWhitelist
testObject_UpdateServiceWhitelist_provider_8 = UpdateServiceWhitelist {updateServiceWhitelistProvider = (Id (fromJust (UUID.fromString "0000001d-0000-0008-0000-001300000017"))), updateServiceWhitelistService = (Id (fromJust (UUID.fromString "00000013-0000-0012-0000-001d0000000e"))), updateServiceWhitelistStatus = False}
testObject_UpdateServiceWhitelist_provider_9 :: UpdateServiceWhitelist
testObject_UpdateServiceWhitelist_provider_9 = UpdateServiceWhitelist {updateServiceWhitelistProvider = (Id (fromJust (UUID.fromString "00000018-0000-0010-0000-000b00000017"))), updateServiceWhitelistService = (Id (fromJust (UUID.fromString "00000011-0000-001e-0000-000700000003"))), updateServiceWhitelistStatus = True}
testObject_UpdateServiceWhitelist_provider_10 :: UpdateServiceWhitelist
testObject_UpdateServiceWhitelist_provider_10 = UpdateServiceWhitelist {updateServiceWhitelistProvider = (Id (fromJust (UUID.fromString "00000007-0000-001b-0000-00050000000a"))), updateServiceWhitelistService = (Id (fromJust (UUID.fromString "0000000d-0000-0002-0000-000100000003"))), updateServiceWhitelistStatus = True}
testObject_UpdateServiceWhitelist_provider_11 :: UpdateServiceWhitelist
testObject_UpdateServiceWhitelist_provider_11 = UpdateServiceWhitelist {updateServiceWhitelistProvider = (Id (fromJust (UUID.fromString "00000012-0000-001a-0000-00120000001b"))), updateServiceWhitelistService = (Id (fromJust (UUID.fromString "00000005-0000-000c-0000-000400000002"))), updateServiceWhitelistStatus = False}
testObject_UpdateServiceWhitelist_provider_12 :: UpdateServiceWhitelist
testObject_UpdateServiceWhitelist_provider_12 = UpdateServiceWhitelist {updateServiceWhitelistProvider = (Id (fromJust (UUID.fromString "00000012-0000-0007-0000-000d00000005"))), updateServiceWhitelistService = (Id (fromJust (UUID.fromString "0000000e-0000-001b-0000-001b00000002"))), updateServiceWhitelistStatus = True}
testObject_UpdateServiceWhitelist_provider_13 :: UpdateServiceWhitelist
testObject_UpdateServiceWhitelist_provider_13 = UpdateServiceWhitelist {updateServiceWhitelistProvider = (Id (fromJust (UUID.fromString "00000011-0000-000a-0000-002000000012"))), updateServiceWhitelistService = (Id (fromJust (UUID.fromString "00000005-0000-000c-0000-001200000006"))), updateServiceWhitelistStatus = False}
testObject_UpdateServiceWhitelist_provider_14 :: UpdateServiceWhitelist
testObject_UpdateServiceWhitelist_provider_14 = UpdateServiceWhitelist {updateServiceWhitelistProvider = (Id (fromJust (UUID.fromString "00000017-0000-000c-0000-001a00000011"))), updateServiceWhitelistService = (Id (fromJust (UUID.fromString "00000005-0000-0005-0000-00060000001c"))), updateServiceWhitelistStatus = True}
testObject_UpdateServiceWhitelist_provider_15 :: UpdateServiceWhitelist
testObject_UpdateServiceWhitelist_provider_15 = UpdateServiceWhitelist {updateServiceWhitelistProvider = (Id (fromJust (UUID.fromString "00000015-0000-000d-0000-00120000001d"))), updateServiceWhitelistService = (Id (fromJust (UUID.fromString "0000000d-0000-0008-0000-00150000001d"))), updateServiceWhitelistStatus = True}
testObject_UpdateServiceWhitelist_provider_16 :: UpdateServiceWhitelist
testObject_UpdateServiceWhitelist_provider_16 = UpdateServiceWhitelist {updateServiceWhitelistProvider = (Id (fromJust (UUID.fromString "0000001c-0000-0013-0000-000500000002"))), updateServiceWhitelistService = (Id (fromJust (UUID.fromString "0000000c-0000-0005-0000-000700000007"))), updateServiceWhitelistStatus = False}
testObject_UpdateServiceWhitelist_provider_17 :: UpdateServiceWhitelist
testObject_UpdateServiceWhitelist_provider_17 = UpdateServiceWhitelist {updateServiceWhitelistProvider = (Id (fromJust (UUID.fromString "00000019-0000-000d-0000-00100000001f"))), updateServiceWhitelistService = (Id (fromJust (UUID.fromString "00000017-0000-0014-0000-00160000001c"))), updateServiceWhitelistStatus = False}
testObject_UpdateServiceWhitelist_provider_18 :: UpdateServiceWhitelist
testObject_UpdateServiceWhitelist_provider_18 = UpdateServiceWhitelist {updateServiceWhitelistProvider = (Id (fromJust (UUID.fromString "00000015-0000-001a-0000-00160000001f"))), updateServiceWhitelistService = (Id (fromJust (UUID.fromString "00000002-0000-001b-0000-000000000003"))), updateServiceWhitelistStatus = True}
testObject_UpdateServiceWhitelist_provider_19 :: UpdateServiceWhitelist
testObject_UpdateServiceWhitelist_provider_19 = UpdateServiceWhitelist {updateServiceWhitelistProvider = (Id (fromJust (UUID.fromString "00000004-0000-001c-0000-000b0000000d"))), updateServiceWhitelistService = (Id (fromJust (UUID.fromString "00000000-0000-0019-0000-001600000008"))), updateServiceWhitelistStatus = False}
testObject_UpdateServiceWhitelist_provider_20 :: UpdateServiceWhitelist
testObject_UpdateServiceWhitelist_provider_20 = UpdateServiceWhitelist {updateServiceWhitelistProvider = (Id (fromJust (UUID.fromString "00000006-0000-0012-0000-00160000000a"))), updateServiceWhitelistService = (Id (fromJust (UUID.fromString "0000001d-0000-001f-0000-00170000000d"))), updateServiceWhitelistStatus = False}
