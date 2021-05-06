{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.AssetKey_user where

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
import Wire.API.Asset
import Wire.API.Asset.V3.Resumable
import Wire.API.Call.Config
import Wire.API.Connection
import Wire.API.Conversation
import Wire.API.Conversation.Bot
import Wire.API.Conversation.Code
import Wire.API.Conversation.Member
import Wire.API.Conversation.Role
import Wire.API.Conversation.Typing
import Wire.API.CustomBackend
import Wire.API.Event.Conversation
import Wire.API.Message
import Wire.API.Notification (QueuedNotification, queuedNotification, QueuedNotificationList, queuedNotificationList)
import Wire.API.Properties
-- import Wire.API.Provider
import Wire.API.Provider.Bot
import Wire.API.Provider.External
import Wire.API.Provider.Service
-- import Wire.API.Provider.Service.Tag
import Wire.API.Push.Token hiding (Transport)
import qualified Wire.API.Push.Token as Push.Token
import Wire.API.Team
import Wire.API.Team.Role
-- import Wire.API.Team.SearchVisibility
import Wire.API.User
import Wire.API.User.Activation
import Wire.API.User.Auth
import Wire.API.User.Client
import Wire.API.User.Client.Prekey
import Wire.API.User.Handle
import Wire.API.User.Identity
import Wire.API.User.Password
import Wire.API.User.Profile
import Wire.API.User.RichInfo
import Wire.API.User.Search
import Wire.API.Wrapped
testObject_AssetKey_user_1 :: AssetKey
testObject_AssetKey_user_1 = AssetKeyV3 (Id (fromJust (UUID.fromString "0000002f-0000-000b-0000-000a00000024"))) AssetExpiring
testObject_AssetKey_user_2 :: AssetKey
testObject_AssetKey_user_2 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000000-0000-0065-0000-001200000049"))) AssetEternalInfrequentAccess
testObject_AssetKey_user_3 :: AssetKey
testObject_AssetKey_user_3 = AssetKeyV3 (Id (fromJust (UUID.fromString "0000004f-0000-0021-0000-006700000029"))) AssetExpiring
testObject_AssetKey_user_4 :: AssetKey
testObject_AssetKey_user_4 = AssetKeyV3 (Id (fromJust (UUID.fromString "0000003c-0000-006b-0000-005e00000074"))) AssetVolatile
testObject_AssetKey_user_5 :: AssetKey
testObject_AssetKey_user_5 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000005-0000-007b-0000-000f00000052"))) AssetEternalInfrequentAccess
testObject_AssetKey_user_6 :: AssetKey
testObject_AssetKey_user_6 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000079-0000-0020-0000-004000000078"))) AssetVolatile
testObject_AssetKey_user_7 :: AssetKey
testObject_AssetKey_user_7 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000055-0000-0056-0000-005b00000014"))) AssetExpiring
testObject_AssetKey_user_8 :: AssetKey
testObject_AssetKey_user_8 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000053-0000-0022-0000-003b00000046"))) AssetEternal
testObject_AssetKey_user_9 :: AssetKey
testObject_AssetKey_user_9 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000012-0000-0016-0000-006600000018"))) AssetExpiring
testObject_AssetKey_user_10 :: AssetKey
testObject_AssetKey_user_10 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000012-0000-003f-0000-00080000004e"))) AssetPersistent
testObject_AssetKey_user_11 :: AssetKey
testObject_AssetKey_user_11 = AssetKeyV3 (Id (fromJust (UUID.fromString "0000001d-0000-0069-0000-004100000005"))) AssetEternal
testObject_AssetKey_user_12 :: AssetKey
testObject_AssetKey_user_12 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000035-0000-004c-0000-000e00000054"))) AssetPersistent
testObject_AssetKey_user_13 :: AssetKey
testObject_AssetKey_user_13 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000068-0000-0023-0000-00300000007e"))) AssetEternal
testObject_AssetKey_user_14 :: AssetKey
testObject_AssetKey_user_14 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000066-0000-004e-0000-00200000000a"))) AssetPersistent
testObject_AssetKey_user_15 :: AssetKey
testObject_AssetKey_user_15 = AssetKeyV3 (Id (fromJust (UUID.fromString "0000005f-0000-005a-0000-006d0000002b"))) AssetPersistent
testObject_AssetKey_user_16 :: AssetKey
testObject_AssetKey_user_16 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000038-0000-005a-0000-006700000018"))) AssetExpiring
testObject_AssetKey_user_17 :: AssetKey
testObject_AssetKey_user_17 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000065-0000-0014-0000-00480000000c"))) AssetExpiring
testObject_AssetKey_user_18 :: AssetKey
testObject_AssetKey_user_18 = AssetKeyV3 (Id (fromJust (UUID.fromString "0000005c-0000-0033-0000-000e00000018"))) AssetEternal
testObject_AssetKey_user_19 :: AssetKey
testObject_AssetKey_user_19 = AssetKeyV3 (Id (fromJust (UUID.fromString "0000003b-0000-0061-0000-00720000001f"))) AssetPersistent
testObject_AssetKey_user_20 :: AssetKey
testObject_AssetKey_user_20 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000025-0000-001d-0000-003a00000031"))) AssetEternalInfrequentAccess
