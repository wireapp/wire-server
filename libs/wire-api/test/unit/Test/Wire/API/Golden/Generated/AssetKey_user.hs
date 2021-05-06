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
testObject_AssetKey_user_1 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000012-0000-0052-0000-000300000000"))) AssetVolatile
testObject_AssetKey_user_2 :: AssetKey
testObject_AssetKey_user_2 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000000-0000-0046-0000-002d00000009"))) AssetVolatile
testObject_AssetKey_user_3 :: AssetKey
testObject_AssetKey_user_3 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000008-0000-007f-0000-006b00000007"))) AssetEternal
testObject_AssetKey_user_4 :: AssetKey
testObject_AssetKey_user_4 = AssetKeyV3 (Id (fromJust (UUID.fromString "0000005c-0000-0051-0000-00030000006d"))) AssetEternal
testObject_AssetKey_user_5 :: AssetKey
testObject_AssetKey_user_5 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000002-0000-0043-0000-00190000002c"))) AssetEternal
testObject_AssetKey_user_6 :: AssetKey
testObject_AssetKey_user_6 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000073-0000-0071-0000-005300000026"))) AssetExpiring
testObject_AssetKey_user_7 :: AssetKey
testObject_AssetKey_user_7 = AssetKeyV3 (Id (fromJust (UUID.fromString "0000007d-0000-0019-0000-00720000006f"))) AssetExpiring
testObject_AssetKey_user_8 :: AssetKey
testObject_AssetKey_user_8 = AssetKeyV3 (Id (fromJust (UUID.fromString "0000003b-0000-002e-0000-006c0000002b"))) AssetVolatile
testObject_AssetKey_user_9 :: AssetKey
testObject_AssetKey_user_9 = AssetKeyV3 (Id (fromJust (UUID.fromString "0000005e-0000-005a-0000-000800000016"))) AssetExpiring
testObject_AssetKey_user_10 :: AssetKey
testObject_AssetKey_user_10 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000007-0000-006f-0000-002e0000005a"))) AssetVolatile
testObject_AssetKey_user_11 :: AssetKey
testObject_AssetKey_user_11 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000066-0000-0022-0000-005d0000001b"))) AssetExpiring
testObject_AssetKey_user_12 :: AssetKey
testObject_AssetKey_user_12 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000060-0000-0076-0000-006e0000001c"))) AssetExpiring
testObject_AssetKey_user_13 :: AssetKey
testObject_AssetKey_user_13 = AssetKeyV3 (Id (fromJust (UUID.fromString "0000001a-0000-0044-0000-00230000006d"))) AssetPersistent
testObject_AssetKey_user_14 :: AssetKey
testObject_AssetKey_user_14 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000029-0000-0013-0000-00230000002d"))) AssetExpiring
testObject_AssetKey_user_15 :: AssetKey
testObject_AssetKey_user_15 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000056-0000-0029-0000-001200000028"))) AssetVolatile
testObject_AssetKey_user_16 :: AssetKey
testObject_AssetKey_user_16 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000063-0000-006e-0000-001100000046"))) AssetEternal
testObject_AssetKey_user_17 :: AssetKey
testObject_AssetKey_user_17 = AssetKeyV3 (Id (fromJust (UUID.fromString "0000000c-0000-004a-0000-005200000029"))) AssetEternal
testObject_AssetKey_user_18 :: AssetKey
testObject_AssetKey_user_18 = AssetKeyV3 (Id (fromJust (UUID.fromString "0000007b-0000-0022-0000-005400000076"))) AssetVolatile
testObject_AssetKey_user_19 :: AssetKey
testObject_AssetKey_user_19 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000034-0000-007b-0000-007b00000052"))) AssetPersistent
testObject_AssetKey_user_20 :: AssetKey
testObject_AssetKey_user_20 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000047-0000-0046-0000-005a0000000f"))) AssetEternalInfrequentAccess
