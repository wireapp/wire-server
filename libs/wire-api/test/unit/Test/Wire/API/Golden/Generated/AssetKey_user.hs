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
testObject_AssetKey_user_1 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000080-0000-0008-0000-001a0000006a"))) AssetVolatile
testObject_AssetKey_user_2 :: AssetKey
testObject_AssetKey_user_2 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000037-0000-0056-0000-006b0000002f"))) AssetEternalInfrequentAccess
testObject_AssetKey_user_3 :: AssetKey
testObject_AssetKey_user_3 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000033-0000-000a-0000-00330000000a"))) AssetPersistent
testObject_AssetKey_user_4 :: AssetKey
testObject_AssetKey_user_4 = AssetKeyV3 (Id (fromJust (UUID.fromString "0000006e-0000-0058-0000-00050000000c"))) AssetEternal
testObject_AssetKey_user_5 :: AssetKey
testObject_AssetKey_user_5 = AssetKeyV3 (Id (fromJust (UUID.fromString "0000003d-0000-0005-0000-005e00000029"))) AssetExpiring
testObject_AssetKey_user_6 :: AssetKey
testObject_AssetKey_user_6 = AssetKeyV3 (Id (fromJust (UUID.fromString "0000004b-0000-000d-0000-006a00000019"))) AssetEternalInfrequentAccess
testObject_AssetKey_user_7 :: AssetKey
testObject_AssetKey_user_7 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000043-0000-006b-0000-00710000007e"))) AssetEternal
testObject_AssetKey_user_8 :: AssetKey
testObject_AssetKey_user_8 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000027-0000-002a-0000-00060000000c"))) AssetExpiring
testObject_AssetKey_user_9 :: AssetKey
testObject_AssetKey_user_9 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000079-0000-0018-0000-005600000009"))) AssetPersistent
testObject_AssetKey_user_10 :: AssetKey
testObject_AssetKey_user_10 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000008-0000-0054-0000-00290000002d"))) AssetPersistent
testObject_AssetKey_user_11 :: AssetKey
testObject_AssetKey_user_11 = AssetKeyV3 (Id (fromJust (UUID.fromString "0000003f-0000-0020-0000-001d00000025"))) AssetEternal
testObject_AssetKey_user_12 :: AssetKey
testObject_AssetKey_user_12 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000012-0000-0047-0000-00670000003c"))) AssetEternal
testObject_AssetKey_user_13 :: AssetKey
testObject_AssetKey_user_13 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000015-0000-0040-0000-005e0000005d"))) AssetVolatile
testObject_AssetKey_user_14 :: AssetKey
testObject_AssetKey_user_14 = AssetKeyV3 (Id (fromJust (UUID.fromString "0000007d-0000-0059-0000-004a0000001e"))) AssetExpiring
testObject_AssetKey_user_15 :: AssetKey
testObject_AssetKey_user_15 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000050-0000-0068-0000-000700000048"))) AssetEternalInfrequentAccess
testObject_AssetKey_user_16 :: AssetKey
testObject_AssetKey_user_16 = AssetKeyV3 (Id (fromJust (UUID.fromString "0000006d-0000-0003-0000-00130000007b"))) AssetVolatile
testObject_AssetKey_user_17 :: AssetKey
testObject_AssetKey_user_17 = AssetKeyV3 (Id (fromJust (UUID.fromString "0000005e-0000-0027-0000-00160000005f"))) AssetVolatile
testObject_AssetKey_user_18 :: AssetKey
testObject_AssetKey_user_18 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000077-0000-0046-0000-007e0000006f"))) AssetPersistent
testObject_AssetKey_user_19 :: AssetKey
testObject_AssetKey_user_19 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000072-0000-007c-0000-00500000006d"))) AssetEternalInfrequentAccess
testObject_AssetKey_user_20 :: AssetKey
testObject_AssetKey_user_20 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000040-0000-0053-0000-002800000055"))) AssetEternalInfrequentAccess
