{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ConvTeamInfo_user where

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
testObject_ConvTeamInfo_user_1 :: ConvTeamInfo
testObject_ConvTeamInfo_user_1 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000035-0000-0010-0000-003000000022"))), cnvManaged = False}
testObject_ConvTeamInfo_user_2 :: ConvTeamInfo
testObject_ConvTeamInfo_user_2 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "0000006c-0000-003d-0000-006b0000000d"))), cnvManaged = True}
testObject_ConvTeamInfo_user_3 :: ConvTeamInfo
testObject_ConvTeamInfo_user_3 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "0000000a-0000-0041-0000-004400000067"))), cnvManaged = False}
testObject_ConvTeamInfo_user_4 :: ConvTeamInfo
testObject_ConvTeamInfo_user_4 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "0000001c-0000-0012-0000-00100000002f"))), cnvManaged = True}
testObject_ConvTeamInfo_user_5 :: ConvTeamInfo
testObject_ConvTeamInfo_user_5 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000004-0000-000d-0000-004200000049"))), cnvManaged = True}
testObject_ConvTeamInfo_user_6 :: ConvTeamInfo
testObject_ConvTeamInfo_user_6 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000044-0000-003f-0000-00740000000d"))), cnvManaged = False}
testObject_ConvTeamInfo_user_7 :: ConvTeamInfo
testObject_ConvTeamInfo_user_7 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000038-0000-000f-0000-000b00000010"))), cnvManaged = True}
testObject_ConvTeamInfo_user_8 :: ConvTeamInfo
testObject_ConvTeamInfo_user_8 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "0000002e-0000-0065-0000-004b00000005"))), cnvManaged = False}
testObject_ConvTeamInfo_user_9 :: ConvTeamInfo
testObject_ConvTeamInfo_user_9 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000017-0000-0047-0000-003800000069"))), cnvManaged = False}
testObject_ConvTeamInfo_user_10 :: ConvTeamInfo
testObject_ConvTeamInfo_user_10 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000008-0000-0080-0000-004700000029"))), cnvManaged = False}
testObject_ConvTeamInfo_user_11 :: ConvTeamInfo
testObject_ConvTeamInfo_user_11 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "0000000e-0000-0028-0000-005a00000008"))), cnvManaged = True}
testObject_ConvTeamInfo_user_12 :: ConvTeamInfo
testObject_ConvTeamInfo_user_12 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "0000006c-0000-002f-0000-007f00000059"))), cnvManaged = True}
testObject_ConvTeamInfo_user_13 :: ConvTeamInfo
testObject_ConvTeamInfo_user_13 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000030-0000-0080-0000-001b00000080"))), cnvManaged = True}
testObject_ConvTeamInfo_user_14 :: ConvTeamInfo
testObject_ConvTeamInfo_user_14 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000049-0000-007b-0000-001000000008"))), cnvManaged = False}
testObject_ConvTeamInfo_user_15 :: ConvTeamInfo
testObject_ConvTeamInfo_user_15 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000044-0000-0023-0000-00690000000b"))), cnvManaged = False}
testObject_ConvTeamInfo_user_16 :: ConvTeamInfo
testObject_ConvTeamInfo_user_16 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000009-0000-0004-0000-006a00000078"))), cnvManaged = True}
testObject_ConvTeamInfo_user_17 :: ConvTeamInfo
testObject_ConvTeamInfo_user_17 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000033-0000-0060-0000-001100000011"))), cnvManaged = True}
testObject_ConvTeamInfo_user_18 :: ConvTeamInfo
testObject_ConvTeamInfo_user_18 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "0000003c-0000-0018-0000-00580000001a"))), cnvManaged = True}
testObject_ConvTeamInfo_user_19 :: ConvTeamInfo
testObject_ConvTeamInfo_user_19 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000065-0000-0019-0000-005a00000065"))), cnvManaged = True}
testObject_ConvTeamInfo_user_20 :: ConvTeamInfo
testObject_ConvTeamInfo_user_20 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "0000003f-0000-0024-0000-000d00000047"))), cnvManaged = False}
