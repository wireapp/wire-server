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
testObject_ConvTeamInfo_user_1 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000073-0000-0072-0000-003300000067"))), cnvManaged = False}
testObject_ConvTeamInfo_user_2 :: ConvTeamInfo
testObject_ConvTeamInfo_user_2 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0067-0000-00240000006b"))), cnvManaged = True}
testObject_ConvTeamInfo_user_3 :: ConvTeamInfo
testObject_ConvTeamInfo_user_3 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "0000006a-0000-0080-0000-004300000072"))), cnvManaged = False}
testObject_ConvTeamInfo_user_4 :: ConvTeamInfo
testObject_ConvTeamInfo_user_4 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000046-0000-003f-0000-000d00000041"))), cnvManaged = True}
testObject_ConvTeamInfo_user_5 :: ConvTeamInfo
testObject_ConvTeamInfo_user_5 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "0000001d-0000-0039-0000-002600000023"))), cnvManaged = False}
testObject_ConvTeamInfo_user_6 :: ConvTeamInfo
testObject_ConvTeamInfo_user_6 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "0000007c-0000-0012-0000-007200000035"))), cnvManaged = False}
testObject_ConvTeamInfo_user_7 :: ConvTeamInfo
testObject_ConvTeamInfo_user_7 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000033-0000-005a-0000-001b0000006b"))), cnvManaged = False}
testObject_ConvTeamInfo_user_8 :: ConvTeamInfo
testObject_ConvTeamInfo_user_8 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000025-0000-0056-0000-004e00000048"))), cnvManaged = False}
testObject_ConvTeamInfo_user_9 :: ConvTeamInfo
testObject_ConvTeamInfo_user_9 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "0000003b-0000-0041-0000-006c00000005"))), cnvManaged = True}
testObject_ConvTeamInfo_user_10 :: ConvTeamInfo
testObject_ConvTeamInfo_user_10 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000061-0000-000d-0000-00100000005b"))), cnvManaged = True}
testObject_ConvTeamInfo_user_11 :: ConvTeamInfo
testObject_ConvTeamInfo_user_11 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "0000001e-0000-000e-0000-001000000010"))), cnvManaged = True}
testObject_ConvTeamInfo_user_12 :: ConvTeamInfo
testObject_ConvTeamInfo_user_12 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "0000007a-0000-004c-0000-003a00000076"))), cnvManaged = True}
testObject_ConvTeamInfo_user_13 :: ConvTeamInfo
testObject_ConvTeamInfo_user_13 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "0000007b-0000-0026-0000-004a0000004e"))), cnvManaged = True}
testObject_ConvTeamInfo_user_14 :: ConvTeamInfo
testObject_ConvTeamInfo_user_14 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000059-0000-0058-0000-003b00000031"))), cnvManaged = True}
testObject_ConvTeamInfo_user_15 :: ConvTeamInfo
testObject_ConvTeamInfo_user_15 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000064-0000-0050-0000-001200000038"))), cnvManaged = True}
testObject_ConvTeamInfo_user_16 :: ConvTeamInfo
testObject_ConvTeamInfo_user_16 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000029-0000-0043-0000-004200000043"))), cnvManaged = True}
testObject_ConvTeamInfo_user_17 :: ConvTeamInfo
testObject_ConvTeamInfo_user_17 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "0000003d-0000-0038-0000-007700000031"))), cnvManaged = True}
testObject_ConvTeamInfo_user_18 :: ConvTeamInfo
testObject_ConvTeamInfo_user_18 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "0000002d-0000-0073-0000-003500000077"))), cnvManaged = True}
testObject_ConvTeamInfo_user_19 :: ConvTeamInfo
testObject_ConvTeamInfo_user_19 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "0000001d-0000-001e-0000-00090000003f"))), cnvManaged = False}
testObject_ConvTeamInfo_user_20 :: ConvTeamInfo
testObject_ConvTeamInfo_user_20 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000017-0000-004f-0000-006900000017"))), cnvManaged = True}
