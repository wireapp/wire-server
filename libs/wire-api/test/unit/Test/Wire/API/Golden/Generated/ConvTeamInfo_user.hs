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
testObject_ConvTeamInfo_user_1 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "0000002c-0000-0057-0000-00650000004b"))), cnvManaged = False}
testObject_ConvTeamInfo_user_2 :: ConvTeamInfo
testObject_ConvTeamInfo_user_2 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000013-0000-0052-0000-00560000005c"))), cnvManaged = False}
testObject_ConvTeamInfo_user_3 :: ConvTeamInfo
testObject_ConvTeamInfo_user_3 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "0000007e-0000-0010-0000-000100000048"))), cnvManaged = True}
testObject_ConvTeamInfo_user_4 :: ConvTeamInfo
testObject_ConvTeamInfo_user_4 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000009-0000-0073-0000-007000000068"))), cnvManaged = False}
testObject_ConvTeamInfo_user_5 :: ConvTeamInfo
testObject_ConvTeamInfo_user_5 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "0000005f-0000-0036-0000-00070000007b"))), cnvManaged = True}
testObject_ConvTeamInfo_user_6 :: ConvTeamInfo
testObject_ConvTeamInfo_user_6 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000042-0000-0040-0000-002d00000045"))), cnvManaged = False}
testObject_ConvTeamInfo_user_7 :: ConvTeamInfo
testObject_ConvTeamInfo_user_7 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000051-0000-0072-0000-002c00000028"))), cnvManaged = False}
testObject_ConvTeamInfo_user_8 :: ConvTeamInfo
testObject_ConvTeamInfo_user_8 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "0000005b-0000-0000-0000-002800000056"))), cnvManaged = True}
testObject_ConvTeamInfo_user_9 :: ConvTeamInfo
testObject_ConvTeamInfo_user_9 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000065-0000-0044-0000-005600000009"))), cnvManaged = False}
testObject_ConvTeamInfo_user_10 :: ConvTeamInfo
testObject_ConvTeamInfo_user_10 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "0000001d-0000-0009-0000-007500000037"))), cnvManaged = False}
testObject_ConvTeamInfo_user_11 :: ConvTeamInfo
testObject_ConvTeamInfo_user_11 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "0000007f-0000-006d-0000-002d00000046"))), cnvManaged = False}
testObject_ConvTeamInfo_user_12 :: ConvTeamInfo
testObject_ConvTeamInfo_user_12 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000002-0000-0066-0000-003000000045"))), cnvManaged = False}
testObject_ConvTeamInfo_user_13 :: ConvTeamInfo
testObject_ConvTeamInfo_user_13 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000053-0000-002d-0000-001b0000006e"))), cnvManaged = False}
testObject_ConvTeamInfo_user_14 :: ConvTeamInfo
testObject_ConvTeamInfo_user_14 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000052-0000-0019-0000-001000000078"))), cnvManaged = True}
testObject_ConvTeamInfo_user_15 :: ConvTeamInfo
testObject_ConvTeamInfo_user_15 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000072-0000-0076-0000-007e0000005b"))), cnvManaged = True}
testObject_ConvTeamInfo_user_16 :: ConvTeamInfo
testObject_ConvTeamInfo_user_16 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000073-0000-0044-0000-00070000003f"))), cnvManaged = True}
testObject_ConvTeamInfo_user_17 :: ConvTeamInfo
testObject_ConvTeamInfo_user_17 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000011-0000-0018-0000-002f0000006b"))), cnvManaged = False}
testObject_ConvTeamInfo_user_18 :: ConvTeamInfo
testObject_ConvTeamInfo_user_18 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0041-0000-001900000050"))), cnvManaged = True}
testObject_ConvTeamInfo_user_19 :: ConvTeamInfo
testObject_ConvTeamInfo_user_19 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000065-0000-0007-0000-006500000035"))), cnvManaged = True}
testObject_ConvTeamInfo_user_20 :: ConvTeamInfo
testObject_ConvTeamInfo_user_20 = ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000057-0000-0037-0000-007b00000051"))), cnvManaged = True}
