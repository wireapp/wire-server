{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ConversationReceiptModeUpdate_user where

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
testObject_ConversationReceiptModeUpdate_user_1 :: ConversationReceiptModeUpdate
testObject_ConversationReceiptModeUpdate_user_1 = ConversationReceiptModeUpdate {cruReceiptMode = ReceiptMode {unReceiptMode = 15184}}
testObject_ConversationReceiptModeUpdate_user_2 :: ConversationReceiptModeUpdate
testObject_ConversationReceiptModeUpdate_user_2 = ConversationReceiptModeUpdate {cruReceiptMode = ReceiptMode {unReceiptMode = -14118}}
testObject_ConversationReceiptModeUpdate_user_3 :: ConversationReceiptModeUpdate
testObject_ConversationReceiptModeUpdate_user_3 = ConversationReceiptModeUpdate {cruReceiptMode = ReceiptMode {unReceiptMode = 9890}}
testObject_ConversationReceiptModeUpdate_user_4 :: ConversationReceiptModeUpdate
testObject_ConversationReceiptModeUpdate_user_4 = ConversationReceiptModeUpdate {cruReceiptMode = ReceiptMode {unReceiptMode = 10496}}
testObject_ConversationReceiptModeUpdate_user_5 :: ConversationReceiptModeUpdate
testObject_ConversationReceiptModeUpdate_user_5 = ConversationReceiptModeUpdate {cruReceiptMode = ReceiptMode {unReceiptMode = -15427}}
testObject_ConversationReceiptModeUpdate_user_6 :: ConversationReceiptModeUpdate
testObject_ConversationReceiptModeUpdate_user_6 = ConversationReceiptModeUpdate {cruReceiptMode = ReceiptMode {unReceiptMode = 15285}}
testObject_ConversationReceiptModeUpdate_user_7 :: ConversationReceiptModeUpdate
testObject_ConversationReceiptModeUpdate_user_7 = ConversationReceiptModeUpdate {cruReceiptMode = ReceiptMode {unReceiptMode = 7440}}
testObject_ConversationReceiptModeUpdate_user_8 :: ConversationReceiptModeUpdate
testObject_ConversationReceiptModeUpdate_user_8 = ConversationReceiptModeUpdate {cruReceiptMode = ReceiptMode {unReceiptMode = 116}}
testObject_ConversationReceiptModeUpdate_user_9 :: ConversationReceiptModeUpdate
testObject_ConversationReceiptModeUpdate_user_9 = ConversationReceiptModeUpdate {cruReceiptMode = ReceiptMode {unReceiptMode = 9279}}
testObject_ConversationReceiptModeUpdate_user_10 :: ConversationReceiptModeUpdate
testObject_ConversationReceiptModeUpdate_user_10 = ConversationReceiptModeUpdate {cruReceiptMode = ReceiptMode {unReceiptMode = -4534}}
testObject_ConversationReceiptModeUpdate_user_11 :: ConversationReceiptModeUpdate
testObject_ConversationReceiptModeUpdate_user_11 = ConversationReceiptModeUpdate {cruReceiptMode = ReceiptMode {unReceiptMode = 10746}}
testObject_ConversationReceiptModeUpdate_user_12 :: ConversationReceiptModeUpdate
testObject_ConversationReceiptModeUpdate_user_12 = ConversationReceiptModeUpdate {cruReceiptMode = ReceiptMode {unReceiptMode = 3300}}
testObject_ConversationReceiptModeUpdate_user_13 :: ConversationReceiptModeUpdate
testObject_ConversationReceiptModeUpdate_user_13 = ConversationReceiptModeUpdate {cruReceiptMode = ReceiptMode {unReceiptMode = -1158}}
testObject_ConversationReceiptModeUpdate_user_14 :: ConversationReceiptModeUpdate
testObject_ConversationReceiptModeUpdate_user_14 = ConversationReceiptModeUpdate {cruReceiptMode = ReceiptMode {unReceiptMode = 16348}}
testObject_ConversationReceiptModeUpdate_user_15 :: ConversationReceiptModeUpdate
testObject_ConversationReceiptModeUpdate_user_15 = ConversationReceiptModeUpdate {cruReceiptMode = ReceiptMode {unReceiptMode = -4835}}
testObject_ConversationReceiptModeUpdate_user_16 :: ConversationReceiptModeUpdate
testObject_ConversationReceiptModeUpdate_user_16 = ConversationReceiptModeUpdate {cruReceiptMode = ReceiptMode {unReceiptMode = -11295}}
testObject_ConversationReceiptModeUpdate_user_17 :: ConversationReceiptModeUpdate
testObject_ConversationReceiptModeUpdate_user_17 = ConversationReceiptModeUpdate {cruReceiptMode = ReceiptMode {unReceiptMode = -697}}
testObject_ConversationReceiptModeUpdate_user_18 :: ConversationReceiptModeUpdate
testObject_ConversationReceiptModeUpdate_user_18 = ConversationReceiptModeUpdate {cruReceiptMode = ReceiptMode {unReceiptMode = 6710}}
testObject_ConversationReceiptModeUpdate_user_19 :: ConversationReceiptModeUpdate
testObject_ConversationReceiptModeUpdate_user_19 = ConversationReceiptModeUpdate {cruReceiptMode = ReceiptMode {unReceiptMode = 12191}}
testObject_ConversationReceiptModeUpdate_user_20 :: ConversationReceiptModeUpdate
testObject_ConversationReceiptModeUpdate_user_20 = ConversationReceiptModeUpdate {cruReceiptMode = ReceiptMode {unReceiptMode = -12388}}
