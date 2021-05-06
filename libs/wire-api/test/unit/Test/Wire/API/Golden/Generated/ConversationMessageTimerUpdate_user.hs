{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ConversationMessageTimerUpdate_user where

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
testObject_ConversationMessageTimerUpdate_user_1 :: ConversationMessageTimerUpdate
testObject_ConversationMessageTimerUpdate_user_1 = ConversationMessageTimerUpdate {cupMessageTimer = Just (Ms {ms = 3718831199668390})}
testObject_ConversationMessageTimerUpdate_user_2 :: ConversationMessageTimerUpdate
testObject_ConversationMessageTimerUpdate_user_2 = ConversationMessageTimerUpdate {cupMessageTimer = Nothing}
testObject_ConversationMessageTimerUpdate_user_3 :: ConversationMessageTimerUpdate
testObject_ConversationMessageTimerUpdate_user_3 = ConversationMessageTimerUpdate {cupMessageTimer = Just (Ms {ms = 8276757615804350})}
testObject_ConversationMessageTimerUpdate_user_4 :: ConversationMessageTimerUpdate
testObject_ConversationMessageTimerUpdate_user_4 = ConversationMessageTimerUpdate {cupMessageTimer = Just (Ms {ms = 8990679926717935})}
testObject_ConversationMessageTimerUpdate_user_5 :: ConversationMessageTimerUpdate
testObject_ConversationMessageTimerUpdate_user_5 = ConversationMessageTimerUpdate {cupMessageTimer = Just (Ms {ms = 7444694319371822})}
testObject_ConversationMessageTimerUpdate_user_6 :: ConversationMessageTimerUpdate
testObject_ConversationMessageTimerUpdate_user_6 = ConversationMessageTimerUpdate {cupMessageTimer = Nothing}
testObject_ConversationMessageTimerUpdate_user_7 :: ConversationMessageTimerUpdate
testObject_ConversationMessageTimerUpdate_user_7 = ConversationMessageTimerUpdate {cupMessageTimer = Nothing}
testObject_ConversationMessageTimerUpdate_user_8 :: ConversationMessageTimerUpdate
testObject_ConversationMessageTimerUpdate_user_8 = ConversationMessageTimerUpdate {cupMessageTimer = Just (Ms {ms = 2736406071397183})}
testObject_ConversationMessageTimerUpdate_user_9 :: ConversationMessageTimerUpdate
testObject_ConversationMessageTimerUpdate_user_9 = ConversationMessageTimerUpdate {cupMessageTimer = Just (Ms {ms = 7120513985074754})}
testObject_ConversationMessageTimerUpdate_user_10 :: ConversationMessageTimerUpdate
testObject_ConversationMessageTimerUpdate_user_10 = ConversationMessageTimerUpdate {cupMessageTimer = Just (Ms {ms = 6385791917950266})}
testObject_ConversationMessageTimerUpdate_user_11 :: ConversationMessageTimerUpdate
testObject_ConversationMessageTimerUpdate_user_11 = ConversationMessageTimerUpdate {cupMessageTimer = Nothing}
testObject_ConversationMessageTimerUpdate_user_12 :: ConversationMessageTimerUpdate
testObject_ConversationMessageTimerUpdate_user_12 = ConversationMessageTimerUpdate {cupMessageTimer = Just (Ms {ms = 5854527436104484})}
testObject_ConversationMessageTimerUpdate_user_13 :: ConversationMessageTimerUpdate
testObject_ConversationMessageTimerUpdate_user_13 = ConversationMessageTimerUpdate {cupMessageTimer = Nothing}
testObject_ConversationMessageTimerUpdate_user_14 :: ConversationMessageTimerUpdate
testObject_ConversationMessageTimerUpdate_user_14 = ConversationMessageTimerUpdate {cupMessageTimer = Nothing}
testObject_ConversationMessageTimerUpdate_user_15 :: ConversationMessageTimerUpdate
testObject_ConversationMessageTimerUpdate_user_15 = ConversationMessageTimerUpdate {cupMessageTimer = Just (Ms {ms = 8776085809545631})}
testObject_ConversationMessageTimerUpdate_user_16 :: ConversationMessageTimerUpdate
testObject_ConversationMessageTimerUpdate_user_16 = ConversationMessageTimerUpdate {cupMessageTimer = Nothing}
testObject_ConversationMessageTimerUpdate_user_17 :: ConversationMessageTimerUpdate
testObject_ConversationMessageTimerUpdate_user_17 = ConversationMessageTimerUpdate {cupMessageTimer = Nothing}
testObject_ConversationMessageTimerUpdate_user_18 :: ConversationMessageTimerUpdate
testObject_ConversationMessageTimerUpdate_user_18 = ConversationMessageTimerUpdate {cupMessageTimer = Just (Ms {ms = 3226343098238712})}
testObject_ConversationMessageTimerUpdate_user_19 :: ConversationMessageTimerUpdate
testObject_ConversationMessageTimerUpdate_user_19 = ConversationMessageTimerUpdate {cupMessageTimer = Just (Ms {ms = 1630928790590149})}
testObject_ConversationMessageTimerUpdate_user_20 :: ConversationMessageTimerUpdate
testObject_ConversationMessageTimerUpdate_user_20 = ConversationMessageTimerUpdate {cupMessageTimer = Nothing}
