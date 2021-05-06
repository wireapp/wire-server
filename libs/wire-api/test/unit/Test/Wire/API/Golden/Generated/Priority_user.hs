{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Priority_user where

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
testObject_Priority_user_1 :: Priority
testObject_Priority_user_1 = HighPriority
testObject_Priority_user_2 :: Priority
testObject_Priority_user_2 = HighPriority
testObject_Priority_user_3 :: Priority
testObject_Priority_user_3 = HighPriority
testObject_Priority_user_4 :: Priority
testObject_Priority_user_4 = HighPriority
testObject_Priority_user_5 :: Priority
testObject_Priority_user_5 = LowPriority
testObject_Priority_user_6 :: Priority
testObject_Priority_user_6 = LowPriority
testObject_Priority_user_7 :: Priority
testObject_Priority_user_7 = HighPriority
testObject_Priority_user_8 :: Priority
testObject_Priority_user_8 = HighPriority
testObject_Priority_user_9 :: Priority
testObject_Priority_user_9 = HighPriority
testObject_Priority_user_10 :: Priority
testObject_Priority_user_10 = HighPriority
testObject_Priority_user_11 :: Priority
testObject_Priority_user_11 = HighPriority
testObject_Priority_user_12 :: Priority
testObject_Priority_user_12 = LowPriority
testObject_Priority_user_13 :: Priority
testObject_Priority_user_13 = LowPriority
testObject_Priority_user_14 :: Priority
testObject_Priority_user_14 = HighPriority
testObject_Priority_user_15 :: Priority
testObject_Priority_user_15 = LowPriority
testObject_Priority_user_16 :: Priority
testObject_Priority_user_16 = LowPriority
testObject_Priority_user_17 :: Priority
testObject_Priority_user_17 = HighPriority
testObject_Priority_user_18 :: Priority
testObject_Priority_user_18 = LowPriority
testObject_Priority_user_19 :: Priority
testObject_Priority_user_19 = HighPriority
testObject_Priority_user_20 :: Priority
testObject_Priority_user_20 = LowPriority
