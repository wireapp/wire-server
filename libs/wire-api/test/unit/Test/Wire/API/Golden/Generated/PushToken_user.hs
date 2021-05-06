{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.PushToken_user where

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
testObject_PushToken_user_1 :: PushToken
testObject_PushToken_user_1 = (pushToken (GCM) (AppName {appNameText = "\48117y\78416e\26034o"}) (Token {tokenText = "\CAN\ETB\EOTwpb"}) (ClientId {client = "3"}))
testObject_PushToken_user_2 :: PushToken
testObject_PushToken_user_2 = (pushToken (APNSVoIP) (AppName {appNameText = ""}) (Token {tokenText = "\1034984\1064200\NUL"}) (ClientId {client = "4"}))
testObject_PushToken_user_3 :: PushToken
testObject_PushToken_user_3 = (pushToken (APNSVoIP) (AppName {appNameText = "q.\DC3$!"}) (Token {tokenText = ",\"2\1085045\1009508"}) (ClientId {client = "5"}))
testObject_PushToken_user_4 :: PushToken
testObject_PushToken_user_4 = (pushToken (GCM) (AppName {appNameText = "\CAN^"}) (Token {tokenText = "\5155H\1065213_\NAK+"}) (ClientId {client = "15"}))
testObject_PushToken_user_5 :: PushToken
testObject_PushToken_user_5 = (pushToken (APNSVoIP) (AppName {appNameText = "\ETB\1050060{"}) (Token {tokenText = "\12231c\GSj;^"}) (ClientId {client = "10"}))
