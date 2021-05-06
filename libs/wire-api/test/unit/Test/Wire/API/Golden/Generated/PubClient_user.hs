{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.PubClient_user where

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
testObject_PubClient_user_1 :: PubClient
testObject_PubClient_user_1 = PubClient {pubClientId = ClientId {client = "252"}, pubClientClass = Nothing}
testObject_PubClient_user_2 :: PubClient
testObject_PubClient_user_2 = PubClient {pubClientId = ClientId {client = "9e2"}, pubClientClass = Just TabletClient}
testObject_PubClient_user_3 :: PubClient
testObject_PubClient_user_3 = PubClient {pubClientId = ClientId {client = "bab"}, pubClientClass = Just LegalHoldClient}
testObject_PubClient_user_4 :: PubClient
testObject_PubClient_user_4 = PubClient {pubClientId = ClientId {client = "3e6"}, pubClientClass = Just LegalHoldClient}
testObject_PubClient_user_5 :: PubClient
testObject_PubClient_user_5 = PubClient {pubClientId = ClientId {client = "fa1"}, pubClientClass = Just PhoneClient}
testObject_PubClient_user_6 :: PubClient
testObject_PubClient_user_6 = PubClient {pubClientId = ClientId {client = "2fb"}, pubClientClass = Nothing}
testObject_PubClient_user_7 :: PubClient
testObject_PubClient_user_7 = PubClient {pubClientId = ClientId {client = "8e8"}, pubClientClass = Just PhoneClient}
testObject_PubClient_user_8 :: PubClient
testObject_PubClient_user_8 = PubClient {pubClientId = ClientId {client = "a8a"}, pubClientClass = Just LegalHoldClient}
testObject_PubClient_user_9 :: PubClient
testObject_PubClient_user_9 = PubClient {pubClientId = ClientId {client = "f6e"}, pubClientClass = Nothing}
testObject_PubClient_user_10 :: PubClient
testObject_PubClient_user_10 = PubClient {pubClientId = ClientId {client = "47b"}, pubClientClass = Nothing}
testObject_PubClient_user_11 :: PubClient
testObject_PubClient_user_11 = PubClient {pubClientId = ClientId {client = "e7b"}, pubClientClass = Just LegalHoldClient}
testObject_PubClient_user_12 :: PubClient
testObject_PubClient_user_12 = PubClient {pubClientId = ClientId {client = "57e"}, pubClientClass = Nothing}
testObject_PubClient_user_13 :: PubClient
testObject_PubClient_user_13 = PubClient {pubClientId = ClientId {client = "e30"}, pubClientClass = Just LegalHoldClient}
testObject_PubClient_user_14 :: PubClient
testObject_PubClient_user_14 = PubClient {pubClientId = ClientId {client = "5a9"}, pubClientClass = Just TabletClient}
testObject_PubClient_user_15 :: PubClient
testObject_PubClient_user_15 = PubClient {pubClientId = ClientId {client = "2b0"}, pubClientClass = Just DesktopClient}
testObject_PubClient_user_16 :: PubClient
testObject_PubClient_user_16 = PubClient {pubClientId = ClientId {client = "300"}, pubClientClass = Nothing}
testObject_PubClient_user_17 :: PubClient
testObject_PubClient_user_17 = PubClient {pubClientId = ClientId {client = "7b"}, pubClientClass = Nothing}
testObject_PubClient_user_18 :: PubClient
testObject_PubClient_user_18 = PubClient {pubClientId = ClientId {client = "849"}, pubClientClass = Just TabletClient}
testObject_PubClient_user_19 :: PubClient
testObject_PubClient_user_19 = PubClient {pubClientId = ClientId {client = "a6d"}, pubClientClass = Just PhoneClient}
testObject_PubClient_user_20 :: PubClient
testObject_PubClient_user_20 = PubClient {pubClientId = ClientId {client = "ec0"}, pubClientClass = Just LegalHoldClient}
