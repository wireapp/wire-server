{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Client_user where

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
testObject_Client_1 :: Client
testObject_Client_1 = Client {clientId = ClientId {client = "4"}, clientType = PermanentClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-06T11:53:06.606Z")), clientClass = Just PhoneClient, clientLabel = Just "", clientCookie = Just (CookieLabel {cookieLabelText = "\1002711Mh"}), clientLocation = Just (location (Latitude (-2.846461124559934)) (Longitude (1.3486532540294984))), clientModel = Just ""}
testObject_Client_2 :: Client
testObject_Client_2 = Client {clientId = ClientId {client = "1"}, clientType = TemporaryClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-07T06:39:30.331Z")), clientClass = Just LegalHoldClient, clientLabel = Just "+", clientCookie = Nothing, clientLocation = Just (location (Latitude (-2.889593632797622)) (Longitude (0.9803841423546777))), clientModel = Just "\1051503kY"}
testObject_Client_3 :: Client
testObject_Client_3 = Client {clientId = ClientId {client = "2"}, clientType = PermanentClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-07T14:49:24.157Z")), clientClass = Just DesktopClient, clientLabel = Nothing, clientCookie = Just (CookieLabel {cookieLabelText = "{"}), clientLocation = Just (location (Latitude (-2.4716759799667343)) (Longitude (-2.058506902282196))), clientModel = Just "\SOH\EOT"}
testObject_Client_4 :: Client
testObject_Client_4 = Client {clientId = ClientId {client = "3"}, clientType = TemporaryClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-07T15:10:12.048Z")), clientClass = Just LegalHoldClient, clientLabel = Just "", clientCookie = Nothing, clientLocation = Just (location (Latitude (2.071964947216057)) (Longitude (-0.15435597262133263))), clientModel = Just "v\b"}
testObject_Client_5 :: Client
testObject_Client_5 = Client {clientId = ClientId {client = "3"}, clientType = LegalHoldClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-12T16:26:21.247Z")), clientClass = Just LegalHoldClient, clientLabel = Just "\1112758", clientCookie = Nothing, clientLocation = Just (location (Latitude (-2.0537496203636847)) (Longitude (0.8821740532291071))), clientModel = Just "^jS"}
