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
testObject_Client_user_1 :: Client
testObject_Client_user_1 = Client {clientId = ClientId {client = "4"}, clientType = TemporaryClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-06T07:11:56.530Z")), clientClass = Just TabletClient, clientLabel = Just "d\1050830\b", clientCookie = Nothing, clientLocation = Just (location (Latitude (-0.7029632619992906)) (Longitude (2.5935243840093456))), clientModel = Nothing}
testObject_Client_user_2 :: Client
testObject_Client_user_2 = Client {clientId = ClientId {client = "0"}, clientType = LegalHoldClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-06T06:25:01.513Z")), clientClass = Nothing, clientLabel = Just "W\DEL)", clientCookie = Just (CookieLabel {cookieLabelText = "1YS"}), clientLocation = Just (location (Latitude (1.6358358529817063)) (Longitude (2.3664305713835523))), clientModel = Just "8L"}
testObject_Client_user_3 :: Client
testObject_Client_user_3 = Client {clientId = ClientId {client = "3"}, clientType = TemporaryClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-08T18:46:24.824Z")), clientClass = Nothing, clientLabel = Just "m", clientCookie = Just (CookieLabel {cookieLabelText = ""}), clientLocation = Just (location (Latitude (0.45614100051534984)) (Longitude (-1.098542708064097))), clientModel = Just "4{_"}
testObject_Client_user_4 :: Client
testObject_Client_user_4 = Client {clientId = ClientId {client = "3"}, clientType = PermanentClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-10T03:16:57.903Z")), clientClass = Just TabletClient, clientLabel = Just "", clientCookie = Just (CookieLabel {cookieLabelText = "!\ETX"}), clientLocation = Just (location (Latitude (-0.20735922570922874)) (Longitude (-0.9785703894431976))), clientModel = Nothing}
testObject_Client_user_5 :: Client
testObject_Client_user_5 = Client {clientId = ClientId {client = "3"}, clientType = TemporaryClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-12T03:28:12.415Z")), clientClass = Just PhoneClient, clientLabel = Just "\141916\983948", clientCookie = Just (CookieLabel {cookieLabelText = "m\171188"}), clientLocation = Just (location (Latitude (-2.1785959693998063)) (Longitude (1.1364016242178772))), clientModel = Just "l!"}
testObject_Client_user_6 :: Client
testObject_Client_user_6 = Client {clientId = ClientId {client = "4"}, clientType = TemporaryClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-06T01:40:23.358Z")), clientClass = Nothing, clientLabel = Just "", clientCookie = Just (CookieLabel {cookieLabelText = "mE"}), clientLocation = Just (location (Latitude (-1.688768177286179)) (Longitude (-2.84313928636116))), clientModel = Just ""}
testObject_Client_user_7 :: Client
testObject_Client_user_7 = Client {clientId = ClientId {client = "0"}, clientType = PermanentClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-06T10:28:03.442Z")), clientClass = Just TabletClient, clientLabel = Nothing, clientCookie = Just (CookieLabel {cookieLabelText = "=+y"}), clientLocation = Just (location (Latitude (-2.7656911011654306)) (Longitude (-1.9964486578758405))), clientModel = Just ""}
testObject_Client_user_8 :: Client
testObject_Client_user_8 = Client {clientId = ClientId {client = "1"}, clientType = PermanentClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-08T18:22:48.496Z")), clientClass = Just DesktopClient, clientLabel = Just "\EOT", clientCookie = Just (CookieLabel {cookieLabelText = "^"}), clientLocation = Just (location (Latitude (-2.9321594896046212)) (Longitude (1.3464455447957007))), clientModel = Just "\DC1VI"}
testObject_Client_user_9 :: Client
testObject_Client_user_9 = Client {clientId = ClientId {client = "2"}, clientType = PermanentClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-07T17:45:12.128Z")), clientClass = Just PhoneClient, clientLabel = Just "", clientCookie = Just (CookieLabel {cookieLabelText = "\158965"}), clientLocation = Nothing, clientModel = Just "#\125072\1027491"}
testObject_Client_user_10 :: Client
testObject_Client_user_10 = Client {clientId = ClientId {client = "2"}, clientType = LegalHoldClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-09T18:34:53.928Z")), clientClass = Just LegalHoldClient, clientLabel = Just ";ae", clientCookie = Just (CookieLabel {cookieLabelText = ""}), clientLocation = Just (location (Latitude (0.33091556222623103)) (Longitude (1.8799235651851092))), clientModel = Nothing}
testObject_Client_user_11 :: Client
testObject_Client_user_11 = Client {clientId = ClientId {client = "4"}, clientType = PermanentClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-07T05:23:58.913Z")), clientClass = Just PhoneClient, clientLabel = Just "{\120072", clientCookie = Just (CookieLabel {cookieLabelText = ""}), clientLocation = Just (location (Latitude (1.75252304161672)) (Longitude (1.741261279918145))), clientModel = Just "\19832"}
testObject_Client_user_12 :: Client
testObject_Client_user_12 = Client {clientId = ClientId {client = "2"}, clientType = TemporaryClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-06T01:06:04.687Z")), clientClass = Just PhoneClient, clientLabel = Just "f\ESC\183044", clientCookie = Just (CookieLabel {cookieLabelText = "C\1029269"}), clientLocation = Just (location (Latitude (7.843762281502281e-2)) (Longitude (-0.49541094653565865))), clientModel = Just "x"}
testObject_Client_user_13 :: Client
testObject_Client_user_13 = Client {clientId = ClientId {client = "1"}, clientType = LegalHoldClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-06T23:06:44.251Z")), clientClass = Just PhoneClient, clientLabel = Just "L", clientCookie = Just (CookieLabel {cookieLabelText = ""}), clientLocation = Just (location (Latitude (0.7244100553768098)) (Longitude (-0.25879979535005565))), clientModel = Just "\147436[t"}
testObject_Client_user_14 :: Client
testObject_Client_user_14 = Client {clientId = ClientId {client = "1"}, clientType = TemporaryClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-08T21:08:33.124Z")), clientClass = Nothing, clientLabel = Nothing, clientCookie = Just (CookieLabel {cookieLabelText = "K"}), clientLocation = Just (location (Latitude (1.4191607375221862)) (Longitude (-0.12151361588389611))), clientModel = Just "7\151148\DEL"}
testObject_Client_user_15 :: Client
testObject_Client_user_15 = Client {clientId = ClientId {client = "4"}, clientType = LegalHoldClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-09T15:18:19.265Z")), clientClass = Nothing, clientLabel = Just "I\188473S", clientCookie = Just (CookieLabel {cookieLabelText = "N\150364"}), clientLocation = Just (location (Latitude (0.7017809821578086)) (Longitude (0.25672624043729586))), clientModel = Just "\92321H\77921"}
testObject_Client_user_16 :: Client
testObject_Client_user_16 = Client {clientId = ClientId {client = "4"}, clientType = LegalHoldClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-11T19:26:39.048Z")), clientClass = Nothing, clientLabel = Just "\120413N", clientCookie = Nothing, clientLocation = Just (location (Latitude (-2.6152969321804616)) (Longitude (-2.885611189676324))), clientModel = Nothing}
testObject_Client_user_17 :: Client
testObject_Client_user_17 = Client {clientId = ClientId {client = "4"}, clientType = LegalHoldClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-06T12:55:05.230Z")), clientClass = Just LegalHoldClient, clientLabel = Nothing, clientCookie = Just (CookieLabel {cookieLabelText = "\98815\bZ"}), clientLocation = Nothing, clientModel = Just "\GS"}
testObject_Client_user_18 :: Client
testObject_Client_user_18 = Client {clientId = ClientId {client = "1"}, clientType = LegalHoldClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-12T01:50:44.484Z")), clientClass = Just PhoneClient, clientLabel = Just "\145857\94711", clientCookie = Just (CookieLabel {cookieLabelText = "\1097204U\8374"}), clientLocation = Just (location (Latitude (-1.7740219772801895)) (Longitude (-1.1551059611515313))), clientModel = Just ""}
testObject_Client_user_19 :: Client
testObject_Client_user_19 = Client {clientId = ClientId {client = "4"}, clientType = TemporaryClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-11T13:10:37.573Z")), clientClass = Just TabletClient, clientLabel = Nothing, clientCookie = Nothing, clientLocation = Just (location (Latitude (-0.9337599011413406)) (Longitude (-2.1944347434923617))), clientModel = Just "\EOT"}
testObject_Client_user_20 :: Client
testObject_Client_user_20 = Client {clientId = ClientId {client = "4"}, clientType = PermanentClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-11T10:42:18.997Z")), clientClass = Just DesktopClient, clientLabel = Nothing, clientCookie = Nothing, clientLocation = Just (location (Latitude (-1.2200930894518824)) (Longitude (2.3541040545167817))), clientModel = Nothing}
