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
testObject_Client_user_1 = Client {clientId = ClientId {client = "0"}, clientType = PermanentClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-09T13:44:16.232Z")), clientClass = Nothing, clientLabel = Just "", clientCookie = Just (CookieLabel {cookieLabelText = ";\1111624k"}), clientLocation = Just (location (Latitude (-1.271053429832247)) (Longitude (2.435406495481607))), clientModel = Nothing}
testObject_Client_user_2 :: Client
testObject_Client_user_2 = Client {clientId = ClientId {client = "2"}, clientType = PermanentClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-06T04:41:31.389Z")), clientClass = Nothing, clientLabel = Just "", clientCookie = Just (CookieLabel {cookieLabelText = "\NUL"}), clientLocation = Just (location (Latitude (0.4463093477463767)) (Longitude (-1.0460403962565947))), clientModel = Just ":?"}
testObject_Client_user_3 :: Client
testObject_Client_user_3 = Client {clientId = ClientId {client = "3"}, clientType = LegalHoldClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-08T08:15:52.671Z")), clientClass = Just DesktopClient, clientLabel = Just "", clientCookie = Just (CookieLabel {cookieLabelText = ""}), clientLocation = Just (location (Latitude (0.19177209532241626)) (Longitude (-0.5215927173916979))), clientModel = Nothing}
testObject_Client_user_4 :: Client
testObject_Client_user_4 = Client {clientId = ClientId {client = "3"}, clientType = TemporaryClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-07T11:44:39.763Z")), clientClass = Nothing, clientLabel = Just "L:\162426", clientCookie = Nothing, clientLocation = Just (location (Latitude (-1.7510596369963514)) (Longitude (1.87642456130842))), clientModel = Just "\DC2"}
testObject_Client_user_5 :: Client
testObject_Client_user_5 = Client {clientId = ClientId {client = "0"}, clientType = PermanentClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-09T18:00:02.755Z")), clientClass = Just PhoneClient, clientLabel = Just "\1060244", clientCookie = Just (CookieLabel {cookieLabelText = ""}), clientLocation = Just (location (Latitude (-2.562187290185654)) (Longitude (0.4829917890661461))), clientModel = Just "\ETX\ESC\ETB"}
testObject_Client_user_6 :: Client
testObject_Client_user_6 = Client {clientId = ClientId {client = "0"}, clientType = LegalHoldClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-12T09:30:03.137Z")), clientClass = Just DesktopClient, clientLabel = Just "+", clientCookie = Nothing, clientLocation = Just (location (Latitude (0.7296647108535151)) (Longitude (1.8723612815836965))), clientModel = Nothing}
testObject_Client_user_7 :: Client
testObject_Client_user_7 = Client {clientId = ClientId {client = "2"}, clientType = PermanentClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-11T12:02:50.984Z")), clientClass = Just LegalHoldClient, clientLabel = Just "", clientCookie = Just (CookieLabel {cookieLabelText = "bR"}), clientLocation = Just (location (Latitude (-1.779295053167598)) (Longitude (2.1775063043176446))), clientModel = Just "y0"}
testObject_Client_user_8 :: Client
testObject_Client_user_8 = Client {clientId = ClientId {client = "1"}, clientType = LegalHoldClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-08T21:43:14.453Z")), clientClass = Nothing, clientLabel = Just "FJ\SOH", clientCookie = Just (CookieLabel {cookieLabelText = "\ACK<Z"}), clientLocation = Just (location (Latitude (0.18537220564767767)) (Longitude (-1.2156896669648716))), clientModel = Just "*K"}
testObject_Client_user_9 :: Client
testObject_Client_user_9 = Client {clientId = ClientId {client = "1"}, clientType = LegalHoldClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-08T13:35:49.728Z")), clientClass = Nothing, clientLabel = Nothing, clientCookie = Just (CookieLabel {cookieLabelText = "\989708N"}), clientLocation = Just (location (Latitude (-0.30513940631197556)) (Longitude (-1.2262326894224325))), clientModel = Just "n\38559"}
testObject_Client_user_10 :: Client
testObject_Client_user_10 = Client {clientId = ClientId {client = "0"}, clientType = PermanentClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-10T02:37:27.834Z")), clientClass = Just PhoneClient, clientLabel = Just "", clientCookie = Just (CookieLabel {cookieLabelText = "\166083}"}), clientLocation = Just (location (Latitude (-0.5047437997722026)) (Longitude (0.9449834317580266))), clientModel = Just "\DC1"}
testObject_Client_user_11 :: Client
testObject_Client_user_11 = Client {clientId = ClientId {client = "0"}, clientType = PermanentClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-06T05:57:03.042Z")), clientClass = Nothing, clientLabel = Just "u\nS", clientCookie = Nothing, clientLocation = Just (location (Latitude (2.0946193893887166)) (Longitude (0.3652025632295467))), clientModel = Just ""}
testObject_Client_user_12 :: Client
testObject_Client_user_12 = Client {clientId = ClientId {client = "1"}, clientType = PermanentClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-10T13:33:38.382Z")), clientClass = Nothing, clientLabel = Just "\40196u", clientCookie = Just (CookieLabel {cookieLabelText = ""}), clientLocation = Just (location (Latitude (2.417516619316501)) (Longitude (-1.6698362431970926))), clientModel = Nothing}
testObject_Client_user_13 :: Client
testObject_Client_user_13 = Client {clientId = ClientId {client = "3"}, clientType = TemporaryClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-12T14:16:57.431Z")), clientClass = Just TabletClient, clientLabel = Nothing, clientCookie = Just (CookieLabel {cookieLabelText = ""}), clientLocation = Just (location (Latitude (1.6884364674159358)) (Longitude (1.5074171953977986))), clientModel = Just "@4\1053467"}
testObject_Client_user_14 :: Client
testObject_Client_user_14 = Client {clientId = ClientId {client = "3"}, clientType = PermanentClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-06T06:43:11.788Z")), clientClass = Just DesktopClient, clientLabel = Just "po\DEL", clientCookie = Nothing, clientLocation = Just (location (Latitude (1.5064725803815533)) (Longitude (-2.9528143406655842))), clientModel = Just "er"}
testObject_Client_user_15 :: Client
testObject_Client_user_15 = Client {clientId = ClientId {client = "2"}, clientType = LegalHoldClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-09T08:18:35.007Z")), clientClass = Nothing, clientLabel = Just "uC", clientCookie = Nothing, clientLocation = Just (location (Latitude (-1.67551777620952)) (Longitude (2.9793899348296944))), clientModel = Nothing}
testObject_Client_user_16 :: Client
testObject_Client_user_16 = Client {clientId = ClientId {client = "3"}, clientType = LegalHoldClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-06T03:12:47.792Z")), clientClass = Nothing, clientLabel = Just "\95667\SUBr", clientCookie = Just (CookieLabel {cookieLabelText = ""}), clientLocation = Nothing, clientModel = Just "\135288\""}
testObject_Client_user_17 :: Client
testObject_Client_user_17 = Client {clientId = ClientId {client = "0"}, clientType = TemporaryClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-10T18:41:46.679Z")), clientClass = Just LegalHoldClient, clientLabel = Just "\ENQ", clientCookie = Just (CookieLabel {cookieLabelText = "\1084082"}), clientLocation = Just (location (Latitude (1.3768141333462518)) (Longitude (-1.2574111397808534))), clientModel = Just "\DEL{"}
testObject_Client_user_18 :: Client
testObject_Client_user_18 = Client {clientId = ClientId {client = "2"}, clientType = TemporaryClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-06T19:45:58.276Z")), clientClass = Just DesktopClient, clientLabel = Just "2\47242", clientCookie = Just (CookieLabel {cookieLabelText = "F\121363\1046840"}), clientLocation = Just (location (Latitude (1.4504306007315932e-2)) (Longitude (-2.8536539959908795))), clientModel = Just "n"}
testObject_Client_user_19 :: Client
testObject_Client_user_19 = Client {clientId = ClientId {client = "2"}, clientType = TemporaryClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-11T10:39:32.627Z")), clientClass = Just LegalHoldClient, clientLabel = Just "J\DC2b", clientCookie = Just (CookieLabel {cookieLabelText = ""}), clientLocation = Just (location (Latitude (1.3238929785196476)) (Longitude (-2.2995312859471593))), clientModel = Just "\74371$"}
testObject_Client_user_20 :: Client
testObject_Client_user_20 = Client {clientId = ClientId {client = "2"}, clientType = TemporaryClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-11T19:24:49.105Z")), clientClass = Just LegalHoldClient, clientLabel = Nothing, clientCookie = Just (CookieLabel {cookieLabelText = "\132854\1034236h"}), clientLocation = Just (location (Latitude (-1.1332032200401911)) (Longitude (-2.346376727338312))), clientModel = Just "`\137703\178800"}
