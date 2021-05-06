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
testObject_Client_user_1 = Client {clientId = ClientId {client = "1"}, clientType = LegalHoldClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-09T02:44:29.744Z")), clientClass = Just DesktopClient, clientLabel = Just "<g\52613", clientCookie = Just (CookieLabel {cookieLabelText = ""}), clientLocation = Just (location (Latitude (1.8306584723075554)) (Longitude (1.2285698528377957))), clientModel = Nothing}
testObject_Client_user_2 :: Client
testObject_Client_user_2 = Client {clientId = ClientId {client = "4"}, clientType = PermanentClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-08T08:16:34.069Z")), clientClass = Just TabletClient, clientLabel = Just "\DLE\10672", clientCookie = Nothing, clientLocation = Nothing, clientModel = Just ""}
testObject_Client_user_3 :: Client
testObject_Client_user_3 = Client {clientId = ClientId {client = "2"}, clientType = LegalHoldClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-09T12:01:21.224Z")), clientClass = Just LegalHoldClient, clientLabel = Just "", clientCookie = Nothing, clientLocation = Just (location (Latitude (1.8679951339188388)) (Longitude (-2.41050213522754))), clientModel = Just "\131442\""}
testObject_Client_user_4 :: Client
testObject_Client_user_4 = Client {clientId = ClientId {client = "1"}, clientType = TemporaryClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-07T18:15:20.954Z")), clientClass = Just PhoneClient, clientLabel = Nothing, clientCookie = Just (CookieLabel {cookieLabelText = "\DC1\SIW"}), clientLocation = Just (location (Latitude (0.3316069385285121)) (Longitude (-2.64102636737782))), clientModel = Just "Uj\\"}
testObject_Client_user_5 :: Client
testObject_Client_user_5 = Client {clientId = ClientId {client = "4"}, clientType = LegalHoldClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-11T21:52:21.263Z")), clientClass = Just PhoneClient, clientLabel = Just "", clientCookie = Just (CookieLabel {cookieLabelText = "UBm"}), clientLocation = Just (location (Latitude (-0.538767157436715)) (Longitude (-2.1824360080947227))), clientModel = Just "\DC2"}
testObject_Client_user_6 :: Client
testObject_Client_user_6 = Client {clientId = ClientId {client = "4"}, clientType = LegalHoldClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-11T21:34:13.791Z")), clientClass = Just DesktopClient, clientLabel = Just "", clientCookie = Just (CookieLabel {cookieLabelText = "\1080952\ACK\t"}), clientLocation = Nothing, clientModel = Nothing}
testObject_Client_user_7 :: Client
testObject_Client_user_7 = Client {clientId = ClientId {client = "0"}, clientType = TemporaryClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-10T15:53:57.415Z")), clientClass = Just LegalHoldClient, clientLabel = Just "j", clientCookie = Just (CookieLabel {cookieLabelText = "_"}), clientLocation = Just (location (Latitude (-2.7187048173350066)) (Longitude (-1.9364581163804666))), clientModel = Just ""}
testObject_Client_user_8 :: Client
testObject_Client_user_8 = Client {clientId = ClientId {client = "1"}, clientType = LegalHoldClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-09T06:49:54.569Z")), clientClass = Nothing, clientLabel = Just "\1024571\136525%", clientCookie = Just (CookieLabel {cookieLabelText = ","}), clientLocation = Nothing, clientModel = Just "\1077527"}
testObject_Client_user_9 :: Client
testObject_Client_user_9 = Client {clientId = ClientId {client = "3"}, clientType = LegalHoldClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-11T10:06:24.769Z")), clientClass = Just LegalHoldClient, clientLabel = Just "\158881B\ESC", clientCookie = Nothing, clientLocation = Just (location (Latitude (-1.2282344521772326)) (Longitude (-0.26239909672600337))), clientModel = Just "\986109\SOH"}
testObject_Client_user_10 :: Client
testObject_Client_user_10 = Client {clientId = ClientId {client = "0"}, clientType = TemporaryClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-06T06:40:35.083Z")), clientClass = Just PhoneClient, clientLabel = Nothing, clientCookie = Just (CookieLabel {cookieLabelText = "J"}), clientLocation = Just (location (Latitude (-1.941257788744514)) (Longitude (2.2424242909631866))), clientModel = Just ""}
testObject_Client_user_11 :: Client
testObject_Client_user_11 = Client {clientId = ClientId {client = "2"}, clientType = TemporaryClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-06T08:07:16.419Z")), clientClass = Nothing, clientLabel = Just "\1089577\a\24318", clientCookie = Nothing, clientLocation = Just (location (Latitude (0.8115615401942267)) (Longitude (-1.4148228300395214))), clientModel = Just "`"}
testObject_Client_user_12 :: Client
testObject_Client_user_12 = Client {clientId = ClientId {client = "4"}, clientType = TemporaryClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-06T01:17:00.901Z")), clientClass = Just TabletClient, clientLabel = Nothing, clientCookie = Just (CookieLabel {cookieLabelText = "\SOH"}), clientLocation = Just (location (Latitude (1.8748851627985326)) (Longitude (-2.9381510450800166))), clientModel = Just "73"}
testObject_Client_user_13 :: Client
testObject_Client_user_13 = Client {clientId = ClientId {client = "3"}, clientType = TemporaryClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-12T18:59:12.078Z")), clientClass = Nothing, clientLabel = Just "\34921", clientCookie = Nothing, clientLocation = Just (location (Latitude (1.6419852562929074)) (Longitude (0.46521983298826614))), clientModel = Just ""}
testObject_Client_user_14 :: Client
testObject_Client_user_14 = Client {clientId = ClientId {client = "2"}, clientType = PermanentClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-06T11:41:08.073Z")), clientClass = Just PhoneClient, clientLabel = Just "3\1084746", clientCookie = Just (CookieLabel {cookieLabelText = ")"}), clientLocation = Just (location (Latitude (0.4740165691385071)) (Longitude (-0.7507842183494385))), clientModel = Nothing}
testObject_Client_user_15 :: Client
testObject_Client_user_15 = Client {clientId = ClientId {client = "4"}, clientType = PermanentClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-07T15:56:26.487Z")), clientClass = Just LegalHoldClient, clientLabel = Just "v\1048344", clientCookie = Nothing, clientLocation = Just (location (Latitude (-2.4587678840187497)) (Longitude (-6.457121115036067e-2))), clientModel = Nothing}
testObject_Client_user_16 :: Client
testObject_Client_user_16 = Client {clientId = ClientId {client = "2"}, clientType = LegalHoldClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-07T00:03:34.633Z")), clientClass = Nothing, clientLabel = Just "O", clientCookie = Just (CookieLabel {cookieLabelText = "?"}), clientLocation = Nothing, clientModel = Just "U"}
testObject_Client_user_17 :: Client
testObject_Client_user_17 = Client {clientId = ClientId {client = "1"}, clientType = PermanentClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-12T07:31:00.240Z")), clientClass = Just PhoneClient, clientLabel = Just "E\63296z", clientCookie = Just (CookieLabel {cookieLabelText = "\DEL0"}), clientLocation = Just (location (Latitude (-3.768905466418775e-2)) (Longitude (-2.1327955773267653))), clientModel = Just "O\ETX\172590"}
testObject_Client_user_18 :: Client
testObject_Client_user_18 = Client {clientId = ClientId {client = "3"}, clientType = TemporaryClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-10T06:40:49.904Z")), clientClass = Just LegalHoldClient, clientLabel = Nothing, clientCookie = Nothing, clientLocation = Nothing, clientModel = Just ""}
testObject_Client_user_19 :: Client
testObject_Client_user_19 = Client {clientId = ClientId {client = "0"}, clientType = LegalHoldClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-07T12:06:27.521Z")), clientClass = Just TabletClient, clientLabel = Nothing, clientCookie = Just (CookieLabel {cookieLabelText = ""}), clientLocation = Just (location (Latitude (0.676565841949401)) (Longitude (2.4535842826150946))), clientModel = Just "q4"}
testObject_Client_user_20 :: Client
testObject_Client_user_20 = Client {clientId = ClientId {client = "1"}, clientType = TemporaryClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-11T10:12:39.481Z")), clientClass = Nothing, clientLabel = Just "", clientCookie = Nothing, clientLocation = Nothing, clientModel = Just "\SYN6"}
