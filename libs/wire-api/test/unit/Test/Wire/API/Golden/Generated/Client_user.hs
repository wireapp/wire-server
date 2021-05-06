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
testObject_Client_user_1 = Client {clientId = ClientId {client = "3"}, clientType = PermanentClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-11T10:48:10.472Z")), clientClass = Just DesktopClient, clientLabel = Just "", clientCookie = Just (CookieLabel {cookieLabelText = "\172603"}), clientLocation = Just (location (Latitude (-4.327044821240934e-2)) (Longitude (0.19908265229212985))), clientModel = Just "s\a@"}
testObject_Client_user_2 :: Client
testObject_Client_user_2 = Client {clientId = ClientId {client = "0"}, clientType = TemporaryClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-09T12:26:03.214Z")), clientClass = Nothing, clientLabel = Just "", clientCookie = Just (CookieLabel {cookieLabelText = "#"}), clientLocation = Just (location (Latitude (-1.9674603428100124)) (Longitude (0.55761939414223))), clientModel = Just "\DC3"}
testObject_Client_user_3 :: Client
testObject_Client_user_3 = Client {clientId = ClientId {client = "3"}, clientType = PermanentClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-12T00:38:43.544Z")), clientClass = Just TabletClient, clientLabel = Just "H_", clientCookie = Just (CookieLabel {cookieLabelText = "\1102861\996585\RS"}), clientLocation = Nothing, clientModel = Just "\1009616\DLE\1095176"}
testObject_Client_user_4 :: Client
testObject_Client_user_4 = Client {clientId = ClientId {client = "3"}, clientType = TemporaryClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-12T04:00:09.267Z")), clientClass = Nothing, clientLabel = Just "#", clientCookie = Just (CookieLabel {cookieLabelText = "\f"}), clientLocation = Just (location (Latitude (0.15455718379396607)) (Longitude (-0.37568305373649014))), clientModel = Just "\33569"}
testObject_Client_user_5 :: Client
testObject_Client_user_5 = Client {clientId = ClientId {client = "3"}, clientType = TemporaryClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-09T07:30:56.101Z")), clientClass = Just PhoneClient, clientLabel = Just "`", clientCookie = Nothing, clientLocation = Just (location (Latitude (-1.8063615293208488)) (Longitude (-1.1556448661515193))), clientModel = Just "\176695"}
