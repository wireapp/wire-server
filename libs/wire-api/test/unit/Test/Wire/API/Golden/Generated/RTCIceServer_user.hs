{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.RTCIceServer_user where

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
testObject_RTCIceServer_user_1 :: RTCIceServer
testObject_RTCIceServer_user_1 = (rtcIceServer ((turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "127.79.195.77"))) (read "1") (Just TransportTCP)) :| [(turnURI (SchemeTurns) (TurnHostName "123") (read "0") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostName "123") (read "0") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostName "123") (read "1") (Nothing)),(turnURI (SchemeTurn) (TurnHostName "123") (read "0") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "2") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostName "a-c") (read "2") (Nothing)),(turnURI (SchemeTurn) (TurnHostName "007.com") (read "1") (Just TransportTCP))]) ((turnUsername (secondsToNominalDiffTime (199.000000000000)) ("pw0pzunurh") & tuVersion .~ (0) & tuKeyindex .~ (31) & tuT .~ ('+'))) ((fromRight undefined (validate ("")))))
testObject_RTCIceServer_user_2 :: RTCIceServer
testObject_RTCIceServer_user_2 = (rtcIceServer ((turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Just TransportUDP)) :| []) ((turnUsername (secondsToNominalDiffTime (198.000000000000)) ("4j61") & tuVersion .~ (8) & tuKeyindex .~ (3) & tuT .~ ('\SYN'))) ((fromRight undefined (validate ("HSGuBRg=")))))
testObject_RTCIceServer_user_3 :: RTCIceServer
testObject_RTCIceServer_user_3 = (rtcIceServer ((turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "189.156.80.106"))) (read "0") (Just TransportUDP)) :| [(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "84.58.1.27"))) (read "1") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "2") (Nothing)),(turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "2") (Nothing)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "25.154.184.232"))) (read "1") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostName "123") (read "0") (Nothing)),(turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "2") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostName "a-c") (read "1") (Just TransportUDP))]) ((turnUsername (secondsToNominalDiffTime (145.000000000000)) ("gxwyztc") & tuVersion .~ (0) & tuKeyindex .~ (27) & tuT .~ ('x'))) ((fromRight undefined (validate ("CXB0txcoU+Mv")))))
testObject_RTCIceServer_user_4 :: RTCIceServer
testObject_RTCIceServer_user_4 = (rtcIceServer ((turnURI (SchemeTurns) (TurnHostName "host.name") (read "2") (Just TransportTCP)) :| [(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "22.6.91.65"))) (read "2") (Nothing)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "21.53.200.99"))) (read "0") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostName "a-c") (read "1") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "147.246.188.21"))) (read "2") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostName "007.com") (read "0") (Just TransportTCP))]) ((turnUsername (secondsToNominalDiffTime (101.000000000000)) ("0q4faa") & tuVersion .~ (9) & tuKeyindex .~ (14) & tuT .~ ('\EM'))) ((fromRight undefined (validate ("HEq759g=")))))
testObject_RTCIceServer_user_5 :: RTCIceServer
testObject_RTCIceServer_user_5 = (rtcIceServer ((turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "247.149.133.226"))) (read "1") (Just TransportUDP)) :| [(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "218.10.114.93"))) (read "0") (Nothing)),(turnURI (SchemeTurns) (TurnHostName "007.com") (read "1") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "2") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Nothing)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "142.174.194.142"))) (read "2") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostName "host.name") (read "0") (Nothing)),(turnURI (SchemeTurns) (TurnHostName "007.com") (read "1") (Just TransportTCP))]) ((turnUsername (secondsToNominalDiffTime (135.000000000000)) ("ikp") & tuVersion .~ (8) & tuKeyindex .~ (10) & tuT .~ ('\128381'))) ((fromRight undefined (validate ("5+1c")))))
