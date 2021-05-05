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
testObject_RTCIceServer_1 :: RTCIceServer
testObject_RTCIceServer_1 = (rtcIceServer ((turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "56.34.228.63"))) (read "2") (Just TransportTCP)) :| [(turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "2") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostName "007.com") (read "1") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostName "host.name") (read "2") (Just TransportUDP))]) ((turnUsername (secondsToNominalDiffTime (209.000000000000)) ("by") & tuVersion .~ (7) & tuKeyindex .~ (23) & tuT .~ ('R'))) ((fromRight undefined (validate ("oW+uSg==")))))
testObject_RTCIceServer_2 :: RTCIceServer
testObject_RTCIceServer_2 = (rtcIceServer ((turnURI (SchemeTurns) (TurnHostName "host.name") (read "1") (Just TransportTCP)) :| [(turnURI (SchemeTurn) (TurnHostName "a-c") (read "1") (Just TransportUDP))]) ((turnUsername (secondsToNominalDiffTime (65.000000000000)) ("z8n6t1aw9h") & tuVersion .~ (7) & tuKeyindex .~ (14) & tuT .~ ('E'))) ((fromRight undefined (validate ("")))))
testObject_RTCIceServer_3 :: RTCIceServer
testObject_RTCIceServer_3 = (rtcIceServer ((turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "232.24.192.76"))) (read "2") (Nothing)) :| [(turnURI (SchemeTurns) (TurnHostName "host.name") (read "1") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostName "a-c") (read "1") (Nothing)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "3.7.140.63"))) (read "1") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "192.232.33.230"))) (read "0") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostName "123") (read "1") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "130.191.25.2"))) (read "1") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostName "a-c") (read "1") (Nothing)),(turnURI (SchemeTurn) (TurnHostName "007.com") (read "0") (Nothing)),(turnURI (SchemeTurns) (TurnHostName "a-c") (read "1") (Nothing)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "249.146.184.201"))) (read "0") (Just TransportUDP))]) ((turnUsername (secondsToNominalDiffTime (106.000000000000)) ("m1909fnxx") & tuVersion .~ (0) & tuKeyindex .~ (12) & tuT .~ ('\44353'))) ((fromRight undefined (validate ("k8q4uQ==")))))
testObject_RTCIceServer_4 :: RTCIceServer
testObject_RTCIceServer_4 = (rtcIceServer ((turnURI (SchemeTurn) (TurnHostName "host.name") (read "0") (Nothing)) :| [(turnURI (SchemeTurn) (TurnHostName "host.name") (read "0") (Nothing)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "242.181.117.90"))) (read "1") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Nothing)),(turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "2") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostName "host.name") (read "1") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "26.229.173.145"))) (read "1") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostName "a-c") (read "2") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "2") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostName "host.name") (read "2") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "177.6.88.224"))) (read "2") (Just TransportUDP))]) ((turnUsername (secondsToNominalDiffTime (158.000000000000)) ("wixscwbcpz") & tuVersion .~ (9) & tuKeyindex .~ (13) & tuT .~ ('\1064764'))) ((fromRight undefined (validate ("FeIhjmpo")))))
testObject_RTCIceServer_5 :: RTCIceServer
testObject_RTCIceServer_5 = (rtcIceServer ((turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "31.167.243.70"))) (read "0") (Just TransportUDP)) :| [(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "180.172.131.37"))) (read "1") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostName "007.com") (read "0") (Nothing)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "58.220.5.234"))) (read "0") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "59.140.245.41"))) (read "1") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "170.97.209.241"))) (read "1") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "105.67.55.0"))) (read "1") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "30.3.90.175"))) (read "0") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "72.135.127.42"))) (read "0") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "43.80.231.82"))) (read "2") (Just TransportTCP))]) ((turnUsername (secondsToNominalDiffTime (147.000000000000)) ("ty2y9ozz") & tuVersion .~ (9) & tuKeyindex .~ (11) & tuT .~ ('\1098042'))) ((fromRight undefined (validate ("fw==")))))
