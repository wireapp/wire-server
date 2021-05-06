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
testObject_RTCIceServer_user_1 = (rtcIceServer ((turnURI (SchemeTurns) (TurnHostName "123") (read "1") (Just TransportTCP)) :| [(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "162.141.143.127"))) (read "1") (Nothing)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "164.228.74.183"))) (read "2") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "75.0.46.63"))) (read "2") (Nothing)),(turnURI (SchemeTurns) (TurnHostName "007.com") (read "2") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "6.126.123.94"))) (read "1") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostName "host.name") (read "1") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "206.139.34.235"))) (read "2") (Nothing)),(turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "2") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostName "host.name") (read "1") (Just TransportUDP))]) ((turnUsername (secondsToNominalDiffTime (1.000000000000)) ("0appzac8ku") & tuVersion .~ (6) & tuKeyindex .~ (25) & tuT .~ ('h'))) ((fromRight undefined (validate ("jQS8")))))
testObject_RTCIceServer_user_2 :: RTCIceServer
testObject_RTCIceServer_user_2 = (rtcIceServer ((turnURI (SchemeTurn) (TurnHostName "host.name") (read "2") (Just TransportTCP)) :| []) ((turnUsername (secondsToNominalDiffTime (70.000000000000)) ("uwgo") & tuVersion .~ (1) & tuKeyindex .~ (32) & tuT .~ ('g'))) ((fromRight undefined (validate ("GYMvaJlr")))))
testObject_RTCIceServer_user_3 :: RTCIceServer
testObject_RTCIceServer_user_3 = (rtcIceServer ((turnURI (SchemeTurns) (TurnHostName "a-c") (read "1") (Just TransportTCP)) :| [(turnURI (SchemeTurn) (TurnHostName "a-c") (read "0") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostName "123") (read "0") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostName "a-c") (read "1") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Just TransportTCP))]) ((turnUsername (secondsToNominalDiffTime (87.000000000000)) ("8fb") & tuVersion .~ (7) & tuKeyindex .~ (20) & tuT .~ ('Y'))) ((fromRight undefined (validate ("B96N0/oHdg==")))))
testObject_RTCIceServer_user_4 :: RTCIceServer
testObject_RTCIceServer_user_4 = (rtcIceServer ((turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "207.7.240.112"))) (read "1") (Just TransportTCP)) :| [(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "24.106.50.116"))) (read "0") (Nothing)),(turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Nothing)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "12.83.146.112"))) (read "2") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "21.243.186.56"))) (read "2") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostName "007.com") (read "0") (Nothing)),(turnURI (SchemeTurns) (TurnHostName "123") (read "1") (Nothing)),(turnURI (SchemeTurn) (TurnHostName "a-c") (read "0") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "248.215.139.207"))) (read "1") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostName "a-c") (read "0") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostName "a-c") (read "0") (Just TransportUDP))]) ((turnUsername (secondsToNominalDiffTime (162.000000000000)) ("w0yi4agjk") & tuVersion .~ (1) & tuKeyindex .~ (1) & tuT .~ ('@'))) ((fromRight undefined (validate ("hw==")))))
testObject_RTCIceServer_user_5 :: RTCIceServer
testObject_RTCIceServer_user_5 = (rtcIceServer ((turnURI (SchemeTurns) (TurnHostName "host.name") (read "0") (Just TransportTCP)) :| [(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "54.247.43.102"))) (read "1") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "96.202.113.13"))) (read "1") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostName "host.name") (read "2") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "2") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostName "a-c") (read "2") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostName "123") (read "2") (Nothing)),(turnURI (SchemeTurns) (TurnHostName "host.name") (read "1") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostName "123") (read "0") (Just TransportTCP))]) ((turnUsername (secondsToNominalDiffTime (56.000000000000)) ("e9aed0") & tuVersion .~ (7) & tuKeyindex .~ (2) & tuT .~ ('\1055383'))) ((fromRight undefined (validate ("4+kzVHNiGg==")))))
testObject_RTCIceServer_user_6 :: RTCIceServer
testObject_RTCIceServer_user_6 = (rtcIceServer ((turnURI (SchemeTurns) (TurnHostName "007.com") (read "2") (Just TransportTCP)) :| [(turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Nothing))]) ((turnUsername (secondsToNominalDiffTime (144.000000000000)) ("hdmjw8k4i2") & tuVersion .~ (5) & tuKeyindex .~ (2) & tuT .~ (']'))) ((fromRight undefined (validate ("AphWt4g=")))))
testObject_RTCIceServer_user_7 :: RTCIceServer
testObject_RTCIceServer_user_7 = (rtcIceServer ((turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "2.200.242.242"))) (read "1") (Just TransportUDP)) :| []) ((turnUsername (secondsToNominalDiffTime (124.000000000000)) ("ic1") & tuVersion .~ (3) & tuKeyindex .~ (3) & tuT .~ ('\SYN'))) ((fromRight undefined (validate ("")))))
testObject_RTCIceServer_user_8 :: RTCIceServer
testObject_RTCIceServer_user_8 = (rtcIceServer ((turnURI (SchemeTurns) (TurnHostName "a-c") (read "0") (Just TransportTCP)) :| [(turnURI (SchemeTurn) (TurnHostName "007.com") (read "1") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostName "007.com") (read "0") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "2") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "223.101.30.220"))) (read "0") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "60.85.42.38"))) (read "1") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostName "123") (read "1") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "200.242.127.148"))) (read "2") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Nothing)),(turnURI (SchemeTurns) (TurnHostName "007.com") (read "0") (Nothing)),(turnURI (SchemeTurn) (TurnHostName "host.name") (read "2") (Just TransportTCP))]) ((turnUsername (secondsToNominalDiffTime (11.000000000000)) ("qs23rmev") & tuVersion .~ (0) & tuKeyindex .~ (7) & tuT .~ ('p'))) ((fromRight undefined (validate ("/IsGDaE=")))))
testObject_RTCIceServer_user_9 :: RTCIceServer
testObject_RTCIceServer_user_9 = (rtcIceServer ((turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "63.60.172.226"))) (read "2") (Just TransportTCP)) :| [(turnURI (SchemeTurns) (TurnHostName "007.com") (read "0") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostName "host.name") (read "2") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostName "007.com") (read "1") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostName "host.name") (read "1") (Nothing))]) ((turnUsername (secondsToNominalDiffTime (75.000000000000)) ("4ilkj2") & tuVersion .~ (8) & tuKeyindex .~ (24) & tuT .~ ('%'))) ((fromRight undefined (validate ("")))))
testObject_RTCIceServer_user_10 :: RTCIceServer
testObject_RTCIceServer_user_10 = (rtcIceServer ((turnURI (SchemeTurns) (TurnHostName "a-c") (read "1") (Just TransportUDP)) :| [(turnURI (SchemeTurn) (TurnHostName "a-c") (read "1") (Nothing)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "64.140.255.31"))) (read "0") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostName "a-c") (read "2") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostName "host.name") (read "1") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostName "a-c") (read "2") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostName "host.name") (read "0") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "203.245.4.85"))) (read "0") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostName "123") (read "2") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostName "007.com") (read "2") (Just TransportUDP))]) ((turnUsername (secondsToNominalDiffTime (122.000000000000)) ("7j92xodt5") & tuVersion .~ (6) & tuKeyindex .~ (24) & tuT .~ ('*'))) ((fromRight undefined (validate ("VgHHSRkn0lKT")))))
testObject_RTCIceServer_user_11 :: RTCIceServer
testObject_RTCIceServer_user_11 = (rtcIceServer ((turnURI (SchemeTurn) (TurnHostName "a-c") (read "2") (Nothing)) :| [(turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "2") (Nothing)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "68.32.206.25"))) (read "1") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostName "host.name") (read "1") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "192.98.25.27"))) (read "0") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "247.241.227.185"))) (read "2") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostName "host.name") (read "2") (Nothing)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "244.232.95.7"))) (read "0") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostName "a-c") (read "1") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "2") (Just TransportTCP))]) ((turnUsername (secondsToNominalDiffTime (138.000000000000)) ("wx8k7bo") & tuVersion .~ (5) & tuKeyindex .~ (16) & tuT .~ ('('))) ((fromRight undefined (validate ("i4WAlsVXSiHh")))))
testObject_RTCIceServer_user_12 :: RTCIceServer
testObject_RTCIceServer_user_12 = (rtcIceServer ((turnURI (SchemeTurns) (TurnHostName "a-c") (read "2") (Just TransportUDP)) :| [(turnURI (SchemeTurns) (TurnHostName "123") (read "1") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostName "host.name") (read "1") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostName "a-c") (read "1") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostName "a-c") (read "0") (Just TransportUDP))]) ((turnUsername (secondsToNominalDiffTime (41.000000000000)) ("c8") & tuVersion .~ (5) & tuKeyindex .~ (26) & tuT .~ ('\97359'))) ((fromRight undefined (validate ("")))))
testObject_RTCIceServer_user_13 :: RTCIceServer
testObject_RTCIceServer_user_13 = (rtcIceServer ((turnURI (SchemeTurn) (TurnHostName "007.com") (read "2") (Just TransportUDP)) :| [(turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostName "a-c") (read "2") (Nothing)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "188.216.47.238"))) (read "2") (Nothing))]) ((turnUsername (secondsToNominalDiffTime (226.000000000000)) ("k") & tuVersion .~ (6) & tuKeyindex .~ (25) & tuT .~ ('2'))) ((fromRight undefined (validate ("5Z30VeE=")))))
testObject_RTCIceServer_user_14 :: RTCIceServer
testObject_RTCIceServer_user_14 = (rtcIceServer ((turnURI (SchemeTurns) (TurnHostName "a-c") (read "0") (Just TransportTCP)) :| [(turnURI (SchemeTurns) (TurnHostName "007.com") (read "2") (Nothing)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "0.139.187.45"))) (read "2") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostName "a-c") (read "0") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostName "a-c") (read "0") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostName "host.name") (read "0") (Nothing))]) ((turnUsername (secondsToNominalDiffTime (171.000000000000)) ("5p") & tuVersion .~ (10) & tuKeyindex .~ (22) & tuT .~ ('\179350'))) ((fromRight undefined (validate ("aCKhfFhGP79O")))))
testObject_RTCIceServer_user_15 :: RTCIceServer
testObject_RTCIceServer_user_15 = (rtcIceServer ((turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "2") (Just TransportUDP)) :| [(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "157.172.63.144"))) (read "0") (Nothing)),(turnURI (SchemeTurns) (TurnHostName "007.com") (read "0") (Nothing)),(turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "63.217.224.208"))) (read "0") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostName "123") (read "2") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostName "123") (read "1") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostName "host.name") (read "0") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "189.42.242.18"))) (read "2") (Just TransportUDP))]) ((turnUsername (secondsToNominalDiffTime (22.000000000000)) ("f88p14") & tuVersion .~ (8) & tuKeyindex .~ (6) & tuT .~ ('\RS'))) ((fromRight undefined (validate ("AwvK+ds=")))))
testObject_RTCIceServer_user_16 :: RTCIceServer
testObject_RTCIceServer_user_16 = (rtcIceServer ((turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "117.158.116.2"))) (read "1") (Just TransportUDP)) :| []) ((turnUsername (secondsToNominalDiffTime (180.000000000000)) ("7s") & tuVersion .~ (10) & tuKeyindex .~ (17) & tuT .~ ('d'))) ((fromRight undefined (validate ("qsdq")))))
testObject_RTCIceServer_user_17 :: RTCIceServer
testObject_RTCIceServer_user_17 = (rtcIceServer ((turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "2") (Nothing)) :| [(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "46.152.140.138"))) (read "1") (Nothing))]) ((turnUsername (secondsToNominalDiffTime (8.000000000000)) ("dw") & tuVersion .~ (6) & tuKeyindex .~ (10) & tuT .~ ('k'))) ((fromRight undefined (validate ("ZfBGAuWepHNj")))))
testObject_RTCIceServer_user_18 :: RTCIceServer
testObject_RTCIceServer_user_18 = (rtcIceServer ((turnURI (SchemeTurn) (TurnHostName "host.name") (read "1") (Just TransportTCP)) :| [(turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "224.236.22.185"))) (read "2") (Nothing)),(turnURI (SchemeTurn) (TurnHostName "host.name") (read "1") (Just TransportUDP))]) ((turnUsername (secondsToNominalDiffTime (181.000000000000)) ("i") & tuVersion .~ (3) & tuKeyindex .~ (30) & tuT .~ ('\EM'))) ((fromRight undefined (validate ("D+o=")))))
testObject_RTCIceServer_user_19 :: RTCIceServer
testObject_RTCIceServer_user_19 = (rtcIceServer ((turnURI (SchemeTurn) (TurnHostName "007.com") (read "1") (Just TransportUDP)) :| [(turnURI (SchemeTurn) (TurnHostName "007.com") (read "1") (Nothing))]) ((turnUsername (secondsToNominalDiffTime (175.000000000000)) ("7734lxu2mo") & tuVersion .~ (4) & tuKeyindex .~ (26) & tuT .~ ('}'))) ((fromRight undefined (validate ("aHU+CA==")))))
testObject_RTCIceServer_user_20 :: RTCIceServer
testObject_RTCIceServer_user_20 = (rtcIceServer ((turnURI (SchemeTurn) (TurnHostName "a-c") (read "1") (Nothing)) :| [(turnURI (SchemeTurns) (TurnHostName "host.name") (read "0") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostName "123") (read "0") (Just TransportTCP))]) ((turnUsername (secondsToNominalDiffTime (105.000000000000)) ("i0ab") & tuVersion .~ (10) & tuKeyindex .~ (22) & tuT .~ (' '))) ((fromRight undefined (validate ("yr8T5SCH")))))
