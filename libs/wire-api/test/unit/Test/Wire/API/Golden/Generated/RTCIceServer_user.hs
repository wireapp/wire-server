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
testObject_RTCIceServer_user_1 = (rtcIceServer ((turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Just TransportTCP)) :| [(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "153.97.86.164"))) (read "2") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Nothing)),(turnURI (SchemeTurns) (TurnHostName "123") (read "2") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostName "a-c") (read "0") (Nothing)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "214.71.124.120"))) (read "1") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostName "a-c") (read "1") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "183.208.103.143"))) (read "1") (Just TransportTCP))]) ((turnUsername (secondsToNominalDiffTime (252.000000000000)) ("81h3d2ptr") & tuVersion .~ (3) & tuKeyindex .~ (22) & tuT .~ ('\51711'))) ((fromRight undefined (validate ("WZ1UER/Tnk+g")))))
testObject_RTCIceServer_user_2 :: RTCIceServer
testObject_RTCIceServer_user_2 = (rtcIceServer ((turnURI (SchemeTurns) (TurnHostName "007.com") (read "1") (Just TransportTCP)) :| [(turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "9.200.216.174"))) (read "2") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "4.5.140.49"))) (read "2") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostName "a-c") (read "0") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "195.151.145.109"))) (read "2") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostName "a-c") (read "0") (Nothing)),(turnURI (SchemeTurns) (TurnHostName "host.name") (read "1") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "167.186.32.168"))) (read "2") (Just TransportUDP))]) ((turnUsername (secondsToNominalDiffTime (32.000000000000)) ("6xmaga6hbx") & tuVersion .~ (6) & tuKeyindex .~ (31) & tuT .~ ('\\'))) ((fromRight undefined (validate ("6Hasff6GED0=")))))
testObject_RTCIceServer_user_3 :: RTCIceServer
testObject_RTCIceServer_user_3 = (rtcIceServer ((turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Just TransportTCP)) :| [(turnURI (SchemeTurn) (TurnHostName "007.com") (read "0") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostName "123") (read "0") (Nothing))]) ((turnUsername (secondsToNominalDiffTime (34.000000000000)) ("u1i") & tuVersion .~ (9) & tuKeyindex .~ (21) & tuT .~ ('\NAK'))) ((fromRight undefined (validate ("/p5J/zOaZcY=")))))
testObject_RTCIceServer_user_4 :: RTCIceServer
testObject_RTCIceServer_user_4 = (rtcIceServer ((turnURI (SchemeTurn) (TurnHostName "host.name") (read "2") (Nothing)) :| [(turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "81.237.30.189"))) (read "2") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostName "007.com") (read "1") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostName "host.name") (read "1") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostName "a-c") (read "1") (Nothing)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "47.193.188.53"))) (read "1") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "10.38.145.127"))) (read "2") (Just TransportUDP))]) ((turnUsername (secondsToNominalDiffTime (224.000000000000)) ("muaowi") & tuVersion .~ (9) & tuKeyindex .~ (2) & tuT .~ ('\ETX'))) ((fromRight undefined (validate ("FKiR")))))
testObject_RTCIceServer_user_5 :: RTCIceServer
testObject_RTCIceServer_user_5 = (rtcIceServer ((turnURI (SchemeTurn) (TurnHostName "123") (read "0") (Just TransportUDP)) :| [(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "228.186.162.4"))) (read "1") (Nothing))]) ((turnUsername (secondsToNominalDiffTime (84.000000000000)) ("3qfjnj4w") & tuVersion .~ (6) & tuKeyindex .~ (16) & tuT .~ ('2'))) ((fromRight undefined (validate ("PCa0ESYb")))))
testObject_RTCIceServer_user_6 :: RTCIceServer
testObject_RTCIceServer_user_6 = (rtcIceServer ((turnURI (SchemeTurn) (TurnHostName "host.name") (read "1") (Just TransportUDP)) :| [(turnURI (SchemeTurn) (TurnHostName "host.name") (read "1") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostName "007.com") (read "2") (Nothing))]) ((turnUsername (secondsToNominalDiffTime (72.000000000000)) ("glwp") & tuVersion .~ (8) & tuKeyindex .~ (8) & tuT .~ ('\175386'))) ((fromRight undefined (validate ("iC8nAA==")))))
testObject_RTCIceServer_user_7 :: RTCIceServer
testObject_RTCIceServer_user_7 = (rtcIceServer ((turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "29.20.10.121"))) (read "2") (Nothing)) :| [(turnURI (SchemeTurn) (TurnHostName "a-c") (read "0") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "236.161.51.114"))) (read "1") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostName "123") (read "1") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "213.102.66.137"))) (read "1") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostName "a-c") (read "1") (Nothing)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "90.128.139.171"))) (read "2") (Nothing)),(turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "63.48.236.40"))) (read "2") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostName "123") (read "2") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "2") (Nothing))]) ((turnUsername (secondsToNominalDiffTime (140.000000000000)) ("g5tp0") & tuVersion .~ (0) & tuKeyindex .~ (18) & tuT .~ ('\NUL'))) ((fromRight undefined (validate ("fxmk")))))
testObject_RTCIceServer_user_8 :: RTCIceServer
testObject_RTCIceServer_user_8 = (rtcIceServer ((turnURI (SchemeTurns) (TurnHostName "host.name") (read "1") (Just TransportTCP)) :| [(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "59.219.104.241"))) (read "0") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostName "007.com") (read "1") (Just TransportTCP))]) ((turnUsername (secondsToNominalDiffTime (60.000000000000)) ("4roscbl83") & tuVersion .~ (0) & tuKeyindex .~ (19) & tuT .~ ('j'))) ((fromRight undefined (validate ("JUhkCSELEpmm")))))
testObject_RTCIceServer_user_9 :: RTCIceServer
testObject_RTCIceServer_user_9 = (rtcIceServer ((turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "180.113.100.13"))) (read "0") (Nothing)) :| [(turnURI (SchemeTurn) (TurnHostName "123") (read "2") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostName "123") (read "1") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "124.17.68.255"))) (read "2") (Nothing)),(turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Nothing)),(turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "213.237.73.16"))) (read "0") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostName "host.name") (read "1") (Nothing)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "62.0.36.89"))) (read "2") (Just TransportTCP))]) ((turnUsername (secondsToNominalDiffTime (34.000000000000)) ("332i") & tuVersion .~ (8) & tuKeyindex .~ (12) & tuT .~ ('7'))) ((fromRight undefined (validate ("47tl")))))
testObject_RTCIceServer_user_10 :: RTCIceServer
testObject_RTCIceServer_user_10 = (rtcIceServer ((turnURI (SchemeTurn) (TurnHostName "host.name") (read "0") (Nothing)) :| [(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "49.236.216.112"))) (read "2") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostName "host.name") (read "1") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "246.59.87.161"))) (read "1") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostName "123") (read "0") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostName "007.com") (read "0") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostName "a-c") (read "1") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostName "123") (read "0") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "255.59.67.146"))) (read "0") (Just TransportTCP))]) ((turnUsername (secondsToNominalDiffTime (177.000000000000)) ("dtkkh594") & tuVersion .~ (0) & tuKeyindex .~ (4) & tuT .~ ('\1057967'))) ((fromRight undefined (validate ("z5U=")))))
testObject_RTCIceServer_user_11 :: RTCIceServer
testObject_RTCIceServer_user_11 = (rtcIceServer ((turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "88.136.173.113"))) (read "0") (Just TransportTCP)) :| [(turnURI (SchemeTurns) (TurnHostName "host.name") (read "0") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "16.113.134.161"))) (read "2") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostName "123") (read "2") (Nothing)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "15.140.146.142"))) (read "0") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostName "a-c") (read "2") (Nothing)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "7.56.85.242"))) (read "0") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "64.254.150.248"))) (read "1") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostName "host.name") (read "2") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostName "a-c") (read "0") (Just TransportTCP))]) ((turnUsername (secondsToNominalDiffTime (240.000000000000)) ("exd9") & tuVersion .~ (0) & tuKeyindex .~ (15) & tuT .~ ('R'))) ((fromRight undefined (validate ("1yscsB/2")))))
testObject_RTCIceServer_user_12 :: RTCIceServer
testObject_RTCIceServer_user_12 = (rtcIceServer ((turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "64.165.167.234"))) (read "2") (Just TransportUDP)) :| [(turnURI (SchemeTurns) (TurnHostName "007.com") (read "0") (Nothing)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "148.197.93.118"))) (read "0") (Nothing)),(turnURI (SchemeTurn) (TurnHostName "host.name") (read "1") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostName "123") (read "0") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostName "123") (read "1") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostName "007.com") (read "2") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostName "007.com") (read "1") (Just TransportTCP))]) ((turnUsername (secondsToNominalDiffTime (24.000000000000)) ("z257ce") & tuVersion .~ (6) & tuKeyindex .~ (29) & tuT .~ ('1'))) ((fromRight undefined (validate ("k3WmRDKFZIY=")))))
testObject_RTCIceServer_user_13 :: RTCIceServer
testObject_RTCIceServer_user_13 = (rtcIceServer ((turnURI (SchemeTurn) (TurnHostName "007.com") (read "0") (Just TransportTCP)) :| [(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "153.179.137.7"))) (read "0") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostName "007.com") (read "0") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostName "host.name") (read "0") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "232.12.104.208"))) (read "2") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostName "host.name") (read "1") (Nothing)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "46.36.64.190"))) (read "0") (Nothing)),(turnURI (SchemeTurns) (TurnHostName "123") (read "1") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostName "a-c") (read "0") (Just TransportTCP))]) ((turnUsername (secondsToNominalDiffTime (7.000000000000)) ("32a") & tuVersion .~ (10) & tuKeyindex .~ (12) & tuT .~ (')'))) ((fromRight undefined (validate ("piyLzoWH")))))
testObject_RTCIceServer_user_14 :: RTCIceServer
testObject_RTCIceServer_user_14 = (rtcIceServer ((turnURI (SchemeTurn) (TurnHostName "123") (read "0") (Nothing)) :| [(turnURI (SchemeTurn) (TurnHostName "007.com") (read "1") (Nothing)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "176.138.212.242"))) (read "0") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostName "123") (read "0") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostName "host.name") (read "1") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostName "123") (read "1") (Nothing)),(turnURI (SchemeTurn) (TurnHostName "host.name") (read "0") (Just TransportUDP))]) ((turnUsername (secondsToNominalDiffTime (136.000000000000)) ("ak4fwzin") & tuVersion .~ (10) & tuKeyindex .~ (22) & tuT .~ ('Q'))) ((fromRight undefined (validate ("hIH/PPEC+fsR")))))
testObject_RTCIceServer_user_15 :: RTCIceServer
testObject_RTCIceServer_user_15 = (rtcIceServer ((turnURI (SchemeTurns) (TurnHostName "007.com") (read "2") (Nothing)) :| [(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "86.213.215.44"))) (read "0") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostName "a-c") (read "0") (Just TransportUDP))]) ((turnUsername (secondsToNominalDiffTime (160.000000000000)) ("a") & tuVersion .~ (6) & tuKeyindex .~ (1) & tuT .~ ('\ETX'))) ((fromRight undefined (validate ("rH01wgZ8DUs=")))))
testObject_RTCIceServer_user_16 :: RTCIceServer
testObject_RTCIceServer_user_16 = (rtcIceServer ((turnURI (SchemeTurn) (TurnHostName "123") (read "2") (Just TransportTCP)) :| [(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "212.178.102.100"))) (read "0") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostName "123") (read "1") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "2") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "29.154.232.140"))) (read "1") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "179.223.166.221"))) (read "1") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "252.230.183.95"))) (read "0") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostName "host.name") (read "1") (Just TransportTCP))]) ((turnUsername (secondsToNominalDiffTime (193.000000000000)) ("l") & tuVersion .~ (0) & tuKeyindex .~ (22) & tuT .~ (';'))) ((fromRight undefined (validate ("aBLEGt6R/A==")))))
testObject_RTCIceServer_user_17 :: RTCIceServer
testObject_RTCIceServer_user_17 = (rtcIceServer ((turnURI (SchemeTurns) (TurnHostName "host.name") (read "1") (Just TransportUDP)) :| [(turnURI (SchemeTurn) (TurnHostName "host.name") (read "0") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostName "host.name") (read "1") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostName "host.name") (read "0") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "165.116.78.60"))) (read "0") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "34.40.105.178"))) (read "2") (Nothing)),(turnURI (SchemeTurn) (TurnHostName "123") (read "2") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostName "a-c") (read "0") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostName "a-c") (read "0") (Nothing)),(turnURI (SchemeTurns) (TurnHostName "host.name") (read "2") (Just TransportUDP))]) ((turnUsername (secondsToNominalDiffTime (144.000000000000)) ("yioea") & tuVersion .~ (9) & tuKeyindex .~ (6) & tuT .~ ('F'))) ((fromRight undefined (validate ("QcgRoDj9lA==")))))
testObject_RTCIceServer_user_18 :: RTCIceServer
testObject_RTCIceServer_user_18 = (rtcIceServer ((turnURI (SchemeTurns) (TurnHostName "host.name") (read "0") (Nothing)) :| [(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "247.84.247.86"))) (read "2") (Just TransportUDP))]) ((turnUsername (secondsToNominalDiffTime (103.000000000000)) ("8bie6wue7p") & tuVersion .~ (0) & tuKeyindex .~ (7) & tuT .~ ('7'))) ((fromRight undefined (validate ("AFs=")))))
testObject_RTCIceServer_user_19 :: RTCIceServer
testObject_RTCIceServer_user_19 = (rtcIceServer ((turnURI (SchemeTurns) (TurnHostName "123") (read "2") (Nothing)) :| [(turnURI (SchemeTurns) (TurnHostName "host.name") (read "1") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostName "007.com") (read "0") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostName "007.com") (read "2") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostName "123") (read "2") (Nothing))]) ((turnUsername (secondsToNominalDiffTime (53.000000000000)) ("3tk023i5pw") & tuVersion .~ (0) & tuKeyindex .~ (4) & tuT .~ ('\35153'))) ((fromRight undefined (validate ("qax2")))))
testObject_RTCIceServer_user_20 :: RTCIceServer
testObject_RTCIceServer_user_20 = (rtcIceServer ((turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "35.212.134.188"))) (read "1") (Just TransportUDP)) :| []) ((turnUsername (secondsToNominalDiffTime (87.000000000000)) ("jf40a5") & tuVersion .~ (0) & tuKeyindex .~ (30) & tuT .~ ('['))) ((fromRight undefined (validate ("Bxy5")))))
