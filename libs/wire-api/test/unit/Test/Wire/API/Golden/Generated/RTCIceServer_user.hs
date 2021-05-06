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
testObject_RTCIceServer_user_1 = (rtcIceServer ((turnURI (SchemeTurn) (TurnHostName "123") (read "0") (Nothing)) :| [(turnURI (SchemeTurns) (TurnHostName "007.com") (read "0") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "33.113.199.66"))) (read "2") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "178.189.192.139"))) (read "1") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostName "123") (read "0") (Nothing)),(turnURI (SchemeTurns) (TurnHostName "007.com") (read "0") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostName "123") (read "1") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "96.187.82.65"))) (read "0") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostName "123") (read "0") (Nothing)),(turnURI (SchemeTurn) (TurnHostName "host.name") (read "1") (Just TransportTCP))]) ((turnUsername (secondsToNominalDiffTime (167.000000000000)) ("79n1bl8img") & tuVersion .~ (4) & tuKeyindex .~ (12) & tuT .~ ('9'))) ((fromRight undefined (validate ("mioWlG19")))))
testObject_RTCIceServer_user_2 :: RTCIceServer
testObject_RTCIceServer_user_2 = (rtcIceServer ((turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "18.158.231.25"))) (read "0") (Nothing)) :| []) ((turnUsername (secondsToNominalDiffTime (223.000000000000)) ("9qwbsi") & tuVersion .~ (3) & tuKeyindex .~ (22) & tuT .~ ('s'))) ((fromRight undefined (validate ("RKnP+Eyj")))))
testObject_RTCIceServer_user_3 :: RTCIceServer
testObject_RTCIceServer_user_3 = (rtcIceServer ((turnURI (SchemeTurn) (TurnHostName "host.name") (read "2") (Nothing)) :| [(turnURI (SchemeTurn) (TurnHostName "007.com") (read "1") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "122.217.195.160"))) (read "1") (Just TransportTCP))]) ((turnUsername (secondsToNominalDiffTime (98.000000000000)) ("zvrglfc") & tuVersion .~ (1) & tuKeyindex .~ (15) & tuT .~ ('\DC2'))) ((fromRight undefined (validate ("ZdShjPZCkg==")))))
testObject_RTCIceServer_user_4 :: RTCIceServer
testObject_RTCIceServer_user_4 = (rtcIceServer ((turnURI (SchemeTurn) (TurnHostName "a-c") (read "1") (Just TransportTCP)) :| [(turnURI (SchemeTurn) (TurnHostName "007.com") (read "0") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Nothing)),(turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Nothing)),(turnURI (SchemeTurn) (TurnHostName "007.com") (read "1") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostName "a-c") (read "2") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "249.55.108.30"))) (read "2") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Just TransportUDP))]) ((turnUsername (secondsToNominalDiffTime (150.000000000000)) ("dwddy0") & tuVersion .~ (9) & tuKeyindex .~ (3) & tuT .~ ('\ETX'))) ((fromRight undefined (validate ("npk=")))))
testObject_RTCIceServer_user_5 :: RTCIceServer
testObject_RTCIceServer_user_5 = (rtcIceServer ((turnURI (SchemeTurn) (TurnHostName "host.name") (read "0") (Just TransportUDP)) :| [(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "214.254.58.119"))) (read "0") (Nothing)),(turnURI (SchemeTurns) (TurnHostName "007.com") (read "0") (Nothing)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "95.51.203.2"))) (read "2") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostName "host.name") (read "2") (Nothing))]) ((turnUsername (secondsToNominalDiffTime (200.000000000000)) ("0fl6dm") & tuVersion .~ (2) & tuKeyindex .~ (11) & tuT .~ ('\170800'))) ((fromRight undefined (validate ("")))))
testObject_RTCIceServer_user_6 :: RTCIceServer
testObject_RTCIceServer_user_6 = (rtcIceServer ((turnURI (SchemeTurn) (TurnHostName "007.com") (read "2") (Just TransportTCP)) :| [(turnURI (SchemeTurns) (TurnHostName "007.com") (read "0") (Just TransportTCP))]) ((turnUsername (secondsToNominalDiffTime (6.000000000000)) ("6u") & tuVersion .~ (0) & tuKeyindex .~ (17) & tuT .~ ('*'))) ((fromRight undefined (validate ("hmK7+IEqnLMu")))))
testObject_RTCIceServer_user_7 :: RTCIceServer
testObject_RTCIceServer_user_7 = (rtcIceServer ((turnURI (SchemeTurns) (TurnHostName "123") (read "1") (Nothing)) :| [(turnURI (SchemeTurns) (TurnHostName "007.com") (read "1") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Nothing)),(turnURI (SchemeTurns) (TurnHostName "a-c") (read "1") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "255.35.62.61"))) (read "1") (Nothing)),(turnURI (SchemeTurn) (TurnHostName "host.name") (read "0") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "52.132.21.72"))) (read "0") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "178.96.101.49"))) (read "1") (Nothing))]) ((turnUsername (secondsToNominalDiffTime (68.000000000000)) ("1uni") & tuVersion .~ (0) & tuKeyindex .~ (27) & tuT .~ ('D'))) ((fromRight undefined (validate ("qmMsrg==")))))
testObject_RTCIceServer_user_8 :: RTCIceServer
testObject_RTCIceServer_user_8 = (rtcIceServer ((turnURI (SchemeTurn) (TurnHostName "123") (read "2") (Just TransportUDP)) :| [(turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "2") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "181.249.242.148"))) (read "1") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "35.144.8.17"))) (read "0") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Nothing)),(turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "2") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostName "123") (read "0") (Just TransportTCP))]) ((turnUsername (secondsToNominalDiffTime (160.000000000000)) ("ie5") & tuVersion .~ (9) & tuKeyindex .~ (27) & tuT .~ ('W'))) ((fromRight undefined (validate ("Dg==")))))
testObject_RTCIceServer_user_9 :: RTCIceServer
testObject_RTCIceServer_user_9 = (rtcIceServer ((turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "78.85.94.76"))) (read "1") (Nothing)) :| [(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "249.81.163.115"))) (read "2") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "15.30.196.8"))) (read "0") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostName "host.name") (read "0") (Nothing))]) ((turnUsername (secondsToNominalDiffTime (73.000000000000)) ("zd07a") & tuVersion .~ (0) & tuKeyindex .~ (12) & tuT .~ ('&'))) ((fromRight undefined (validate ("dE+z7Q==")))))
testObject_RTCIceServer_user_10 :: RTCIceServer
testObject_RTCIceServer_user_10 = (rtcIceServer ((turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Nothing)) :| [(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "118.110.177.100"))) (read "1") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostName "host.name") (read "1") (Just TransportUDP))]) ((turnUsername (secondsToNominalDiffTime (44.000000000000)) ("rrimdd") & tuVersion .~ (10) & tuKeyindex .~ (15) & tuT .~ ('y'))) ((fromRight undefined (validate ("+Kqra422")))))
testObject_RTCIceServer_user_11 :: RTCIceServer
testObject_RTCIceServer_user_11 = (rtcIceServer ((turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "248.81.21.16"))) (read "2") (Just TransportTCP)) :| [(turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "2") (Just TransportUDP))]) ((turnUsername (secondsToNominalDiffTime (85.000000000000)) ("sg3y3") & tuVersion .~ (8) & tuKeyindex .~ (21) & tuT .~ (']'))) ((fromRight undefined (validate ("vScWQBVRzpw=")))))
testObject_RTCIceServer_user_12 :: RTCIceServer
testObject_RTCIceServer_user_12 = (rtcIceServer ((turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "198.195.57.232"))) (read "2") (Just TransportTCP)) :| [(turnURI (SchemeTurn) (TurnHostName "host.name") (read "0") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "28.50.199.217"))) (read "0") (Nothing)),(turnURI (SchemeTurn) (TurnHostName "007.com") (read "1") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "241.56.177.12"))) (read "0") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "2") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "35.68.251.53"))) (read "0") (Nothing)),(turnURI (SchemeTurns) (TurnHostName "007.com") (read "1") (Nothing)),(turnURI (SchemeTurn) (TurnHostName "a-c") (read "1") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "136.48.112.213"))) (read "1") (Nothing))]) ((turnUsername (secondsToNominalDiffTime (76.000000000000)) ("xb") & tuVersion .~ (1) & tuKeyindex .~ (4) & tuT .~ (' '))) ((fromRight undefined (validate ("BHqs0PmAusbC")))))
testObject_RTCIceServer_user_13 :: RTCIceServer
testObject_RTCIceServer_user_13 = (rtcIceServer ((turnURI (SchemeTurn) (TurnHostName "007.com") (read "1") (Just TransportUDP)) :| []) ((turnUsername (secondsToNominalDiffTime (85.000000000000)) ("zqh") & tuVersion .~ (1) & tuKeyindex .~ (22) & tuT .~ ('\DLE'))) ((fromRight undefined (validate ("CmzV5ZjBMJ4f")))))
testObject_RTCIceServer_user_14 :: RTCIceServer
testObject_RTCIceServer_user_14 = (rtcIceServer ((turnURI (SchemeTurns) (TurnHostName "007.com") (read "1") (Just TransportTCP)) :| [(turnURI (SchemeTurns) (TurnHostName "a-c") (read "1") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "113.97.202.123"))) (read "0") (Nothing)),(turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Nothing)),(turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "130.201.51.246"))) (read "0") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostName "123") (read "2") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "243.106.61.255"))) (read "1") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "14.18.82.39"))) (read "2") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "238.37.173.106"))) (read "2") (Just TransportUDP))]) ((turnUsername (secondsToNominalDiffTime (134.000000000000)) ("cu02n8v") & tuVersion .~ (4) & tuKeyindex .~ (29) & tuT .~ ('{'))) ((fromRight undefined (validate ("HJTkZA==")))))
testObject_RTCIceServer_user_15 :: RTCIceServer
testObject_RTCIceServer_user_15 = (rtcIceServer ((turnURI (SchemeTurn) (TurnHostName "123") (read "0") (Just TransportUDP)) :| [(turnURI (SchemeTurn) (TurnHostName "host.name") (read "0") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "27.6.17.38"))) (read "2") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "184.68.33.12"))) (read "0") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "1.255.139.195"))) (read "1") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "192.110.68.21"))) (read "2") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostName "host.name") (read "0") (Nothing)),(turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "2") (Nothing))]) ((turnUsername (secondsToNominalDiffTime (253.000000000000)) ("mpto") & tuVersion .~ (2) & tuKeyindex .~ (6) & tuT .~ ('G'))) ((fromRight undefined (validate ("viiE/uNXXA==")))))
testObject_RTCIceServer_user_16 :: RTCIceServer
testObject_RTCIceServer_user_16 = (rtcIceServer ((turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "248.34.126.191"))) (read "2") (Just TransportTCP)) :| [(turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "31.35.133.131"))) (read "1") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "146.104.208.130"))) (read "0") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostName "a-c") (read "0") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostName "a-c") (read "2") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostName "007.com") (read "2") (Nothing))]) ((turnUsername (secondsToNominalDiffTime (244.000000000000)) ("fugrcu") & tuVersion .~ (1) & tuKeyindex .~ (24) & tuT .~ ('\1055023'))) ((fromRight undefined (validate ("eMKjKmLbHw==")))))
testObject_RTCIceServer_user_17 :: RTCIceServer
testObject_RTCIceServer_user_17 = (rtcIceServer ((turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "2") (Just TransportUDP)) :| [(turnURI (SchemeTurn) (TurnHostName "a-c") (read "1") (Just TransportUDP))]) ((turnUsername (secondsToNominalDiffTime (80.000000000000)) ("vxpu0bl") & tuVersion .~ (5) & tuKeyindex .~ (26) & tuT .~ ('<'))) ((fromRight undefined (validate ("qRaHutE6")))))
testObject_RTCIceServer_user_18 :: RTCIceServer
testObject_RTCIceServer_user_18 = (rtcIceServer ((turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "2") (Just TransportTCP)) :| [(turnURI (SchemeTurn) (TurnHostName "a-c") (read "2") (Just TransportTCP))]) ((turnUsername (secondsToNominalDiffTime (209.000000000000)) ("l84ik3w") & tuVersion .~ (10) & tuKeyindex .~ (27) & tuT .~ ('\FS'))) ((fromRight undefined (validate ("")))))
testObject_RTCIceServer_user_19 :: RTCIceServer
testObject_RTCIceServer_user_19 = (rtcIceServer ((turnURI (SchemeTurns) (TurnHostName "007.com") (read "0") (Just TransportTCP)) :| [(turnURI (SchemeTurns) (TurnHostName "a-c") (read "2") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "179.128.104.188"))) (read "2") (Nothing)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "218.230.16.236"))) (read "1") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostName "host.name") (read "1") (Just TransportUDP)),(turnURI (SchemeTurns) (TurnHostName "a-c") (read "0") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Just TransportUDP)),(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "99.130.205.189"))) (read "1") (Nothing)),(turnURI (SchemeTurn) (TurnHostName "007.com") (read "1") (Just TransportUDP))]) ((turnUsername (secondsToNominalDiffTime (127.000000000000)) ("bydb") & tuVersion .~ (9) & tuKeyindex .~ (3) & tuT .~ ('\SI'))) ((fromRight undefined (validate ("WzCKUUA/")))))
testObject_RTCIceServer_user_20 :: RTCIceServer
testObject_RTCIceServer_user_20 = (rtcIceServer ((turnURI (SchemeTurns) (TurnHostName "007.com") (read "1") (Nothing)) :| [(turnURI (SchemeTurn) (TurnHostName "a-c") (read "1") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "215.24.222.0"))) (read "2") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostName "a-c") (read "2") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostName "host.name") (read "2") (Just TransportTCP)),(turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Nothing)),(turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "2") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostName "123") (read "1") (Just TransportTCP)),(turnURI (SchemeTurns) (TurnHostName "host.name") (read "1") (Nothing))]) ((turnUsername (secondsToNominalDiffTime (103.000000000000)) ("qtbs") & tuVersion .~ (7) & tuKeyindex .~ (0) & tuT .~ ('\ACK'))) ((fromRight undefined (validate ("Jw==")))))
