{-# LANGUAGE OverloadedLists #-}

module Test.Wire.API.Golden.Generated.Client_user where

import Data.Id (ClientId (ClientId, client))
import Data.Json.Util (readUTCTimeMillis)
import Data.Misc
  ( Latitude (Latitude),
    Longitude (Longitude),
    location,
  )
import Imports (Maybe (Just, Nothing), fromJust)
import Wire.API.User.Auth
  ( CookieLabel (CookieLabel, cookieLabelText),
  )
import Wire.API.User.Client
  ( Client (..),
    ClientClass
      ( DesktopClient,
        LegalHoldClient,
        PhoneClient,
        TabletClient
      ),
    ClientType
      ( LegalHoldClientType,
        PermanentClientType,
        TemporaryClientType
      ),
  )

testObject_Client_user_1 :: Client
testObject_Client_user_1 = Client {clientId = ClientId {client = "2"}, clientType = PermanentClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-06T19:39:12.770Z")), clientClass = Just DesktopClient, clientLabel = Just "%*", clientCookie = Nothing, clientLocation = Nothing, clientModel = Just "\995802;\1081067"}

testObject_Client_user_2 :: Client
testObject_Client_user_2 = Client {clientId = ClientId {client = "1"}, clientType = LegalHoldClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-07T08:48:22.537Z")), clientClass = Nothing, clientLabel = Nothing, clientCookie = Just (CookieLabel {cookieLabelText = "\1112890c\1065129"}), clientLocation = Just (location (Latitude (0.6919026326441752)) (Longitude (1.18215529547942))), clientModel = Nothing}

testObject_Client_user_3 :: Client
testObject_Client_user_3 = Client {clientId = ClientId {client = "1"}, clientType = TemporaryClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-07T00:38:22.384Z")), clientClass = Just LegalHoldClient, clientLabel = Just "pi", clientCookie = Just (CookieLabel {cookieLabelText = ""}), clientLocation = Just (location (Latitude (-0.31865405026910076)) (Longitude (6.859482454480745e-2))), clientModel = Nothing}

testObject_Client_user_4 :: Client
testObject_Client_user_4 = Client {clientId = ClientId {client = "3"}, clientType = PermanentClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-06T09:13:45.902Z")), clientClass = Just LegalHoldClient, clientLabel = Nothing, clientCookie = Just (CookieLabel {cookieLabelText = "j"}), clientLocation = Just (location (Latitude (0.43019316470477537)) (Longitude (-2.1994844230432533))), clientModel = Just ""}

testObject_Client_user_5 :: Client
testObject_Client_user_5 = Client {clientId = ClientId {client = "0"}, clientType = TemporaryClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-07T09:07:14.559Z")), clientClass = Just DesktopClient, clientLabel = Nothing, clientCookie = Just (CookieLabel {cookieLabelText = ""}), clientLocation = Just (location (Latitude (-1.505966289957799)) (Longitude (-2.516893825541776))), clientModel = Just "\9015o"}

testObject_Client_user_6 :: Client
testObject_Client_user_6 = Client {clientId = ClientId {client = "4"}, clientType = PermanentClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-08T22:37:53.030Z")), clientClass = Just TabletClient, clientLabel = Nothing, clientCookie = Just (CookieLabel {cookieLabelText = "l\STX"}), clientLocation = Just (location (Latitude (0.3764380360505919)) (Longitude (1.3619562593325738))), clientModel = Just ""}

testObject_Client_user_7 :: Client
testObject_Client_user_7 = Client {clientId = ClientId {client = "4"}, clientType = PermanentClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-07T04:35:34.201Z")), clientClass = Just PhoneClient, clientLabel = Just "", clientCookie = Nothing, clientLocation = Nothing, clientModel = Just ""}

testObject_Client_user_8 :: Client
testObject_Client_user_8 = Client {clientId = ClientId {client = "4"}, clientType = LegalHoldClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-11T06:32:01.921Z")), clientClass = Just PhoneClient, clientLabel = Just "", clientCookie = Just (CookieLabel {cookieLabelText = "\NAKp`"}), clientLocation = Just (location (Latitude (0.8626148594727595)) (Longitude (-1.971023301844283))), clientModel = Just "\1113929"}

testObject_Client_user_9 :: Client
testObject_Client_user_9 = Client {clientId = ClientId {client = "1"}, clientType = LegalHoldClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-08T03:54:56.526Z")), clientClass = Just LegalHoldClient, clientLabel = Just "v\DEL", clientCookie = Just (CookieLabel {cookieLabelText = "G"}), clientLocation = Just (location (Latitude (-0.3086524641730466)) (Longitude (1.72690152811777))), clientModel = Just "\13056m"}

testObject_Client_user_10 :: Client
testObject_Client_user_10 = Client {clientId = ClientId {client = "0"}, clientType = PermanentClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-10T18:42:04.137Z")), clientClass = Nothing, clientLabel = Nothing, clientCookie = Just (CookieLabel {cookieLabelText = "L"}), clientLocation = Just (location (Latitude (-2.6734377548386075)) (Longitude (-1.40544074714727))), clientModel = Just "\CAN"}

testObject_Client_user_11 :: Client
testObject_Client_user_11 = Client {clientId = ClientId {client = "3"}, clientType = TemporaryClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-08T11:57:08.087Z")), clientClass = Just LegalHoldClient, clientLabel = Just "\USb", clientCookie = Just (CookieLabel {cookieLabelText = "5"}), clientLocation = Just (location (Latitude (0.44311730892815937)) (Longitude (0.6936233843789369))), clientModel = Just "ML"}

testObject_Client_user_12 :: Client
testObject_Client_user_12 = Client {clientId = ClientId {client = "2"}, clientType = PermanentClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-08T18:44:00.378Z")), clientClass = Nothing, clientLabel = Just "", clientCookie = Just (CookieLabel {cookieLabelText = "0"}), clientLocation = Just (location (Latitude (-2.502416826395783)) (Longitude (1.4712334862249388))), clientModel = Just ""}

testObject_Client_user_13 :: Client
testObject_Client_user_13 = Client {clientId = ClientId {client = "2"}, clientType = PermanentClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-07T01:09:04.597Z")), clientClass = Just PhoneClient, clientLabel = Just "\1064061", clientCookie = Just (CookieLabel {cookieLabelText = "\f^\1012431"}), clientLocation = Just (location (Latitude (-2.3798205243177692)) (Longitude (-2.619240132398651))), clientModel = Just "\ETB\68772"}

testObject_Client_user_14 :: Client
testObject_Client_user_14 = Client {clientId = ClientId {client = "2"}, clientType = TemporaryClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-12T11:00:10.449Z")), clientClass = Just TabletClient, clientLabel = Just "x\SO", clientCookie = Nothing, clientLocation = Just (location (Latitude (2.459582010332432)) (Longitude (-1.2286910026214775))), clientModel = Just "\1052175\r\917608"}

testObject_Client_user_15 :: Client
testObject_Client_user_15 = Client {clientId = ClientId {client = "3"}, clientType = TemporaryClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-08T11:28:27.778Z")), clientClass = Nothing, clientLabel = Just "\EOTG", clientCookie = Just (CookieLabel {cookieLabelText = "\1100343N"}), clientLocation = Nothing, clientModel = Just "zAI"}

testObject_Client_user_16 :: Client
testObject_Client_user_16 = Client {clientId = ClientId {client = "2"}, clientType = TemporaryClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-12T11:31:10.072Z")), clientClass = Just LegalHoldClient, clientLabel = Just "=E", clientCookie = Just (CookieLabel {cookieLabelText = "U"}), clientLocation = Nothing, clientModel = Just ""}

testObject_Client_user_17 :: Client
testObject_Client_user_17 = Client {clientId = ClientId {client = "4"}, clientType = TemporaryClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-12T02:25:34.770Z")), clientClass = Just DesktopClient, clientLabel = Nothing, clientCookie = Just (CookieLabel {cookieLabelText = ""}), clientLocation = Just (location (Latitude (-1.6915872714820337)) (Longitude (2.1128949838723656))), clientModel = Just ""}

testObject_Client_user_18 :: Client
testObject_Client_user_18 = Client {clientId = ClientId {client = "1"}, clientType = TemporaryClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-07T17:21:05.930Z")), clientClass = Just LegalHoldClient, clientLabel = Just "\996666", clientCookie = Just (CookieLabel {cookieLabelText = "PG:"}), clientLocation = Just (location (Latitude (-1.2949675488134762)) (Longitude (0.43717421775412324))), clientModel = Just "\DEL\1071737"}

testObject_Client_user_19 :: Client
testObject_Client_user_19 = Client {clientId = ClientId {client = "2"}, clientType = PermanentClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-12T07:49:27.999Z")), clientClass = Just DesktopClient, clientLabel = Just "\1098224l", clientCookie = Nothing, clientLocation = Just (location (Latitude (-1.4630309786758076)) (Longitude (-0.5295690632216867))), clientModel = Just ""}

testObject_Client_user_20 :: Client
testObject_Client_user_20 = Client {clientId = ClientId {client = "1"}, clientType = LegalHoldClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-06T18:43:52.483Z")), clientClass = Just PhoneClient, clientLabel = Just "-\1032867v", clientCookie = Just (CookieLabel {cookieLabelText = ""}), clientLocation = Just (location (Latitude (2.8672347564452996)) (Longitude (-0.9990390825956594))), clientModel = Nothing}
