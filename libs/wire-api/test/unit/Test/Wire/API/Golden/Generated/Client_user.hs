{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Client_user where
import Data.Id ( ClientId(ClientId, client) )
import Data.Json.Util ( readUTCTimeMillis )
import Data.Misc
    ( location, Latitude(Latitude), Longitude(Longitude) )
import Imports ( Maybe(Just, Nothing), fromJust )
import Wire.API.User.Auth
    ( CookieLabel(CookieLabel, cookieLabelText) )
import Wire.API.User.Client
    ( Client(..),
      ClientClass(LegalHoldClient, PhoneClient, DesktopClient,
                  TabletClient),
      ClientType(TemporaryClientType, LegalHoldClientType,
                 PermanentClientType) )

testObject_Client_user_1 :: Client
testObject_Client_user_1 = Client {clientId = ClientId {client = "1"}, clientType = LegalHoldClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-08T12:19:13.786Z")), clientClass = Just LegalHoldClient, clientLabel = Just "\70801", clientCookie = Just (CookieLabel {cookieLabelText = "\NUL\64492"}), clientLocation = Just (location (Latitude (-1.548140307108261e-3)) (Longitude (-0.44464300831258446))), clientModel = Just "\CAN\DC3"}
testObject_Client_user_2 :: Client
testObject_Client_user_2 = Client {clientId = ClientId {client = "3"}, clientType = TemporaryClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-07T05:14:31.391Z")), clientClass = Nothing, clientLabel = Just "\78320", clientCookie = Nothing, clientLocation = Just (location (Latitude (1.1027152821953188)) (Longitude (2.1629464382684938))), clientModel = Just "\r%"}
testObject_Client_user_3 :: Client
testObject_Client_user_3 = Client {clientId = ClientId {client = "0"}, clientType = LegalHoldClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-07T16:25:36.467Z")), clientClass = Nothing, clientLabel = Just "", clientCookie = Nothing, clientLocation = Nothing, clientModel = Nothing}
testObject_Client_user_4 :: Client
testObject_Client_user_4 = Client {clientId = ClientId {client = "0"}, clientType = PermanentClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-10T10:08:57.760Z")), clientClass = Just LegalHoldClient, clientLabel = Just "{3", clientCookie = Just (CookieLabel {cookieLabelText = "t\DC4"}), clientLocation = Just (location (Latitude (-2.8345777733950888)) (Longitude (-1.8486328188813823))), clientModel = Just ";Z\1069512"}
testObject_Client_user_5 :: Client
testObject_Client_user_5 = Client {clientId = ClientId {client = "4"}, clientType = LegalHoldClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-09T12:40:17.121Z")), clientClass = Just DesktopClient, clientLabel = Just "\35225\US", clientCookie = Nothing, clientLocation = Nothing, clientModel = Just "loV"}
testObject_Client_user_6 :: Client
testObject_Client_user_6 = Client {clientId = ClientId {client = "4"}, clientType = LegalHoldClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-06T04:42:12.037Z")), clientClass = Just LegalHoldClient, clientLabel = Nothing, clientCookie = Nothing, clientLocation = Just (location (Latitude (0.5238854038549572)) (Longitude (0.8008418972856424))), clientModel = Just "\r\1084404e"}
testObject_Client_user_7 :: Client
testObject_Client_user_7 = Client {clientId = ClientId {client = "4"}, clientType = TemporaryClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-06T22:42:26.726Z")), clientClass = Just LegalHoldClient, clientLabel = Just "\30611", clientCookie = Just (CookieLabel {cookieLabelText = "\999046\98307"}), clientLocation = Nothing, clientModel = Nothing}
testObject_Client_user_8 :: Client
testObject_Client_user_8 = Client {clientId = ClientId {client = "0"}, clientType = TemporaryClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-10T18:23:52.621Z")), clientClass = Nothing, clientLabel = Just "\1081932\159071A", clientCookie = Just (CookieLabel {cookieLabelText = "'"}), clientLocation = Just (location (Latitude (-2.794431875897655)) (Longitude (-1.9350950436046068))), clientModel = Just ""}
testObject_Client_user_9 :: Client
testObject_Client_user_9 = Client {clientId = ClientId {client = "2"}, clientType = PermanentClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-09T21:16:06.931Z")), clientClass = Nothing, clientLabel = Just "e\1070529\996005", clientCookie = Just (CookieLabel {cookieLabelText = "#\149097-"}), clientLocation = Just (location (Latitude (-1.8060908876788007)) (Longitude (-2.049531340415897))), clientModel = Nothing}
testObject_Client_user_10 :: Client
testObject_Client_user_10 = Client {clientId = ClientId {client = "1"}, clientType = TemporaryClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-11T15:44:02.869Z")), clientClass = Nothing, clientLabel = Just "\ETX\GS=", clientCookie = Just (CookieLabel {cookieLabelText = ""}), clientLocation = Just (location (Latitude (-2.424281283091626)) (Longitude (1.7211747869969847))), clientModel = Just "$."}
testObject_Client_user_11 :: Client
testObject_Client_user_11 = Client {clientId = ClientId {client = "4"}, clientType = LegalHoldClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-09T05:22:25.272Z")), clientClass = Just PhoneClient, clientLabel = Just "", clientCookie = Just (CookieLabel {cookieLabelText = "\32761"}), clientLocation = Just (location (Latitude (1.3504661014534058)) (Longitude (-1.1733337498756742))), clientModel = Just "K\1014038"}
testObject_Client_user_12 :: Client
testObject_Client_user_12 = Client {clientId = ClientId {client = "4"}, clientType = LegalHoldClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-11T14:32:58.470Z")), clientClass = Just DesktopClient, clientLabel = Nothing, clientCookie = Nothing, clientLocation = Just (location (Latitude (-0.22064832246177687)) (Longitude (0.6220936382044138))), clientModel = Just "\144038"}
testObject_Client_user_13 :: Client
testObject_Client_user_13 = Client {clientId = ClientId {client = "0"}, clientType = PermanentClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-11T03:02:40.407Z")), clientClass = Just PhoneClient, clientLabel = Just "\986848", clientCookie = Nothing, clientLocation = Just (location (Latitude (0.691658486655809)) (Longitude (-2.3268885154113654))), clientModel = Nothing}
testObject_Client_user_14 :: Client
testObject_Client_user_14 = Client {clientId = ClientId {client = "2"}, clientType = TemporaryClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-06T01:02:15.021Z")), clientClass = Just LegalHoldClient, clientLabel = Just "i26", clientCookie = Just (CookieLabel {cookieLabelText = ""}), clientLocation = Just (location (Latitude (-1.9018924513609665)) (Longitude (-2.486230001143338))), clientModel = Just ""}
testObject_Client_user_15 :: Client
testObject_Client_user_15 = Client {clientId = ClientId {client = "0"}, clientType = TemporaryClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-11T10:21:55.239Z")), clientClass = Just DesktopClient, clientLabel = Just "kp\a", clientCookie = Just (CookieLabel {cookieLabelText = "jL"}), clientLocation = Nothing, clientModel = Just "\146045"}
testObject_Client_user_16 :: Client
testObject_Client_user_16 = Client {clientId = ClientId {client = "1"}, clientType = LegalHoldClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-12T11:13:53.566Z")), clientClass = Just DesktopClient, clientLabel = Just "#Do", clientCookie = Nothing, clientLocation = Just (location (Latitude (0.7065753438925302)) (Longitude (-0.608827797358524))), clientModel = Nothing}
testObject_Client_user_17 :: Client
testObject_Client_user_17 = Client {clientId = ClientId {client = "0"}, clientType = PermanentClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-06T07:35:14.974Z")), clientClass = Just LegalHoldClient, clientLabel = Nothing, clientCookie = Just (CookieLabel {cookieLabelText = "\r"}), clientLocation = Just (location (Latitude (0.6760118580963407)) (Longitude (1.8986936793326665))), clientModel = Just "dn\SOH"}
testObject_Client_user_18 :: Client
testObject_Client_user_18 = Client {clientId = ClientId {client = "1"}, clientType = TemporaryClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-06T03:17:17.183Z")), clientClass = Just TabletClient, clientLabel = Just "\EOTXp", clientCookie = Nothing, clientLocation = Nothing, clientModel = Just "/"}
testObject_Client_user_19 :: Client
testObject_Client_user_19 = Client {clientId = ClientId {client = "1"}, clientType = PermanentClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-11T00:02:38.519Z")), clientClass = Just LegalHoldClient, clientLabel = Just "v\1046847g", clientCookie = Just (CookieLabel {cookieLabelText = "\ENQ"}), clientLocation = Just (location (Latitude (-1.7791795906355625)) (Longitude (0.8176104857899789))), clientModel = Just "\32940{"}
testObject_Client_user_20 :: Client
testObject_Client_user_20 = Client {clientId = ClientId {client = "1"}, clientType = TemporaryClientType, clientTime = (fromJust (readUTCTimeMillis "1864-05-09T16:51:48.277Z")), clientClass = Just LegalHoldClient, clientLabel = Just "L", clientCookie = Just (CookieLabel {cookieLabelText = "\ETX\DC2Y"}), clientLocation = Just (location (Latitude (-2.943213084742563)) (Longitude (-2.9665715282947565))), clientModel = Just "\50327"}
