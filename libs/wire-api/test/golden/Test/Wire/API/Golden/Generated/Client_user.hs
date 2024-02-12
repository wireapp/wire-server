{-# LANGUAGE OverloadedLists #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Test.Wire.API.Golden.Generated.Client_user where

import Data.Id
import Data.Json.Util
import Data.Map qualified as Map
import Data.Set as Set
import Imports
import Wire.API.MLS.CipherSuite
import Wire.API.User.Auth (CookieLabel (CookieLabel, cookieLabelText))
import Wire.API.User.Client

testObject_Client_user_1 :: Client
testObject_Client_user_1 =
  Client
    { clientId = ClientId 2,
      clientType = PermanentClientType,
      clientTime = fromJust (readUTCTimeMillis "1864-05-06T19:39:12.770Z"),
      clientClass = Just DesktopClient,
      clientLabel = Just "%*",
      clientCookie = Nothing,
      clientModel = Just "\995802;\1081067",
      clientCapabilities = ClientCapabilityList Set.empty,
      clientMLSPublicKeys = mempty,
      clientLastActive = Nothing
    }

testObject_Client_user_2 :: Client
testObject_Client_user_2 =
  Client
    { clientId = ClientId 1,
      clientType = LegalHoldClientType,
      clientTime = fromJust (readUTCTimeMillis "1864-05-07T08:48:22.537Z"),
      clientClass = Nothing,
      clientLabel = Nothing,
      clientCookie = Just (CookieLabel {cookieLabelText = "\1112890c\1065129"}),
      clientModel = Nothing,
      clientCapabilities = ClientCapabilityList Set.empty,
      clientMLSPublicKeys = mempty,
      clientLastActive = Nothing
    }

testObject_Client_user_3 :: Client
testObject_Client_user_3 =
  Client
    { clientId = ClientId 1,
      clientType = TemporaryClientType,
      clientTime = fromJust (readUTCTimeMillis "1864-05-07T00:38:22.384Z"),
      clientClass = Just LegalHoldClient,
      clientLabel = Just "pi",
      clientCookie = Just (CookieLabel {cookieLabelText = ""}),
      clientModel = Nothing,
      clientCapabilities = ClientCapabilityList Set.empty,
      clientMLSPublicKeys = mempty,
      clientLastActive = fmap fromUTCTimeMillis (readUTCTimeMillis "2023-07-04T09:35:32.000Z")
    }

testObject_Client_user_4 :: Client
testObject_Client_user_4 =
  Client
    { clientId = ClientId 3,
      clientType = PermanentClientType,
      clientTime = fromJust (readUTCTimeMillis "1864-05-06T09:13:45.902Z"),
      clientClass = Just LegalHoldClient,
      clientLabel = Nothing,
      clientCookie = Just (CookieLabel {cookieLabelText = "j"}),
      clientModel = Just "",
      clientCapabilities = ClientCapabilityList Set.empty,
      clientMLSPublicKeys = mempty,
      clientLastActive = Nothing
    }

testObject_Client_user_5 :: Client
testObject_Client_user_5 =
  Client
    { clientId = ClientId 0,
      clientType = TemporaryClientType,
      clientTime = fromJust (readUTCTimeMillis "1864-05-07T09:07:14.559Z"),
      clientClass = Just DesktopClient,
      clientLabel = Nothing,
      clientCookie = Just (CookieLabel {cookieLabelText = ""}),
      clientModel = Just "\9015o",
      clientCapabilities = ClientCapabilityList Set.empty,
      clientMLSPublicKeys = mempty,
      clientLastActive = Nothing
    }

testObject_Client_user_6 :: Client
testObject_Client_user_6 =
  Client
    { clientId = ClientId 4,
      clientType = PermanentClientType,
      clientTime = fromJust (readUTCTimeMillis "1864-05-08T22:37:53.030Z"),
      clientClass = Just TabletClient,
      clientLabel = Nothing,
      clientCookie = Just (CookieLabel {cookieLabelText = "l\STX"}),
      clientModel = Just "",
      clientCapabilities = ClientCapabilityList Set.empty,
      clientMLSPublicKeys = mempty,
      clientLastActive = fmap fromUTCTimeMillis (readUTCTimeMillis "2021-09-15T22:00:21.000Z")
    }

testObject_Client_user_7 :: Client
testObject_Client_user_7 =
  Client
    { clientId = ClientId 4,
      clientType = PermanentClientType,
      clientTime = fromJust (readUTCTimeMillis "1864-05-07T04:35:34.201Z"),
      clientClass = Just PhoneClient,
      clientLabel = Just "",
      clientCookie = Nothing,
      clientModel = Just "",
      clientCapabilities = ClientCapabilityList Set.empty,
      clientMLSPublicKeys = mempty,
      clientLastActive = Nothing
    }

testObject_Client_user_8 :: Client
testObject_Client_user_8 =
  Client
    { clientId = ClientId 4,
      clientType = LegalHoldClientType,
      clientTime = fromJust (readUTCTimeMillis "1864-05-11T06:32:01.921Z"),
      clientClass = Just PhoneClient,
      clientLabel = Just "",
      clientCookie = Just (CookieLabel {cookieLabelText = "\NAKp`"}),
      clientModel = Just "\1113929",
      clientCapabilities = ClientCapabilityList Set.empty,
      clientMLSPublicKeys = mempty,
      clientLastActive = Nothing
    }

testObject_Client_user_9 :: Client
testObject_Client_user_9 =
  Client
    { clientId = ClientId 1,
      clientType = LegalHoldClientType,
      clientTime = fromJust (readUTCTimeMillis "1864-05-08T03:54:56.526Z"),
      clientClass = Just LegalHoldClient,
      clientLabel = Just "v\DEL",
      clientCookie = Just (CookieLabel {cookieLabelText = "G"}),
      clientModel = Just "\13056m",
      clientCapabilities = ClientCapabilityList Set.empty,
      clientMLSPublicKeys = mempty,
      clientLastActive = Nothing
    }

testObject_Client_user_10 :: Client
testObject_Client_user_10 =
  Client
    { clientId = ClientId 0,
      clientType = PermanentClientType,
      clientTime = fromJust (readUTCTimeMillis "1864-05-10T18:42:04.137Z"),
      clientClass = Nothing,
      clientLabel = Nothing,
      clientCookie = Just (CookieLabel {cookieLabelText = "L"}),
      clientModel = Just "\CAN",
      clientCapabilities = ClientCapabilityList Set.empty,
      clientMLSPublicKeys = Map.fromList [(Ed25519, "ZmFrZSBwdWJsaWMga2V5")],
      clientLastActive = Nothing
    }

testObject_Client_user_11 :: Client
testObject_Client_user_11 =
  Client
    { clientId = ClientId 3,
      clientType = TemporaryClientType,
      clientTime = fromJust (readUTCTimeMillis "1864-05-08T11:57:08.087Z"),
      clientClass = Just LegalHoldClient,
      clientLabel = Just "\USb",
      clientCookie = Just (CookieLabel {cookieLabelText = "5"}),
      clientModel = Just "ML",
      clientCapabilities = ClientCapabilityList Set.empty,
      clientMLSPublicKeys = mempty,
      clientLastActive = Nothing
    }

testObject_Client_user_12 :: Client
testObject_Client_user_12 =
  Client
    { clientId = ClientId 2,
      clientType = PermanentClientType,
      clientTime = fromJust (readUTCTimeMillis "1864-05-08T18:44:00.378Z"),
      clientClass = Nothing,
      clientLabel = Just "",
      clientCookie = Just (CookieLabel {cookieLabelText = "0"}),
      clientModel = Just "",
      clientCapabilities = ClientCapabilityList Set.empty,
      clientMLSPublicKeys = mempty,
      clientLastActive = Nothing
    }

testObject_Client_user_13 :: Client
testObject_Client_user_13 =
  Client
    { clientId = ClientId 2,
      clientType = PermanentClientType,
      clientTime = fromJust (readUTCTimeMillis "1864-05-07T01:09:04.597Z"),
      clientClass = Just PhoneClient,
      clientLabel = Just "\1064061",
      clientCookie = Just (CookieLabel {cookieLabelText = "\f^\1012431"}),
      clientModel = Just "\ETB\68772",
      clientCapabilities = ClientCapabilityList Set.empty,
      clientMLSPublicKeys = mempty,
      clientLastActive = Nothing
    }

testObject_Client_user_14 :: Client
testObject_Client_user_14 =
  Client
    { clientId = ClientId 2,
      clientType = TemporaryClientType,
      clientTime = fromJust (readUTCTimeMillis "1864-05-12T11:00:10.449Z"),
      clientClass = Just TabletClient,
      clientLabel = Just "x\SO",
      clientCookie = Nothing,
      clientModel = Just "\1052175\r\917608",
      clientCapabilities = ClientCapabilityList Set.empty,
      clientMLSPublicKeys = mempty,
      clientLastActive = Nothing
    }

testObject_Client_user_15 :: Client
testObject_Client_user_15 =
  Client
    { clientId = ClientId 3,
      clientType = TemporaryClientType,
      clientTime = fromJust (readUTCTimeMillis "1864-05-08T11:28:27.778Z"),
      clientClass = Nothing,
      clientLabel = Just "\EOTG",
      clientCookie = Just (CookieLabel {cookieLabelText = "\1100343N"}),
      clientModel = Just "zAI",
      clientCapabilities = ClientCapabilityList Set.empty,
      clientMLSPublicKeys = mempty,
      clientLastActive = Nothing
    }

testObject_Client_user_16 :: Client
testObject_Client_user_16 =
  Client
    { clientId = ClientId 2,
      clientType = TemporaryClientType,
      clientTime = fromJust (readUTCTimeMillis "1864-05-12T11:31:10.072Z"),
      clientClass = Just LegalHoldClient,
      clientLabel = Just "=E",
      clientCookie = Just (CookieLabel {cookieLabelText = "U"}),
      clientModel = Just "",
      clientCapabilities = ClientCapabilityList Set.empty,
      clientMLSPublicKeys = mempty,
      clientLastActive = Nothing
    }

testObject_Client_user_17 :: Client
testObject_Client_user_17 =
  Client
    { clientId = ClientId 4,
      clientType = TemporaryClientType,
      clientTime = fromJust (readUTCTimeMillis "1864-05-12T02:25:34.770Z"),
      clientClass = Just DesktopClient,
      clientLabel = Nothing,
      clientCookie = Just (CookieLabel {cookieLabelText = ""}),
      clientModel = Just "",
      clientCapabilities = ClientCapabilityList Set.empty,
      clientMLSPublicKeys = mempty,
      clientLastActive = Nothing
    }

testObject_Client_user_18 :: Client
testObject_Client_user_18 =
  Client
    { clientId = ClientId 1,
      clientType = TemporaryClientType,
      clientTime = fromJust (readUTCTimeMillis "1864-05-07T17:21:05.930Z"),
      clientClass = Just LegalHoldClient,
      clientLabel = Just "\996666",
      clientCookie = Just (CookieLabel {cookieLabelText = "PG:"}),
      clientModel = Just "\DEL\1071737",
      clientCapabilities = ClientCapabilityList Set.empty,
      clientMLSPublicKeys = mempty,
      clientLastActive = Nothing
    }

testObject_Client_user_19 :: Client
testObject_Client_user_19 =
  Client
    { clientId = ClientId 2,
      clientType = PermanentClientType,
      clientTime = fromJust (readUTCTimeMillis "1864-05-12T07:49:27.999Z"),
      clientClass = Just DesktopClient,
      clientLabel = Just "\1098224l",
      clientCookie = Nothing,
      clientModel = Just "",
      clientCapabilities = ClientCapabilityList Set.empty,
      clientMLSPublicKeys = mempty,
      clientLastActive = Nothing
    }

testObject_Client_user_20 :: Client
testObject_Client_user_20 =
  Client
    { clientId = ClientId 1,
      clientType = LegalHoldClientType,
      clientTime = fromJust (readUTCTimeMillis "1864-05-06T18:43:52.483Z"),
      clientClass = Just PhoneClient,
      clientLabel = Just "-\1032867v",
      clientCookie = Just (CookieLabel {cookieLabelText = ""}),
      clientModel = Nothing,
      clientCapabilities = ClientCapabilityList (Set.fromList [ClientSupportsLegalholdImplicitConsent]),
      clientMLSPublicKeys = mempty,
      clientLastActive = Nothing
    }
