{-# LANGUAGE OverloadedLists #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Wire.API.Golden.Generated.PushToken_user where

import Data.Id (ClientId (ClientId, client))
import Wire.API.Push.Token
  ( AppName (AppName, appNameText),
    PushToken,
    Token (Token, tokenText),
    Transport (APNS, APNSSandbox, APNSVoIP, APNSVoIPSandbox, GCM),
    pushToken,
  )

testObject_PushToken_user_1 :: PushToken
testObject_PushToken_user_1 = (pushToken (APNSSandbox) (AppName {appNameText = "G\1008289"}) (Token {tokenText = "\EOT8M\NAKAv\1104873"}) (ClientId {client = "16"}))

testObject_PushToken_user_2 :: PushToken
testObject_PushToken_user_2 = (pushToken (APNSVoIP) (AppName {appNameText = "G\r\42700\r"}) (Token {tokenText = "\188813\ETX\118978"}) (ClientId {client = "1e"}))

testObject_PushToken_user_3 :: PushToken
testObject_PushToken_user_3 = (pushToken (APNSSandbox) (AppName {appNameText = "9\f"}) (Token {tokenText = "\ETX"}) (ClientId {client = "13"}))

testObject_PushToken_user_4 :: PushToken
testObject_PushToken_user_4 = (pushToken (APNSSandbox) (AppName {appNameText = "C%\SOH\v\1091701H\1022158"}) (Token {tokenText = "\SOH$\SYNW"}) (ClientId {client = "b"}))

testObject_PushToken_user_5 :: PushToken
testObject_PushToken_user_5 = (pushToken (GCM) (AppName {appNameText = "<F"}) (Token {tokenText = "\468\&9\1037687mz\27063\FS"}) (ClientId {client = "19"}))

testObject_PushToken_user_6 :: PushToken
testObject_PushToken_user_6 = (pushToken (APNSVoIP) (AppName {appNameText = "\1043230"}) (Token {tokenText = "_S"}) (ClientId {client = "15"}))

testObject_PushToken_user_7 :: PushToken
testObject_PushToken_user_7 = (pushToken (GCM) (AppName {appNameText = "\997790"}) (Token {tokenText = "Z\STXJ"}) (ClientId {client = "14"}))

testObject_PushToken_user_8 :: PushToken
testObject_PushToken_user_8 = (pushToken (APNSVoIP) (AppName {appNameText = "*/"}) (Token {tokenText = "c\DEL\993798\1029314"}) (ClientId {client = "1f"}))

testObject_PushToken_user_9 :: PushToken
testObject_PushToken_user_9 = (pushToken (APNSVoIPSandbox) (AppName {appNameText = ""}) (Token {tokenText = "\990144Fm\1086553\1050803"}) (ClientId {client = "8"}))

testObject_PushToken_user_10 :: PushToken
testObject_PushToken_user_10 = (pushToken (APNSVoIPSandbox) (AppName {appNameText = "J"}) (Token {tokenText = "h\998173ZFt0"}) (ClientId {client = "17"}))

testObject_PushToken_user_11 :: PushToken
testObject_PushToken_user_11 = (pushToken (APNSVoIP) (AppName {appNameText = "\1037307\43873\42199"}) (Token {tokenText = "I"}) (ClientId {client = "11"}))

testObject_PushToken_user_12 :: PushToken
testObject_PushToken_user_12 = (pushToken (APNSVoIP) (AppName {appNameText = "4\157970E$\179876Z\152804"}) (Token {tokenText = "5Q\29832\9020#\f"}) (ClientId {client = "18"}))

testObject_PushToken_user_13 :: PushToken
testObject_PushToken_user_13 = (pushToken (APNSVoIP) (AppName {appNameText = "\111173\1022802;\147450;`"}) (Token {tokenText = "os\1078856\&9\DC4{"}) (ClientId {client = "1b"}))

testObject_PushToken_user_14 :: PushToken
testObject_PushToken_user_14 = (pushToken (APNSVoIPSandbox) (AppName {appNameText = "\23524\17553\145614?L"}) (Token {tokenText = "@"}) (ClientId {client = "1a"}))

testObject_PushToken_user_15 :: PushToken
testObject_PushToken_user_15 = (pushToken (APNSSandbox) (AppName {appNameText = ""}) (Token {tokenText = "\SI\12188h\120421\54584\NUL\NAK"}) (ClientId {client = "6"}))

testObject_PushToken_user_16 :: PushToken
testObject_PushToken_user_16 = (pushToken (APNSVoIP) (AppName {appNameText = "s\25171z\182700"}) (Token {tokenText = "\CAN"}) (ClientId {client = "1d"}))

testObject_PushToken_user_17 :: PushToken
testObject_PushToken_user_17 = (pushToken (APNSSandbox) (AppName {appNameText = "$\1042956\SOH"}) (Token {tokenText = "\"-*4\1000675q\1045235"}) (ClientId {client = "15"}))

testObject_PushToken_user_18 :: PushToken
testObject_PushToken_user_18 = (pushToken (GCM) (AppName {appNameText = "\1057135;\134834v4\b6"}) (Token {tokenText = "#\983343V\DELH\1053936\94560"}) (ClientId {client = "1e"}))

testObject_PushToken_user_19 :: PushToken
testObject_PushToken_user_19 = (pushToken (APNSVoIP) (AppName {appNameText = "\992696\1085063\1030175\128353"}) (Token {tokenText = "n%\NAK\8959l\ETX"}) (ClientId {client = "1d"}))

testObject_PushToken_user_20 :: PushToken
testObject_PushToken_user_20 = (pushToken (APNS) (AppName {appNameText = "j\462BPh&"}) (Token {tokenText = "\93035,/.pL"}) (ClientId {client = "9"}))
