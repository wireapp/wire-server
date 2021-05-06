{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.PushToken_user where
import Data.Id ( ClientId(ClientId, client) )
import Wire.API.Push.Token
    ( pushToken,
      AppName(AppName, appNameText),
      PushToken,
      Token(Token, tokenText),
      Transport(GCM, APNSSandbox, APNSVoIPSandbox, APNSVoIP, APNS) )

testObject_PushToken_user_1 :: PushToken
testObject_PushToken_user_1 = (pushToken (APNSVoIPSandbox) (AppName {appNameText = "E$\DELV\1107383\r"}) (Token {tokenText = "$t\1035453O\181284]"}) (ClientId {client = "19"}))
testObject_PushToken_user_2 :: PushToken
testObject_PushToken_user_2 = (pushToken (APNS) (AppName {appNameText = ""}) (Token {tokenText = ">%\NUL\166412"}) (ClientId {client = "10"}))
testObject_PushToken_user_3 :: PushToken
testObject_PushToken_user_3 = (pushToken (APNS) (AppName {appNameText = "XX\1106186\ETB"}) (Token {tokenText = "["}) (ClientId {client = "11"}))
testObject_PushToken_user_4 :: PushToken
testObject_PushToken_user_4 = (pushToken (APNSVoIP) (AppName {appNameText = "\"\24911\DLEK"}) (Token {tokenText = "\CAN\992745\996250FY"}) (ClientId {client = "20"}))
testObject_PushToken_user_5 :: PushToken
testObject_PushToken_user_5 = (pushToken (APNS) (AppName {appNameText = "\150558"}) (Token {tokenText = "<\ETBR"}) (ClientId {client = "8"}))
testObject_PushToken_user_6 :: PushToken
testObject_PushToken_user_6 = (pushToken (GCM) (AppName {appNameText = "\18856;"}) (Token {tokenText = "\1074702}\vvJ"}) (ClientId {client = "16"}))
testObject_PushToken_user_7 :: PushToken
testObject_PushToken_user_7 = (pushToken (APNSVoIP) (AppName {appNameText = "6Lu"}) (Token {tokenText = "~\ESC=\14794"}) (ClientId {client = "10"}))
testObject_PushToken_user_8 :: PushToken
testObject_PushToken_user_8 = (pushToken (APNSSandbox) (AppName {appNameText = "[\\\1026856\DC3?"}) (Token {tokenText = "@<\ru9"}) (ClientId {client = "e"}))
testObject_PushToken_user_9 :: PushToken
testObject_PushToken_user_9 = (pushToken (APNSVoIP) (AppName {appNameText = "U"}) (Token {tokenText = ""}) (ClientId {client = "1f"}))
testObject_PushToken_user_10 :: PushToken
testObject_PushToken_user_10 = (pushToken (APNSVoIP) (AppName {appNameText = "=\t\t"}) (Token {tokenText = ">\rv!%"}) (ClientId {client = "1f"}))
testObject_PushToken_user_11 :: PushToken
testObject_PushToken_user_11 = (pushToken (APNSVoIP) (AppName {appNameText = "\SOH"}) (Token {tokenText = ""}) (ClientId {client = "c"}))
testObject_PushToken_user_12 :: PushToken
testObject_PushToken_user_12 = (pushToken (APNSVoIPSandbox) (AppName {appNameText = "Gd'\FS\127883\SYN"}) (Token {tokenText = ""}) (ClientId {client = "6"}))
testObject_PushToken_user_13 :: PushToken
testObject_PushToken_user_13 = (pushToken (APNSVoIP) (AppName {appNameText = "\DLE,@X\30606\SUB\DC3"}) (Token {tokenText = "_f\f%7 Y"}) (ClientId {client = "1b"}))
testObject_PushToken_user_14 :: PushToken
testObject_PushToken_user_14 = (pushToken (APNSVoIPSandbox) (AppName {appNameText = "\139092S\163064p\135518H"}) (Token {tokenText = "\1054324"}) (ClientId {client = "1e"}))
testObject_PushToken_user_15 :: PushToken
testObject_PushToken_user_15 = (pushToken (GCM) (AppName {appNameText = "\1001726i\SOH \DC3\US"}) (Token {tokenText = "p\SUB\1048391eiy\NAK"}) (ClientId {client = "d"}))
testObject_PushToken_user_16 :: PushToken
testObject_PushToken_user_16 = (pushToken (APNSVoIP) (AppName {appNameText = "\1017285w"}) (Token {tokenText = "\EOT\SI\139020\EOTC"}) (ClientId {client = "e"}))
testObject_PushToken_user_17 :: PushToken
testObject_PushToken_user_17 = (pushToken (GCM) (AppName {appNameText = "huG"}) (Token {tokenText = "\118868tMd"}) (ClientId {client = "0"}))
testObject_PushToken_user_18 :: PushToken
testObject_PushToken_user_18 = (pushToken (APNSVoIP) (AppName {appNameText = "0"}) (Token {tokenText = ""}) (ClientId {client = "4"}))
testObject_PushToken_user_19 :: PushToken
testObject_PushToken_user_19 = (pushToken (APNS) (AppName {appNameText = "8@~0\f"}) (Token {tokenText = "\SI<\EM\191056\118933"}) (ClientId {client = "e"}))
testObject_PushToken_user_20 :: PushToken
testObject_PushToken_user_20 = (pushToken (GCM) (AppName {appNameText = "\1024"}) (Token {tokenText = ""}) (ClientId {client = "1"}))
