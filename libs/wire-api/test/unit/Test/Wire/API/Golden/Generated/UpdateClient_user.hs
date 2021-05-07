{-# LANGUAGE OverloadedLists #-}

module Test.Wire.API.Golden.Generated.UpdateClient_user where

import Imports (Maybe (Just, Nothing))
import Wire.API.User.Client (UpdateClient (..))
import Wire.API.User.Client.Prekey
  ( Prekey (Prekey, prekeyId, prekeyKey),
    PrekeyId (PrekeyId, keyId),
    lastPrekey,
  )

testObject_UpdateClient_user_1 :: UpdateClient
testObject_UpdateClient_user_1 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = ","}, Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "G\1039380"}], updateClientLastKey = Just (lastPrekey ("")), updateClientLabel = Nothing}

testObject_UpdateClient_user_2 :: UpdateClient
testObject_UpdateClient_user_2 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = "~"}, Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = "\STX"}], updateClientLastKey = Nothing, updateClientLabel = Just "\14793\13068\SOH\74214\US"}

testObject_UpdateClient_user_3 :: UpdateClient
testObject_UpdateClient_user_3 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}, Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "vi"}], updateClientLastKey = Just (lastPrekey ("L\100005")), updateClientLabel = Just "\NUL\12245B\ACK"}

testObject_UpdateClient_user_4 :: UpdateClient
testObject_UpdateClient_user_4 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\997860"}, Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}, Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}, Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "u"}], updateClientLastKey = Just (lastPrekey ("")), updateClientLabel = Just "M\1066358^YH:l"}

testObject_UpdateClient_user_5 :: UpdateClient
testObject_UpdateClient_user_5 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}, Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\1022268"}, Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], updateClientLastKey = Just (lastPrekey ("Cs \74536=")), updateClientLabel = Just "I\1038139\tCzGW\1034813"}

testObject_UpdateClient_user_6 :: UpdateClient
testObject_UpdateClient_user_6 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}, Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}, Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "+"}], updateClientLastKey = Just (lastPrekey ("")), updateClientLabel = Nothing}

testObject_UpdateClient_user_7 :: UpdateClient
testObject_UpdateClient_user_7 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}, Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}, Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}, Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}, Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}, Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}, Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}, Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}], updateClientLastKey = Nothing, updateClientLabel = Just "D9"}

testObject_UpdateClient_user_8 :: UpdateClient
testObject_UpdateClient_user_8 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 4}, prekeyKey = "_Xx;"}], updateClientLastKey = Nothing, updateClientLabel = Just "8\NAKD\57788\111128"}

testObject_UpdateClient_user_9 :: UpdateClient
testObject_UpdateClient_user_9 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}, Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}, Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}, Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}, Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}, Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], updateClientLastKey = Nothing, updateClientLabel = Just "a\24415\\<c\1047390.\1050080"}

testObject_UpdateClient_user_10 :: UpdateClient
testObject_UpdateClient_user_10 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}, Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}, Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}, Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}], updateClientLastKey = Just (lastPrekey ("")), updateClientLabel = Nothing}

testObject_UpdateClient_user_11 :: UpdateClient
testObject_UpdateClient_user_11 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "("}, Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ":"}, Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}, Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "d"}, Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\DEL"}], updateClientLastKey = Nothing, updateClientLabel = Just "`9q)\24055"}

testObject_UpdateClient_user_12 :: UpdateClient
testObject_UpdateClient_user_12 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 3}, prekeyKey = "W\24095S"}], updateClientLastKey = Nothing, updateClientLabel = Just "o\SUB"}

testObject_UpdateClient_user_13 :: UpdateClient
testObject_UpdateClient_user_13 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "z\28560]"}], updateClientLastKey = Just (lastPrekey ("/\CAN\1031188\DLEk")), updateClientLabel = Just ""}

testObject_UpdateClient_user_14 :: UpdateClient
testObject_UpdateClient_user_14 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "1h"}], updateClientLastKey = Just (lastPrekey ("'S\asm")), updateClientLabel = Nothing}

testObject_UpdateClient_user_15 :: UpdateClient
testObject_UpdateClient_user_15 = UpdateClient {updateClientPrekeys = [], updateClientLastKey = Nothing, updateClientLabel = Nothing}

testObject_UpdateClient_user_16 :: UpdateClient
testObject_UpdateClient_user_16 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = "Q}"}], updateClientLastKey = Just (lastPrekey ("Ll\SO\996866k\176052\EOTf\DC2")), updateClientLabel = Just "\993565\&6"}

testObject_UpdateClient_user_17 :: UpdateClient
testObject_UpdateClient_user_17 = UpdateClient {updateClientPrekeys = [], updateClientLastKey = Nothing, updateClientLabel = Just "c\1003571S\NAK\DLE}0 6\RS"}

testObject_UpdateClient_user_18 :: UpdateClient
testObject_UpdateClient_user_18 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ">\1068703"}, Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = "E"}], updateClientLastKey = Just (lastPrekey ("\1010230\1006024\&4/")), updateClientLabel = Just "\SOH\n\16789?a\1034213\DC4O"}

testObject_UpdateClient_user_19 :: UpdateClient
testObject_UpdateClient_user_19 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "b"}, Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}, Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}, Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}, Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], updateClientLastKey = Just (lastPrekey ("\191090t\96624\113765i")), updateClientLabel = Just "\f&QO\"u\t-\\"}

testObject_UpdateClient_user_20 :: UpdateClient
testObject_UpdateClient_user_20 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}, Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}, Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}, Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}, Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}, Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}, Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], updateClientLastKey = Just (lastPrekey ("\DC4 }Kg\ve3")), updateClientLabel = Just "\ESC\EOT\SOHccn\US{Y5"}
