{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.UserLegalHoldStatusResponse_team where
import Data.Id ( ClientId(ClientId, client) )
import Imports ( Maybe(Nothing, Just) )
import Data.LegalHold
    ( UserLegalHoldStatus(UserLegalHoldDisabled, UserLegalHoldPending,
                          UserLegalHoldEnabled) )
import Wire.API.Team.LegalHold ( UserLegalHoldStatusResponse(..) )
import Wire.API.User.Client.Prekey ( lastPrekey )

testObject_UserLegalHoldStatusResponse_team_1 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_1 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldEnabled, ulhsrLastPrekey = Just (lastPrekey ("\1072196>_`Cvx")), ulhsrClientId = Just (ClientId {client = "de"})}
testObject_UserLegalHoldStatusResponse_team_2 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_2 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldPending, ulhsrLastPrekey = Just (lastPrekey ("")), ulhsrClientId = Nothing}
testObject_UserLegalHoldStatusResponse_team_3 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_3 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldDisabled, ulhsrLastPrekey = Just (lastPrekey ("1$\1083760\&3\100579")), ulhsrClientId = Just (ClientId {client = "41"})}
testObject_UserLegalHoldStatusResponse_team_4 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_4 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldPending, ulhsrLastPrekey = Just (lastPrekey ("IU\r;t\47756S\bs|")), ulhsrClientId = Just (ClientId {client = "e"})}
testObject_UserLegalHoldStatusResponse_team_5 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_5 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldDisabled, ulhsrLastPrekey = Just (lastPrekey ("\138055p\74363\ESC;\ETBH")), ulhsrClientId = Just (ClientId {client = "3d"})}
testObject_UserLegalHoldStatusResponse_team_6 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_6 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldPending, ulhsrLastPrekey = Just (lastPrekey ("\DC1t_\78806")), ulhsrClientId = Just (ClientId {client = "7"})}
testObject_UserLegalHoldStatusResponse_team_7 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_7 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldEnabled, ulhsrLastPrekey = Just (lastPrekey ("\SYN\135963")), ulhsrClientId = Just (ClientId {client = "bb"})}
testObject_UserLegalHoldStatusResponse_team_8 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_8 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldEnabled, ulhsrLastPrekey = Just (lastPrekey ("U5\CAN\182862 ^")), ulhsrClientId = Just (ClientId {client = "c7"})}
testObject_UserLegalHoldStatusResponse_team_9 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_9 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldPending, ulhsrLastPrekey = Just (lastPrekey ("\RSC")), ulhsrClientId = Just (ClientId {client = "8"})}
testObject_UserLegalHoldStatusResponse_team_10 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_10 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldDisabled, ulhsrLastPrekey = Nothing, ulhsrClientId = Just (ClientId {client = "5f"})}
testObject_UserLegalHoldStatusResponse_team_11 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_11 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldDisabled, ulhsrLastPrekey = Just (lastPrekey ("")), ulhsrClientId = Just (ClientId {client = "9f"})}
testObject_UserLegalHoldStatusResponse_team_12 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_12 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldEnabled, ulhsrLastPrekey = Just (lastPrekey ("2p\n\ENQ\RS\NUL")), ulhsrClientId = Just (ClientId {client = "71"})}
testObject_UserLegalHoldStatusResponse_team_13 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_13 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldDisabled, ulhsrLastPrekey = Just (lastPrekey ("\168076\"")), ulhsrClientId = Just (ClientId {client = "f3"})}
testObject_UserLegalHoldStatusResponse_team_14 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_14 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldPending, ulhsrLastPrekey = Just (lastPrekey ("R\1008785\"\SIQ")), ulhsrClientId = Just (ClientId {client = "72"})}
testObject_UserLegalHoldStatusResponse_team_15 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_15 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldDisabled, ulhsrLastPrekey = Nothing, ulhsrClientId = Just (ClientId {client = "50"})}
testObject_UserLegalHoldStatusResponse_team_16 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_16 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldDisabled, ulhsrLastPrekey = Nothing, ulhsrClientId = Just (ClientId {client = "c2"})}
testObject_UserLegalHoldStatusResponse_team_17 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_17 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldPending, ulhsrLastPrekey = Just (lastPrekey ("\DC3F}H\b\DLEWSMt")), ulhsrClientId = Nothing}
testObject_UserLegalHoldStatusResponse_team_18 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_18 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldEnabled, ulhsrLastPrekey = Just (lastPrekey ("\98780cjP")), ulhsrClientId = Nothing}
testObject_UserLegalHoldStatusResponse_team_19 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_19 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldEnabled, ulhsrLastPrekey = Just (lastPrekey ("\997911\5419")), ulhsrClientId = Just (ClientId {client = "c4"})}
testObject_UserLegalHoldStatusResponse_team_20 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_20 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldDisabled, ulhsrLastPrekey = Just (lastPrekey ("\156027n h~")), ulhsrClientId = Nothing}
