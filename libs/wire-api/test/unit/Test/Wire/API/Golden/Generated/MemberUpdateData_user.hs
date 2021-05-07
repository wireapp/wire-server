{-# LANGUAGE OverloadedLists #-}

module Test.Wire.API.Golden.Generated.MemberUpdateData_user where

import Data.Id (Id (Id))
import qualified Data.UUID as UUID (fromString)
import Imports
  ( Bool (False, True),
    Maybe (Just, Nothing),
    fromJust,
  )
import Wire.API.Conversation
  ( MutedStatus (MutedStatus, fromMutedStatus),
  )
import Wire.API.Conversation.Role (parseRoleName)
import Wire.API.Event.Conversation (MemberUpdateData (..))

testObject_MemberUpdateData_user_1 :: MemberUpdateData
testObject_MemberUpdateData_user_1 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001"))), misOtrMuted = Just False, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), misOtrMutedRef = Just "#M\95696", misOtrArchived = Just False, misOtrArchivedRef = Just "a", misHidden = Just True, misHiddenRef = Just "1", misConvRoleName = Nothing}

testObject_MemberUpdateData_user_2 :: MemberUpdateData
testObject_MemberUpdateData_user_2 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000001"))), misOtrMuted = Just False, misOtrMutedStatus = Nothing, misOtrMutedRef = Just "L\r\1008876", misOtrArchived = Just True, misOtrArchivedRef = Just "\v\DC4", misHidden = Nothing, misHiddenRef = Just "x\ESC\1112603", misConvRoleName = Just (fromJust (parseRoleName "3fwjaofhryb7nd1hp3nwukjiyxxhgimw8ddzx5s_8ek5nnctkzkic6w51hqugeh6l50hg87dez8pw974dbuywd83njuytv0euf9619s"))}

testObject_MemberUpdateData_user_3 :: MemberUpdateData
testObject_MemberUpdateData_user_3 = MemberUpdateData {misTarget = Nothing, misOtrMuted = Just True, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), misOtrMutedRef = Just "", misOtrArchived = Nothing, misOtrArchivedRef = Nothing, misHidden = Nothing, misHiddenRef = Just "\185389", misConvRoleName = Just (fromJust (parseRoleName "o2pxqmm_oqv3otnujuy0kpz6g1ag2uifcvoifldu9o3712w2tjzgkq0wujb5kgs9ckw3wl1k6bw7g5ar8w5xcbr917engs11a7448nl7zn1aq00b3dd0vx"))}

testObject_MemberUpdateData_user_4 :: MemberUpdateData
testObject_MemberUpdateData_user_4 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000000"))), misOtrMuted = Just True, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -2}), misOtrMutedRef = Just "f\990231\DC2", misOtrArchived = Just True, misOtrArchivedRef = Just "", misHidden = Nothing, misHiddenRef = Nothing, misConvRoleName = Just (fromJust (parseRoleName "t2hf3xe1ifhpvvoslunyqz2nx_lsufmw5zbh42i5j0ivfvo"))}

testObject_MemberUpdateData_user_5 :: MemberUpdateData
testObject_MemberUpdateData_user_5 = MemberUpdateData {misTarget = Nothing, misOtrMuted = Just False, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), misOtrMutedRef = Just "", misOtrArchived = Just True, misOtrArchivedRef = Nothing, misHidden = Just False, misHiddenRef = Just "!", misConvRoleName = Just (fromJust (parseRoleName "_p1ctmxajl8_xxe9_5vzwv4g7gcw_efe75x55n5qv5mokpe10ddg9cfkdjv0tx1fwg9w3ppw9jtpyj8_da5ptc"))}

testObject_MemberUpdateData_user_6 :: MemberUpdateData
testObject_MemberUpdateData_user_6 = MemberUpdateData {misTarget = Nothing, misOtrMuted = Nothing, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -2}), misOtrMutedRef = Nothing, misOtrArchived = Just False, misOtrArchivedRef = Just "Q\DC3\29131", misHidden = Just True, misHiddenRef = Nothing, misConvRoleName = Just (fromJust (parseRoleName "ydqensdr39dvnxk4fcelt2zijpvt_txux15xfo2z2tka925gia97prtrss2zv3bnwpsjsb13tj3x0wg9mbbj2oo4kboh_033za30b4fme5x2m_l85n_"))}

testObject_MemberUpdateData_user_7 :: MemberUpdateData
testObject_MemberUpdateData_user_7 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000000"))), misOtrMuted = Just False, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 2}), misOtrMutedRef = Just "$\ACKN", misOtrArchived = Just True, misOtrArchivedRef = Just "", misHidden = Just False, misHiddenRef = Just "!\a\163817", misConvRoleName = Just (fromJust (parseRoleName "kgb4ma4yj4k07f1x20fyw3c72hquskp01_d0z3as7ebm29puw728ej132sdoha1m88ex8yo_kv646b54vw_v6llj07zq8qzfsrgh56"))}

testObject_MemberUpdateData_user_8 :: MemberUpdateData
testObject_MemberUpdateData_user_8 = MemberUpdateData {misTarget = Nothing, misOtrMuted = Just False, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), misOtrMutedRef = Just "s~", misOtrArchived = Just False, misOtrArchivedRef = Just "\ACKP\v", misHidden = Just True, misHiddenRef = Just "\DC3\RS", misConvRoleName = Just (fromJust (parseRoleName "d0dg0qiu97eat9qbd6ziqu19jagfp89n"))}

testObject_MemberUpdateData_user_9 :: MemberUpdateData
testObject_MemberUpdateData_user_9 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), misOtrMuted = Nothing, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), misOtrMutedRef = Nothing, misOtrArchived = Just True, misOtrArchivedRef = Just "", misHidden = Just False, misHiddenRef = Nothing, misConvRoleName = Just (fromJust (parseRoleName "5cbfwwdai0h6bctmk9it8j38j91esopfi3xfi1n1fmwv4ieayi8gzdtay451y8h37veezkrystz4g49wgsv7ab12"))}

testObject_MemberUpdateData_user_10 :: MemberUpdateData
testObject_MemberUpdateData_user_10 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000001"))), misOtrMuted = Nothing, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), misOtrMutedRef = Just "n\1104041\16853", misOtrArchived = Just True, misOtrArchivedRef = Just "", misHidden = Just True, misHiddenRef = Nothing, misConvRoleName = Just (fromJust (parseRoleName "ffeb0qucf7nfaerti39parabwgbbuwe2mel5h5skepdriy7"))}

testObject_MemberUpdateData_user_11 :: MemberUpdateData
testObject_MemberUpdateData_user_11 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000001"))), misOtrMuted = Just True, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -2}), misOtrMutedRef = Just "p\36049", misOtrArchived = Just True, misOtrArchivedRef = Just "`", misHidden = Just False, misHiddenRef = Just "V", misConvRoleName = Just (fromJust (parseRoleName "kkdmdyqvaafi1taaxkd2n_75cd6edzu52vhzkw2lgmiwe_ghtxdx0yfuiow5w245cazi6b6__kxcw7nc7lveke1dpv2v8hcv4p3p07tcfrhsxe0br0w2yives34t"))}

testObject_MemberUpdateData_user_12 :: MemberUpdateData
testObject_MemberUpdateData_user_12 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000000"))), misOtrMuted = Nothing, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -2}), misOtrMutedRef = Nothing, misOtrArchived = Just True, misOtrArchivedRef = Just "\DC4&", misHidden = Nothing, misHiddenRef = Just "\177634", misConvRoleName = Nothing}

testObject_MemberUpdateData_user_13 :: MemberUpdateData
testObject_MemberUpdateData_user_13 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000000"))), misOtrMuted = Just False, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), misOtrMutedRef = Just "v", misOtrArchived = Nothing, misOtrArchivedRef = Just "N\993883o", misHidden = Just True, misHiddenRef = Just "", misConvRoleName = Just (fromJust (parseRoleName "uu19t9u0ff3w5kv8spa5zyc7fs6fhx42l0mgyrulb1fno5uo81vaj0i2474ag7dfq8sja4abkuhg00fhwquxkztuvqai"))}

testObject_MemberUpdateData_user_14 :: MemberUpdateData
testObject_MemberUpdateData_user_14 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000002"))), misOtrMuted = Just True, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}), misOtrMutedRef = Just "\183388~q", misOtrArchived = Just False, misOtrArchivedRef = Just "", misHidden = Just False, misHiddenRef = Just "&\EOT", misConvRoleName = Just (fromJust (parseRoleName "myz97junegyxat7oqglk92gtatzh189p4rq6aqmyjdd0bj41rg5qhvvpmi7a7ezofimw_x2vw70"))}

testObject_MemberUpdateData_user_15 :: MemberUpdateData
testObject_MemberUpdateData_user_15 = MemberUpdateData {misTarget = Nothing, misOtrMuted = Just True, misOtrMutedStatus = Nothing, misOtrMutedRef = Just "\ENQ\1012859j", misOtrArchived = Nothing, misOtrArchivedRef = Just ">\28014d", misHidden = Just False, misHiddenRef = Just "", misConvRoleName = Nothing}

testObject_MemberUpdateData_user_16 :: MemberUpdateData
testObject_MemberUpdateData_user_16 = MemberUpdateData {misTarget = Nothing, misOtrMuted = Just False, misOtrMutedStatus = Nothing, misOtrMutedRef = Just "", misOtrArchived = Just False, misOtrArchivedRef = Just "]", misHidden = Just True, misHiddenRef = Just "+8\1058595", misConvRoleName = Just (fromJust (parseRoleName "bod0gqeaes81c9qddjcdyqfi5fyzqalv3ppu5e11wx7hs8phvtccawwc7up7rvxylznt9jxtobt8y4oiww3mojghwp_v9twnatquv1mr"))}

testObject_MemberUpdateData_user_17 :: MemberUpdateData
testObject_MemberUpdateData_user_17 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000002"))), misOtrMuted = Nothing, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -2}), misOtrMutedRef = Just "\v", misOtrArchived = Just False, misOtrArchivedRef = Nothing, misHidden = Nothing, misHiddenRef = Just "\ACK\1055626", misConvRoleName = Nothing}

testObject_MemberUpdateData_user_18 :: MemberUpdateData
testObject_MemberUpdateData_user_18 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000002"))), misOtrMuted = Just True, misOtrMutedStatus = Nothing, misOtrMutedRef = Nothing, misOtrArchived = Just False, misOtrArchivedRef = Just "", misHidden = Just True, misHiddenRef = Just "\1078097b", misConvRoleName = Just (fromJust (parseRoleName "c8gq33xy3yowglzoq9rk_x5hul1y8e4d9ceu90jhj3m6u236t7bmw1sss_l4dw_rksd5x7zhyhoji5gh4t5fo49h9lwm3xrlv3ntkmtjusfv0mzcy_c63jkf8oku0"))}

testObject_MemberUpdateData_user_19 :: MemberUpdateData
testObject_MemberUpdateData_user_19 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000002"))), misOtrMuted = Nothing, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), misOtrMutedRef = Nothing, misOtrArchived = Just True, misOtrArchivedRef = Just "I|\990067", misHidden = Just True, misHiddenRef = Nothing, misConvRoleName = Just (fromJust (parseRoleName "egpq8z5jbd3gqk0e0i4w5wul5vuwgo49jks0q3_n4gow"))}

testObject_MemberUpdateData_user_20 :: MemberUpdateData
testObject_MemberUpdateData_user_20 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001"))), misOtrMuted = Nothing, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -2}), misOtrMutedRef = Just "\1087369\49549p", misOtrArchived = Just False, misOtrArchivedRef = Nothing, misHidden = Just True, misHiddenRef = Just "", misConvRoleName = Just (fromJust (parseRoleName "towhht2x6vbbej8ez996qoed_txxn7errr31wnzli7zrq2zsd0mm2saxn4_u_7p1hr53o6t4bsojbpb3_kr4ygseuz0f9kklmyjvs3_n5e5yw0mp2y6ve"))}
