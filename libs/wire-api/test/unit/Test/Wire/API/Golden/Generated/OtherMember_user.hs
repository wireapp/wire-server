{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.OtherMember_user where
import Data.Id ( Id(Id) )
import Imports ( Maybe(Just, Nothing), fromJust )
import qualified Data.UUID as UUID ( fromString )
import Wire.API.Conversation ( OtherMember(..) )
import Wire.API.Conversation.Role ( parseRoleName )
import Wire.API.Provider.Service
    ( ServiceRef(ServiceRef, _serviceRefId, _serviceRefProvider) )

testObject_OtherMember_user_1 :: OtherMember
testObject_OtherMember_user_1 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000003-0000-000c-0000-001600000001"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "6d"))}
testObject_OtherMember_user_2 :: OtherMember
testObject_OtherMember_user_2 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000500000020"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000100000002"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000002")))}), omConvRoleName = (fromJust (parseRoleName "gpg97fi9xb2oogvo03co735jfwab6d5r7ix3m9qf_qw6dsf1ratj7hy5c3inyebekprvcnbgj0npkzcqe55jptkvtsx199r7s0klkbc820ak6so2ndyafp"))}
testObject_OtherMember_user_3 :: OtherMember
testObject_OtherMember_user_3 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000003-0000-0012-0000-000f0000001e"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "unqgx12ns1clky3xwka8issutwu8zj301zctghm1eeivmywmbiq6jen0z3vrz38_inzfvarx1c88t3wnio9krk4x2f"))}
testObject_OtherMember_user_4 :: OtherMember
testObject_OtherMember_user_4 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000016-0000-0013-0000-000200000013"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000300000003")))}), omConvRoleName = (fromJust (parseRoleName "tpmoghsd9uj4d9rh59vknkllqme78kszgqwwzdbohn64iyadpojo1uxg40plar9cfp3gj25ro4zxq0duhichcccpcqphmztfu8t2nt2zvnrmsitg0xe9dopi"))}
testObject_OtherMember_user_5 :: OtherMember
testObject_OtherMember_user_5 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000008-0000-0001-0000-00180000000e"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000400000004"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000400000004")))}), omConvRoleName = (fromJust (parseRoleName "_4lh0dkjw185skha8z56m0ptrelvckvfuhsnmjt7m6g3r"))}
testObject_OtherMember_user_6 :: OtherMember
testObject_OtherMember_user_6 = OtherMember {omId = (Id (fromJust (UUID.fromString "0000000a-0000-0004-0000-001200000010"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000004-0000-0001-0000-000300000003")))}), omConvRoleName = (fromJust (parseRoleName "kabf1hccab"))}
testObject_OtherMember_user_7 :: OtherMember
testObject_OtherMember_user_7 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0015-0000-000b0000000d"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000400000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000000")))}), omConvRoleName = (fromJust (parseRoleName "3ga340wghggalod2k_1zevpi4y5sbwhn8_ms50xt41bi4jl7o"))}
testObject_OtherMember_user_8 :: OtherMember
testObject_OtherMember_user_8 = OtherMember {omId = (Id (fromJust (UUID.fromString "0000001b-0000-0014-0000-000d00000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000200000003"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000300000003")))}), omConvRoleName = (fromJust (parseRoleName "wy6d7_q2tcsurzj_ew_0gkp36bmdr6rozq04fmeixbs8t9fb_ap3rx5g9h60avvqmpsb3wjkr_1q029x4c6l58aqg9xp65v_7f98brd94wodmb"))}
testObject_OtherMember_user_9 :: OtherMember
testObject_OtherMember_user_9 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000009-0000-000b-0000-000400000017"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000300000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000004")))}), omConvRoleName = (fromJust (parseRoleName "x4hco7ga_u6_pkhsb5jy9tvvn665pik6yycekeomw_ody83n99cr571z2m0si1yolh2u7_n4c3010mncnjqh2d_s2wsd7bhvza8cbth"))}
testObject_OtherMember_user_10 :: OtherMember
testObject_OtherMember_user_10 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000007-0000-001e-0000-000600000003"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "qrm5266bawrn2v7yph_ztlzaxp23kie06wswl_l3gk2o9yb726vlrvdnzik8u_wanc_k3hmml3ofyu9muu"))}
testObject_OtherMember_user_11 :: OtherMember
testObject_OtherMember_user_11 = OtherMember {omId = (Id (fromJust (UUID.fromString "0000000c-0000-0020-0000-002000000012"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000300000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000000000000")))}), omConvRoleName = (fromJust (parseRoleName "b6ksjoqbczdh2w57eq0_wag5k__hrr87_x1qyy0piq57o2ozrx7"))}
testObject_OtherMember_user_12 :: OtherMember
testObject_OtherMember_user_12 = OtherMember {omId = (Id (fromJust (UUID.fromString "0000000b-0000-0011-0000-001c0000001c"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000400000001")))}), omConvRoleName = (fromJust (parseRoleName "vb7v19jp1evqzj7dl7ydo9dhf6u22vj3exorcirqsdoc37t5nzovca2r58dexx3s7qgpzuk77_bsu5qte42mk903g0ilq944yw8kdkpye3pvi82mpjebkkr"))}
testObject_OtherMember_user_13 :: OtherMember
testObject_OtherMember_user_13 = OtherMember {omId = (Id (fromJust (UUID.fromString "0000001d-0000-0014-0000-000300000008"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000400000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000004")))}), omConvRoleName = (fromJust (parseRoleName "x71z"))}
testObject_OtherMember_user_14 :: OtherMember
testObject_OtherMember_user_14 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000002-0000-000e-0000-000800000017"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000004"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000002")))}), omConvRoleName = (fromJust (parseRoleName "lhx7nzbl8idhf7p33l0gn0qapri6yjed4ikhvecnpf83rjq6us7eju8q40kwmba1n"))}
testObject_OtherMember_user_15 :: OtherMember
testObject_OtherMember_user_15 = OtherMember {omId = (Id (fromJust (UUID.fromString "0000000f-0000-0009-0000-000000000020"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000300000003"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000400000003")))}), omConvRoleName = (fromJust (parseRoleName "1j0nr8q958y785231ofj6nbitqs3s21fkj"))}
testObject_OtherMember_user_16 :: OtherMember
testObject_OtherMember_user_16 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000012-0000-000c-0000-000100000013"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "y8ix4o9mucb"))}
testObject_OtherMember_user_17 :: OtherMember
testObject_OtherMember_user_17 = OtherMember {omId = (Id (fromJust (UUID.fromString "0000001d-0000-0003-0000-00100000001c"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000400000002"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000400000001")))}), omConvRoleName = (fromJust (parseRoleName "4rg0kurnld6w3sbjtn_2r2sx5p94q4ba78dxa6_lv6trsowsm0956jtbgkj5w4vqnp0tks1e"))}
testObject_OtherMember_user_18 :: OtherMember
testObject_OtherMember_user_18 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0017-0000-001300000005"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000400000004"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000100000001")))}), omConvRoleName = (fromJust (parseRoleName "edm5lu3ihulzsam8tntrrc7xi_z_6edakqi21j18e_3lkzp9eeoq53eblny_ctpawbyrwdj3yn_m_kalr1dc_u8_o_9b"))}
testObject_OtherMember_user_19 :: OtherMember
testObject_OtherMember_user_19 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000006-0000-0010-0000-001600000014"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000400000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000100000003")))}), omConvRoleName = (fromJust (parseRoleName "nt1kti73f8oby9wi3ca7rgc7f0w4_954zyywvew05mma8ty6b1rgzbfym3tdud6nlsy"))}
testObject_OtherMember_user_20 :: OtherMember
testObject_OtherMember_user_20 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000018-0000-0013-0000-000e00000002"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000000000004"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000000000004")))}), omConvRoleName = (fromJust (parseRoleName "0ruhjhzxrdwcmzbun1vo8mqk1gervy2g_xivgny8wzdwl3v230artwum4yxbyd8wah550td55edugycz_u"))}
