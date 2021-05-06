{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.NewConvUnmanaged_user where
import Data.Id ( Id(Id) )
import Data.Misc ( Milliseconds(Ms, ms) )
import qualified Data.Set as Set ( fromList )
import Imports ( Bool(False), Maybe(Just, Nothing), fromJust )
import qualified Data.UUID as UUID ( fromString )
import Wire.API.Conversation
    ( Access(LinkAccess, PrivateAccess, CodeAccess, InviteAccess),
      AccessRole(PrivateAccessRole, ActivatedAccessRole, TeamAccessRole,
                 NonActivatedAccessRole),
      ConvTeamInfo(ConvTeamInfo, cnvTeamId, cnvManaged),
      NewConv(NewConv, newConvUsers, newConvName, newConvAccess,
              newConvAccessRole, newConvTeam, newConvMessageTimer,
              newConvReceiptMode, newConvUsersRole),
      NewConvUnmanaged(..),
      ReceiptMode(ReceiptMode, unReceiptMode) )
import Wire.API.Conversation.Role ( parseRoleName )

testObject_NewConvUnmanaged_user_1 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_1 = NewConvUnmanaged (NewConv {newConvUsers = [], newConvName = Just "\160296>\1021569", newConvAccess = Set.fromList [InviteAccess,CodeAccess], newConvAccessRole = Nothing, newConvTeam = Nothing, newConvMessageTimer = Just (Ms {ms = 2544733731596991}), newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "q4w4zwsxg9m0p6e4c"))})
testObject_NewConvUnmanaged_user_2 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_2 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))], newConvName = Just "", newConvAccess = Set.fromList [InviteAccess], newConvAccessRole = Nothing, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), cnvManaged = False}), newConvMessageTimer = Just (Ms {ms = 4696411905315899}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 1}), newConvUsersRole = (fromJust (parseRoleName "dr70g5kgv1k0r2l9g3ln2zgolqcenod9y_a5ca9yev2qmtcy4p2wr92"))})
testObject_NewConvUnmanaged_user_3 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_3 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))], newConvName = Just "8", newConvAccess = Set.fromList [InviteAccess,LinkAccess], newConvAccessRole = Just PrivateAccessRole, newConvTeam = Nothing, newConvMessageTimer = Just (Ms {ms = 3716646025226973}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 1}), newConvUsersRole = (fromJust (parseRoleName "cvgw3u3yks1hj_e93h91z3qhxs"))})
testObject_NewConvUnmanaged_user_4 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_4 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))], newConvName = Just "", newConvAccess = Set.fromList [], newConvAccessRole = Nothing, newConvTeam = Nothing, newConvMessageTimer = Just (Ms {ms = 3268880126641313}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -1}), newConvUsersRole = (fromJust (parseRoleName "qvb9961462li"))})
testObject_NewConvUnmanaged_user_5 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_5 = NewConvUnmanaged (NewConv {newConvUsers = [], newConvName = Just "\EM", newConvAccess = Set.fromList [CodeAccess], newConvAccessRole = Just PrivateAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), cnvManaged = False}), newConvMessageTimer = Just (Ms {ms = 5609874269731025}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -1}), newConvUsersRole = (fromJust (parseRoleName "6yuq3jqonhm9ap318h_jjvh2ms8aptoldlvaq4_2b_xrwdymaestye9kc9q"))})
testObject_NewConvUnmanaged_user_6 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_6 = NewConvUnmanaged (NewConv {newConvUsers = [], newConvName = Just "", newConvAccess = Set.fromList [InviteAccess], newConvAccessRole = Just PrivateAccessRole, newConvTeam = Nothing, newConvMessageTimer = Just (Ms {ms = 8596075222064762}), newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "hjw1cgxfkj43ww5o8u_8n8rnz21ldlotk5med82x79u9_u1rjwm_ybkaubhcwsgbnu2gcnlbbfe1z6jplwu89d2nplqebhhy8x9tpz9"))})
testObject_NewConvUnmanaged_user_7 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_7 = NewConvUnmanaged (NewConv {newConvUsers = [], newConvName = Just "\ESC", newConvAccess = Set.fromList [PrivateAccess,InviteAccess], newConvAccessRole = Just ActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), cnvManaged = False}), newConvMessageTimer = Just (Ms {ms = 7422276704696853}), newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "r686bo0zf6sa8selhjual1qud90bo6toauj36ha"))})
testObject_NewConvUnmanaged_user_8 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_8 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))], newConvName = Just "\141635}", newConvAccess = Set.fromList [PrivateAccess,LinkAccess], newConvAccessRole = Nothing, newConvTeam = Nothing, newConvMessageTimer = Just (Ms {ms = 3451097295803180}), newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "zp4651ptylju3api6s7ckj037y5hto7gr6ox9ldvfrmr5n0l1cwn"))})
testObject_NewConvUnmanaged_user_9 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_9 = NewConvUnmanaged (NewConv {newConvUsers = [], newConvName = Just "6", newConvAccess = Set.fromList [], newConvAccessRole = Nothing, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), cnvManaged = False}), newConvMessageTimer = Just (Ms {ms = 8829844718438636}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -2}), newConvUsersRole = (fromJust (parseRoleName "95_wku0pa4meghagu639srlun0jmryh2_qecv2b_fh960yvw0uo34a6sbhnf6hhvp6re4ewcqni76z6_dne_rqtu"))})
testObject_NewConvUnmanaged_user_10 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_10 = NewConvUnmanaged (NewConv {newConvUsers = [], newConvName = Just "Wz\t", newConvAccess = Set.fromList [PrivateAccess], newConvAccessRole = Just PrivateAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), cnvManaged = False}), newConvMessageTimer = Just (Ms {ms = 7607214962151733}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -2}), newConvUsersRole = (fromJust (parseRoleName "k2y1fsuv8a6yqcx_ru4_s1b765ev0fhgc95862y149y5"))})
testObject_NewConvUnmanaged_user_11 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_11 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))], newConvName = Nothing, newConvAccess = Set.fromList [LinkAccess], newConvAccessRole = Just PrivateAccessRole, newConvTeam = Nothing, newConvMessageTimer = Nothing, newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -1}), newConvUsersRole = (fromJust (parseRoleName "7m2rigps7xx6ehfgy_4ltkuh4a3gm5k4_42w_vx23p4e6o0frbpzz1801jtue2jmom5rxd"))})
testObject_NewConvUnmanaged_user_12 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_12 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))], newConvName = Nothing, newConvAccess = Set.fromList [InviteAccess], newConvAccessRole = Just ActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), cnvManaged = False}), newConvMessageTimer = Just (Ms {ms = 6102484555485702}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -1}), newConvUsersRole = (fromJust (parseRoleName "83_i63si8og2qon1ocyd4pdpwyynye_7mre9t5tu94hmfh4yz_byjqx1769h65d9dro6f97u3zyj9h9r53iraeib8ymyk36ofkgv1y3irhg0tv"))})
testObject_NewConvUnmanaged_user_13 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_13 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))], newConvName = Just "w=-", newConvAccess = Set.fromList [CodeAccess], newConvAccessRole = Just NonActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), cnvManaged = False}), newConvMessageTimer = Just (Ms {ms = 6316039276979393}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -1}), newConvUsersRole = (fromJust (parseRoleName "89ts8ki4h"))})
testObject_NewConvUnmanaged_user_14 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_14 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))], newConvName = Just "['\ETB", newConvAccess = Set.fromList [LinkAccess], newConvAccessRole = Just PrivateAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), cnvManaged = False}), newConvMessageTimer = Nothing, newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -1}), newConvUsersRole = (fromJust (parseRoleName "fg0nn_5_ioojxtvptsvavajmyayhv4xxa9767f0e9qxh00y83222_ur4ctc2cx6poic23nxwh35oankuh8ylb54kyd3hxepa9ltfkele0yhy3fig3u7br8mn0sn_1r08"))})
testObject_NewConvUnmanaged_user_15 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_15 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))], newConvName = Nothing, newConvAccess = Set.fromList [LinkAccess,CodeAccess], newConvAccessRole = Just ActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), cnvManaged = False}), newConvMessageTimer = Just (Ms {ms = 5461311904578359}), newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "wut3r6uh5pa8jp12e846ojerhcuy4o5kel36b7laqqneob5k1erz6856_ouhk0nxx8fq4rh86idgfrdh2n8gnb974isiufe1onntlrn_b"))})
testObject_NewConvUnmanaged_user_16 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_16 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))], newConvName = Nothing, newConvAccess = Set.fromList [LinkAccess], newConvAccessRole = Just TeamAccessRole, newConvTeam = Nothing, newConvMessageTimer = Just (Ms {ms = 6880903625375126}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -2}), newConvUsersRole = (fromJust (parseRoleName "2e9sw5qvkaqpx172c8vsozsq4"))})
testObject_NewConvUnmanaged_user_17 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_17 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))], newConvName = Just "R\SYNG", newConvAccess = Set.fromList [PrivateAccess], newConvAccessRole = Just PrivateAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), cnvManaged = False}), newConvMessageTimer = Just (Ms {ms = 3435982559310780}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 1}), newConvUsersRole = (fromJust (parseRoleName "kfpj2zf0fg9eh03vbeg8kjwra85yb7o2sgi3nj8ahbbuigq5310g5ekuv7tyt22ruibttndjuc1k0mt102gmcktgf4_"))})
testObject_NewConvUnmanaged_user_18 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_18 = NewConvUnmanaged (NewConv {newConvUsers = [], newConvName = Just "//\159854", newConvAccess = Set.fromList [PrivateAccess], newConvAccessRole = Just NonActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), cnvManaged = False}), newConvMessageTimer = Just (Ms {ms = 4600761777486612}), newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "pt21h5oh6gj4jzokpa88bvqw60ar4xbt6pf5r1_kmj8vditjt21em2wbi6l8bhforqcjs_njfbycosk_55gh"))})
testObject_NewConvUnmanaged_user_19 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_19 = NewConvUnmanaged (NewConv {newConvUsers = [], newConvName = Just "\RS\1079101", newConvAccess = Set.fromList [PrivateAccess,LinkAccess,CodeAccess], newConvAccessRole = Nothing, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), cnvManaged = False}), newConvMessageTimer = Nothing, newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "1noz0_0zfnqvl6z"))})
testObject_NewConvUnmanaged_user_20 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_20 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))], newConvName = Just "\50580\DC2", newConvAccess = Set.fromList [InviteAccess,LinkAccess], newConvAccessRole = Just PrivateAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), cnvManaged = False}), newConvMessageTimer = Nothing, newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -1}), newConvUsersRole = (fromJust (parseRoleName "97op9coab0isoz604xqsa8hrqnkrgc0a32vyubt8v5r1uujips25d6e59w8q"))})
