{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Conversation_user where
import Data.Id ( Id(Id) )
import Data.Misc ( Milliseconds(Ms, ms) )
import Imports
    ( Bool(True, False), Maybe(Just, Nothing), fromJust )
import qualified Data.UUID as UUID ( fromString )
import Wire.API.Conversation
    ( Access(PrivateAccess, InviteAccess, LinkAccess, CodeAccess),
      AccessRole(TeamAccessRole, NonActivatedAccessRole,
                 PrivateAccessRole, ActivatedAccessRole),
      ConvType(RegularConv, ConnectConv, One2OneConv, SelfConv),
      Conversation(..),
      ReceiptMode(ReceiptMode, unReceiptMode),
      ConvMembers(ConvMembers, cmSelf, cmOthers),
      Member(Member, memId, memService, memOtrMuted, memOtrMutedStatus,
             memOtrMutedRef, memOtrArchived, memOtrArchivedRef, memHidden,
             memHiddenRef, memConvRoleName),
      MutedStatus(MutedStatus, fromMutedStatus),
      OtherMember(OtherMember, omId, omService, omConvRoleName) )
import Wire.API.Conversation.Role ( parseRoleName )
import Wire.API.Provider.Service
    ( ServiceRef(ServiceRef, _serviceRefId, _serviceRefProvider) )

testObject_Conversation_user_1 :: Conversation
testObject_Conversation_user_1 = Conversation {cnvId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000002"))), cnvType = RegularConv, cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000002"))), cnvAccess = [PrivateAccess], cnvAccessRole = ActivatedAccessRole, cnvName = Just "W\34965", cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), memOtrMutedRef = Nothing, memOtrArchived = True, memOtrArchivedRef = Nothing, memHidden = True, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "gy6c__2oyz49s2eqdnr5cfutay6_k6qblfdd_4nqbkh1k33psfz3e5d6j_n6bl27gqrp7dbodpfjm2xusps0xbsodxy9wepervlhn9tvrkp2qaqu08gw6ahhr7w"))}, cmOthers = [OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "g2ddpuujdg7e6dkfs7n67q0du0ljvha1gjsmhayma4kww3scjnx8s6ckvk"))}]}, cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), cnvMessageTimer = Nothing, cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 0})}
testObject_Conversation_user_2 :: Conversation
testObject_Conversation_user_2 = Conversation {cnvId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000000"))), cnvType = ConnectConv, cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000002"))), cnvAccess = [], cnvAccessRole = PrivateAccessRole, cnvName = Just "(t\ENQ", cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), memService = Nothing, memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), memOtrMutedRef = Nothing, memOtrArchived = False, memOtrArchivedRef = Nothing, memHidden = False, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "b6d3vt47cx1b0h9m9k3dq_yp_t7l7btj3"))}, cmOthers = []}, cnvTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), cnvMessageTimer = Just (Ms {ms = 3468412136670074}), cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 1})}
testObject_Conversation_user_3 :: Conversation
testObject_Conversation_user_3 = Conversation {cnvId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), cnvType = SelfConv, cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000001"))), cnvAccess = [LinkAccess], cnvAccessRole = PrivateAccessRole, cnvName = Nothing, cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), memService = Nothing, memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}), memOtrMutedRef = Just "", memOtrArchived = False, memOtrArchivedRef = Nothing, memHidden = True, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "oh6j6oma1_99ccws20iqt7iuqse50egnrxn2ov1ysmpfrzuengit_s1t2owpsijawucmgwga8eeum0_086"))}, cmOthers = [OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "oltliasom3_fyhjdgfjdn50s6vb"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "ja2nnxfpo6gxdges94v74pd9qec4938jt2"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "zquql2colty2tg0xs7fhe8invppwqrwgjfle9u19s9d5p1wzw0x7e2v"))}]}, cnvTeam = Nothing, cnvMessageTimer = Just (Ms {ms = 4612370228800205}), cnvReceiptMode = Nothing}
testObject_Conversation_user_4 :: Conversation
testObject_Conversation_user_4 = Conversation {cnvId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000000"))), cnvType = One2OneConv, cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), cnvAccess = [CodeAccess,CodeAccess], cnvAccessRole = NonActivatedAccessRole, cnvName = Just ",J\2453", cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), memOtrMutedRef = Nothing, memOtrArchived = False, memOtrArchivedRef = Just "", memHidden = False, memHiddenRef = Nothing, memConvRoleName = (fromJust (parseRoleName "k_4s4f3avnrukrqbdn2bzw2wvf480qgkjwbnoba8oordbu_q792t268bfpjznhwlw8bdhmp3qjsln5y3hngkwkq"))}, cmOthers = [OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))}), omConvRoleName = (fromJust (parseRoleName "eoj5nx3iqlfr6klar1xkp"))}]}, cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), cnvMessageTimer = Nothing, cnvReceiptMode = Just (ReceiptMode {unReceiptMode = -2})}
testObject_Conversation_user_5 :: Conversation
testObject_Conversation_user_5 = Conversation {cnvId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000002"))), cnvType = ConnectConv, cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000001"))), cnvAccess = [], cnvAccessRole = NonActivatedAccessRole, cnvName = Nothing, cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))}), memOtrMuted = False, memOtrMutedStatus = Nothing, memOtrMutedRef = Just "", memOtrArchived = False, memOtrArchivedRef = Just "", memHidden = True, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "sztlx756rucnni5whk29a28c36gx23g676bowrc6068d0brmjcjgg5xyy9eqfju_ls5stu9_pblvpj3z4ck8jfvb1o77fu66fzpde6s18lc6bwd08k6sm4mk15xili"))}, cmOthers = []}, cnvTeam = Nothing, cnvMessageTimer = Just (Ms {ms = 3581708156749438}), cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 2})}
testObject_Conversation_user_6 :: Conversation
testObject_Conversation_user_6 = Conversation {cnvId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000001"))), cnvType = RegularConv, cnvCreator = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000002"))), cnvAccess = [InviteAccess], cnvAccessRole = TeamAccessRole, cnvName = Nothing, cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), memService = Nothing, memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), memOtrMutedRef = Just "", memOtrArchived = False, memOtrArchivedRef = Just "", memHidden = False, memHiddenRef = Nothing, memConvRoleName = (fromJust (parseRoleName "eeqqjs1hq8e_ctjy95gw_hzhzrdn"))}, cmOthers = []}, cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), cnvMessageTimer = Just (Ms {ms = 4061395635315350}), cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 0})}
testObject_Conversation_user_7 :: Conversation
testObject_Conversation_user_7 = Conversation {cnvId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000002"))), cnvType = One2OneConv, cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000002"))), cnvAccess = [], cnvAccessRole = NonActivatedAccessRole, cnvName = Just "k\"", cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))}), memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), memOtrMutedRef = Just "", memOtrArchived = False, memOtrArchivedRef = Nothing, memHidden = True, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "3s74640y9229rf3pwd_tkiahnpyjsrw832y_a5o95b1ifng1ol2dpvflyi5dmfpt7"))}, cmOthers = []}, cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))), cnvMessageTimer = Just (Ms {ms = 1372241780129051}), cnvReceiptMode = Just (ReceiptMode {unReceiptMode = -1})}
testObject_Conversation_user_8 :: Conversation
testObject_Conversation_user_8 = Conversation {cnvId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000001"))), cnvType = SelfConv, cnvCreator = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000000"))), cnvAccess = [LinkAccess,PrivateAccess], cnvAccessRole = NonActivatedAccessRole, cnvName = Just "\1032207s", cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), memOtrMutedRef = Just "", memOtrArchived = True, memOtrArchivedRef = Just "", memHidden = True, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "vo77o7rtovl6_30i6do_cm_1vd9aflt9p7rhjsibyqykypyoigf5ff1acw8p8j28d67xj5k_4v9p4yzmv2xsewgrvhedkrjofbtb"))}, cmOthers = []}, cnvTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000001"))), cnvMessageTimer = Just (Ms {ms = 6352404235302050}), cnvReceiptMode = Nothing}
testObject_Conversation_user_9 :: Conversation
testObject_Conversation_user_9 = Conversation {cnvId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000000"))), cnvType = RegularConv, cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000000"))), cnvAccess = [PrivateAccess,CodeAccess], cnvAccessRole = PrivateAccessRole, cnvName = Just "z=", cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), memService = Nothing, memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), memOtrMutedRef = Just "", memOtrArchived = False, memOtrArchivedRef = Nothing, memHidden = True, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "psnwji9b0g8s_00k8p5zn6wcx65n13w44vhfhoy0ib5a283oveywce9u"))}, cmOthers = []}, cnvTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000002"))), cnvMessageTimer = Nothing, cnvReceiptMode = Just (ReceiptMode {unReceiptMode = -2})}
testObject_Conversation_user_10 :: Conversation
testObject_Conversation_user_10 = Conversation {cnvId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), cnvType = ConnectConv, cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), cnvAccess = [PrivateAccess,PrivateAccess,InviteAccess,LinkAccess,LinkAccess,LinkAccess,CodeAccess,PrivateAccess,InviteAccess], cnvAccessRole = TeamAccessRole, cnvName = Nothing, cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}), memOtrMutedRef = Just "", memOtrArchived = False, memOtrArchivedRef = Just "", memHidden = True, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "v3j8oof114ml84pisfxf2efbrl60voz30dc7shymtts2yv50653ed79u_511_xrm65vgrnnhadwkmwq0ce36w85zpksa0s5nbh2t_"))}, cmOthers = [OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "w0rpcekv5kpb31caetl8n88rjcp9"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))}), omConvRoleName = (fromJust (parseRoleName "2wmk4e3vrt1z1w2y5y540_pd0uax2qggs65kzbub1794uz3ea7vn6h01r6hsrfvu3fy1ea5a9ogwh5t6ix8z5h3v8wwv1qlr"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))}), omConvRoleName = (fromJust (parseRoleName "2yej_y68gzlzq7kmf70"))}]}, cnvTeam = Nothing, cnvMessageTimer = Just (Ms {ms = 1112447118981542}), cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 1})}
testObject_Conversation_user_11 :: Conversation
testObject_Conversation_user_11 = Conversation {cnvId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000002"))), cnvType = SelfConv, cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), cnvAccess = [PrivateAccess,LinkAccess], cnvAccessRole = ActivatedAccessRole, cnvName = Just "", cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))}), memOtrMuted = False, memOtrMutedStatus = Nothing, memOtrMutedRef = Just "", memOtrArchived = True, memOtrArchivedRef = Nothing, memHidden = False, memHiddenRef = Nothing, memConvRoleName = (fromJust (parseRoleName "kw4r44lzyo5vxqocugfqj_1tfoahvhav59gfhtdctvejyrtyclawn2sk7v9ssjpb0y86709qkwo2x"))}, cmOthers = [OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "yeynv11vsncsd3mb1tbyk32cgf8d8ykcatyt2vnr7uhd7d_awxxw2thfz8"))}]}, cnvTeam = Nothing, cnvMessageTimer = Just (Ms {ms = 8794286430901970}), cnvReceiptMode = Just (ReceiptMode {unReceiptMode = -2})}
testObject_Conversation_user_12 :: Conversation
testObject_Conversation_user_12 = Conversation {cnvId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000002"))), cnvType = ConnectConv, cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000001"))), cnvAccess = [PrivateAccess,PrivateAccess], cnvAccessRole = NonActivatedAccessRole, cnvName = Just "vg", cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))}), memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}), memOtrMutedRef = Just "", memOtrArchived = False, memOtrArchivedRef = Just "", memHidden = True, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "mc_gti9g9h6yqmg1vcii902dp6rl7h7jyn3d"))}, cmOthers = []}, cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000002"))), cnvMessageTimer = Just (Ms {ms = 3116519453072491}), cnvReceiptMode = Nothing}
testObject_Conversation_user_13 :: Conversation
testObject_Conversation_user_13 = Conversation {cnvId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000002"))), cnvType = SelfConv, cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000000"))), cnvAccess = [PrivateAccess,CodeAccess], cnvAccessRole = TeamAccessRole, cnvName = Just "1l\GS", cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), memOtrMutedRef = Just "", memOtrArchived = True, memOtrArchivedRef = Just "", memHidden = False, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "_3dkan__s6c3d6glxixb5fkmdketscgsan__38n_t0t1n5lrt1ulgxwutn8p610xorjzq_3yi375ew3r93awoxjs00i65mtru8itsjemm"))}, cmOthers = [OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))}), omConvRoleName = (fromJust (parseRoleName "maub4cb93emjbezp4udn1xxpmjjzjap_63mhpsk1q2o"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))}), omConvRoleName = (fromJust (parseRoleName "4oda7rk1vm1dyjn21ew1vs9oqvhqec"))}]}, cnvTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000000"))), cnvMessageTimer = Just (Ms {ms = 1060240338578873}), cnvReceiptMode = Just (ReceiptMode {unReceiptMode = -1})}
testObject_Conversation_user_14 :: Conversation
testObject_Conversation_user_14 = Conversation {cnvId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000002"))), cnvType = ConnectConv, cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001"))), cnvAccess = [], cnvAccessRole = TeamAccessRole, cnvName = Nothing, cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), memService = Nothing, memOtrMuted = False, memOtrMutedStatus = Nothing, memOtrMutedRef = Just "", memOtrArchived = False, memOtrArchivedRef = Nothing, memHidden = True, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "gyp5fc3vszb2nhu7k0rrrnmneyabcs7gf7jdxaa0"))}, cmOthers = []}, cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000000"))), cnvMessageTimer = Just (Ms {ms = 3851079043048171}), cnvReceiptMode = Nothing}
testObject_Conversation_user_15 :: Conversation
testObject_Conversation_user_15 = Conversation {cnvId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000001"))), cnvType = One2OneConv, cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000001"))), cnvAccess = [], cnvAccessRole = ActivatedAccessRole, cnvName = Nothing, cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))}), memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}), memOtrMutedRef = Just "", memOtrArchived = True, memOtrArchivedRef = Just "", memHidden = True, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "rqyrswu613xms48g1qyjvil_xlx8i7a4jol6gqwby84x5thd1ctmknygje"))}, cmOthers = []}, cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), cnvMessageTimer = Just (Ms {ms = 1782298522775766}), cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 0})}
testObject_Conversation_user_16 :: Conversation
testObject_Conversation_user_16 = Conversation {cnvId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), cnvType = One2OneConv, cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), cnvAccess = [LinkAccess], cnvAccessRole = ActivatedAccessRole, cnvName = Nothing, cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))}), memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), memOtrMutedRef = Just "", memOtrArchived = False, memOtrArchivedRef = Just "", memHidden = True, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "eaafeui4sj7gmk5qqorhpjhxzxgp_g9ojwlr0em2zii7t4osx"))}, cmOthers = [OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))}), omConvRoleName = (fromJust (parseRoleName "bztx5wzmgihftpu7t2xtwjeg7e_e15zloffuh3w22zb_g26f5m517ge5sdnpg53v2tz4dti"))}]}, cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000001"))), cnvMessageTimer = Just (Ms {ms = 271896641536491}), cnvReceiptMode = Nothing}
testObject_Conversation_user_17 :: Conversation
testObject_Conversation_user_17 = Conversation {cnvId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000002"))), cnvType = RegularConv, cnvCreator = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000000"))), cnvAccess = [PrivateAccess], cnvAccessRole = ActivatedAccessRole, cnvName = Just "", cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), memOtrMutedRef = Just "", memOtrArchived = False, memOtrArchivedRef = Just "", memHidden = True, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "vbca"))}, cmOthers = []}, cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), cnvMessageTimer = Nothing, cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 0})}
testObject_Conversation_user_18 :: Conversation
testObject_Conversation_user_18 = Conversation {cnvId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000001"))), cnvType = SelfConv, cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000002"))), cnvAccess = [LinkAccess,CodeAccess], cnvAccessRole = PrivateAccessRole, cnvName = Just "", cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))}), memOtrMuted = True, memOtrMutedStatus = Nothing, memOtrMutedRef = Just "", memOtrArchived = True, memOtrArchivedRef = Just "", memHidden = False, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "ism87y1cs"))}, cmOthers = [OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))}), omConvRoleName = (fromJust (parseRoleName "pxz83af847sx1fgpvcalewn4gebuatl37ianal_9pjor4rgh6k19477y5edjr8tec38iq48js2wfxw1q8m43drf14dn9rvn0ffzyse"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))}), omConvRoleName = (fromJust (parseRoleName "l1_zocc5wq4mrda_ae3rv_a37b528m9t107zcfpo7fbixa5s55698mkwuzn5g63yu9p_t3r1d0gbyti4e3zblnlgqt7ya13endty7husys6c9nxfp35u092k_5il72il"))}]}, cnvTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000002"))), cnvMessageTimer = Just (Ms {ms = 5805268410964021}), cnvReceiptMode = Just (ReceiptMode {unReceiptMode = -2})}
testObject_Conversation_user_19 :: Conversation
testObject_Conversation_user_19 = Conversation {cnvId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000001"))), cnvType = RegularConv, cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000002"))), cnvAccess = [LinkAccess,CodeAccess,PrivateAccess,CodeAccess], cnvAccessRole = ActivatedAccessRole, cnvName = Just "a", cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))}), memOtrMuted = False, memOtrMutedStatus = Nothing, memOtrMutedRef = Just "", memOtrArchived = False, memOtrArchivedRef = Nothing, memHidden = True, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "wlun4f7x3ag0257hrn0604kp1uqge3ely1yq2wjljol6ym300qppjze_iuylwh16jl40h6vzovat3dcy3v72rwx_4ker5_g5"))}, cmOthers = [OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))}), omConvRoleName = (fromJust (parseRoleName "yag2plp6kcbicdi1dco8z0haewubh2vop6_fusds6rcerebn2xuhblkspa7ev8swnn5zbue7ynlc12eim2uey"))}]}, cnvTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000002"))), cnvMessageTimer = Just (Ms {ms = 1635340379846493}), cnvReceiptMode = Just (ReceiptMode {unReceiptMode = -1})}
testObject_Conversation_user_20 :: Conversation
testObject_Conversation_user_20 = Conversation {cnvId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000002"))), cnvType = RegularConv, cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001"))), cnvAccess = [PrivateAccess], cnvAccessRole = TeamAccessRole, cnvName = Just "\ESC\SOH\STX", cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), memOtrMutedRef = Just "", memOtrArchived = False, memOtrArchivedRef = Just "", memHidden = True, memHiddenRef = Nothing, memConvRoleName = (fromJust (parseRoleName "aeqzoy9o_1yy2ot71zdxf029xc8fzffu4x6mqmjmp100ibq2bc7g83yn2dxjr4zp8gztznfmfz78fuj_8thci7poaasvn_d6vw1okbexh7gru5wmp4"))}, cmOthers = [OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "fs34chfz2pe0wsnad46nkwsg9el2sa65kwibbpp"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))}), omConvRoleName = (fromJust (parseRoleName "0uvgzqu0rbcr7xobmg05t8aer7ltwaaxat8v_lw_cqccy1t8wd1r26mgo_wjgqm5sbzlej5n"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "x4tva02a9xyc0q5oq9_voq3ztnt4y6ug7mln8d"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "cl4zdog8wj5kkqko362tkqzvoe_n04gpdi2k_6__nhcxr_he"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))}), omConvRoleName = (fromJust (parseRoleName "8d_2ldc7jcctvypiit47n3i75jrj7rjeatuhtr9ev50k4rpzxxyvzgbmuhanplkst2vz20kt800620h5zwwa7azvyudzaynv5ikm"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))}), omConvRoleName = (fromJust (parseRoleName "xwqyr4gid5i_yq_whl2k3zvgg89m0ylkl_60ooyjx7dyyxplkax8htwhfrano_xfxmtkpcfgxocyp0uhjcemea1k9u7avnm8qy_13li9mhk0dn3wu_9uyzzp_"))}]}, cnvTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), cnvMessageTimer = Just (Ms {ms = 4332666384381030}), cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 0})}
