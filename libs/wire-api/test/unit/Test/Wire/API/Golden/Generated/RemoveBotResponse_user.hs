{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.RemoveBotResponse_user where

import Codec.MIME.Type (Type(..))
import qualified Codec.MIME.Type as MIME
import Control.Lens ((.~))
import Data.Code
import Data.Coerce
import Data.Currency
import Data.Domain
import Data.Handle
import Data.Id
import Data.ISO3166_CountryCodes
import Data.Json.Util
import Data.List1
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty (..))
import Data.Misc
import Data.PEM
import Data.Qualified
import Data.Range (unsafeRange)
import qualified Data.Set as Set
import Data.Text.Ascii
import Data.Time (secondsToNominalDiffTime)
import Imports hiding (LT, GT)
import qualified Data.LanguageCodes
import qualified Data.UUID as UUID
import Test.Tasty (testGroup, TestTree)
import URI.ByteString
import qualified Wire.API.Call.Config as CallConfig
import qualified Wire.API.User.Profile as User.Profile
import qualified Wire.API.Team.Conversation as Team.Conversation
import qualified Wire.API.Provider as Provider
import qualified Wire.API.Provider.Bot as Provider
import qualified Wire.API.Provider.External as Provider
import qualified Wire.API.Provider.Service as Provider
import qualified Wire.API.Provider.Service.Tag as Provider
import Data.Aeson
import GHC.Exts
import Wire.API.Asset
import Wire.API.Asset.V3.Resumable
import Wire.API.Call.Config
import Wire.API.Connection
import Wire.API.Conversation
import Wire.API.Conversation.Bot
import Wire.API.Conversation.Code
import Wire.API.Conversation.Member
import Wire.API.Conversation.Role
import Wire.API.Conversation.Typing
import Wire.API.CustomBackend
import Wire.API.Event.Conversation
import Wire.API.Message
import Wire.API.Notification (QueuedNotification, queuedNotification, QueuedNotificationList, queuedNotificationList)
import Wire.API.Properties
-- import Wire.API.Provider
import Wire.API.Provider.Bot
import Wire.API.Provider.External
import Wire.API.Provider.Service
-- import Wire.API.Provider.Service.Tag
import Wire.API.Push.Token hiding (Transport)
import qualified Wire.API.Push.Token as Push.Token
import Wire.API.Team
import Wire.API.Team.Role
-- import Wire.API.Team.SearchVisibility
import Wire.API.User
import Wire.API.User.Activation
import Wire.API.User.Auth
import Wire.API.User.Client
import Wire.API.User.Client.Prekey
import Wire.API.User.Handle
import Wire.API.User.Identity
import Wire.API.User.Password
import Wire.API.User.Profile
import Wire.API.User.RichInfo
import Wire.API.User.Search
import Wire.API.Wrapped
testObject_RemoveBotResponse_user_1 :: RemoveBotResponse
testObject_RemoveBotResponse_user_1 = RemoveBotResponse {rsRemoveBotEvent = (Event (MemberJoin) ((Id (fromJust (UUID.fromString "00003e1f-0000-1bf3-0000-245f00005f2f")))) ((Id (fromJust (UUID.fromString "00001707-0000-5eac-0000-2f4a000053cf")))) (read "1864-05-31 12:11:30.382 UTC") (Just (EdMembersJoin (SimpleMembers {mMembers = [SimpleMember {smId = (Id (fromJust (UUID.fromString "00000076-0000-001e-0000-00270000000d"))), smConvRoleName = (fromJust (parseRoleName "zivpo1ftwnf4zk9seoq5jn1hml0jrenci3v2vhrxr4w3sr410wp68rd7069ywamg3yocj86xyw2j19_1qt5zz92epv629roynyvz033neue5nm4xxvgwbucf9h0e_r0x"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000003c-0000-003f-0000-003f00000040"))), smConvRoleName = (fromJust (parseRoleName "zn_mc7t1h3jlpgbozyrs7s0"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000048-0000-0075-0000-00610000005b"))), smConvRoleName = (fromJust (parseRoleName "i5gj1xl"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000066-0000-0004-0000-00610000002c"))), smConvRoleName = (fromJust (parseRoleName "03m20o0fw1jue2ieuw344vjeyb2w1v_2a40s0wd0qe_kpc8tnyxkupa6tzagm9i2z"))}]}))))}
testObject_RemoveBotResponse_user_2 :: RemoveBotResponse
testObject_RemoveBotResponse_user_2 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvCodeDelete) ((Id (fromJust (UUID.fromString "00005ade-0000-3bc4-0000-282f0000363e")))) ((Id (fromJust (UUID.fromString "0000605c-0000-19b1-0000-262800005ffe")))) (read "1864-04-09 14:42:17.773 UTC") (Nothing))}
testObject_RemoveBotResponse_user_3 :: RemoveBotResponse
testObject_RemoveBotResponse_user_3 = RemoveBotResponse {rsRemoveBotEvent = (Event (MemberJoin) ((Id (fromJust (UUID.fromString "000079f9-0000-02d6-0000-274b00001825")))) ((Id (fromJust (UUID.fromString "00003025-0000-2860-0000-66d0000017df")))) (read "1864-05-24 14:04:06.532 UTC") (Just (EdMembersJoin (SimpleMembers {mMembers = [SimpleMember {smId = (Id (fromJust (UUID.fromString "00000025-0000-003e-0000-001100000077"))), smConvRoleName = (fromJust (parseRoleName "m_drvevqxr_4u442kwmig99s1x0hivc1vew0rk17isz93ipvlc_9xuffhzx9rdy44x96xcy0kk5iowqd59i0rqxtg9_w2xd4ydm1g3fz9uq4rasb2pb7j"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000003a-0000-0055-0000-005c0000006c"))), smConvRoleName = (fromJust (parseRoleName "nwd98wtozqln0id9u1on5i19uv4kjonohjohommkisvau_1hl00uv4bpfapjoz0fkylcxiuj10gdp_sp0oohkwtxaaeqkhjho8qmxoazsrlvhjwh2yc2"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000001b-0000-0026-0000-006200000026"))), smConvRoleName = (fromJust (parseRoleName "hrrp4fy28kqc_5h1vlrrkr8sdrgzl3q8ob_h0if_hxdf4zwj7tavfhr_dky705y590h9yh0yim7p2"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000007-0000-0071-0000-00720000006d"))), smConvRoleName = (fromJust (parseRoleName "su9fna5djnx6e4b2qip_iclgc6pcuy6lqhvuh5q6lwixhttwgmq61cquh32na42qtxslfnqrrbw526r_u6aj_273_g5mujh"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000022-0000-0034-0000-00740000003d"))), smConvRoleName = (fromJust (parseRoleName "b0ergo4jfi591lhp29nk_knxixv48flbn32ho5yr_foxwi21"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000000c-0000-0071-0000-00390000006a"))), smConvRoleName = (fromJust (parseRoleName "y42_o6xk14hs5zmtpuxcstdlcu6u4h5tut38d8ntcgc0vtsab7pkkt26v48xtbgosegiq_4i83yhnb4b40upa2v2dlab"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000029-0000-0028-0000-000f0000007d"))), smConvRoleName = (fromJust (parseRoleName "fygp6hg4edp2t8uo3dxpb0qoot9u13ra9syb3td21csbr_54cyoavbigoqs5jdg762bhfyz49lbcu98syg3q0k54xmv_61g6nyo1hjrsmww55pee74szmg"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000007e-0000-0042-0000-000c0000006a"))), smConvRoleName = (fromJust (parseRoleName "b3htepbgdbuverv0329qw9lw7ju85lslng5mrnu4il5d3pn7ub74uv9edmsmdmki528x2a2x0rhjw3wlqymdtiyqryc2_3lo1go2tqs5u4nbx1n17g84y2egn9"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000034-0000-0011-0000-002800000054"))), smConvRoleName = (fromJust (parseRoleName "ctt1lg247laxfci_1xag11hjnt0rx93okba6iwxzo2ukvu4p1scnqfv21uqf0h5rn3iany0bd0yo83by2euy"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000015-0000-0069-0000-002700000035"))), smConvRoleName = (fromJust (parseRoleName "4skw0hz4gnshtg1zfy7yvq5mzymoyzubb2q_zog4sq9g1d2s9xbe"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000007c-0000-000b-0000-005200000036"))), smConvRoleName = (fromJust (parseRoleName "6k2nmz6p7wwzsfog5ou2ab_c6g8qtjp0ts4l8so6kp2jxbgcdpj2m8miecli0xs878tn89efefhiaplvrsopyv"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000062-0000-004f-0000-007000000008"))), smConvRoleName = (fromJust (parseRoleName "fa90ud9jxchavi7rh86j_asnx76sl37icebx3a6e7mvh4e6qs72r5wz5jvl7mt2hholjvb1i7sb"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000006a-0000-005d-0000-006300000044"))), smConvRoleName = (fromJust (parseRoleName "a7soodaocsqyy1kd9pvvmqwuq4"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000001-0000-0059-0000-001700000076"))), smConvRoleName = (fromJust (parseRoleName "s77jt7zyvk2ak1odl2zpfnn978xf_cnek2j4dkbqwcbux4_gl6w8d12alff8_m3m08tmznh_ddpcgn9gz5r9cc3n7vumbef6acwkff2ic0c"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000022-0000-001a-0000-003d0000002f"))), smConvRoleName = (fromJust (parseRoleName "6lbre9rk12gjuobvvqazkl7vrh816ehtjhh8jjvi6sdngob28ea35kgytripgixk9x7350uj5_c7eqz5clg2h0ct9rffki7mfme2tlnvm841w"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000059-0000-0079-0000-002b00000035"))), smConvRoleName = (fromJust (parseRoleName "nv7v90z28mqecrjy96k3x1qzduvkflj0w1wpxg"))}]}))))}
testObject_RemoveBotResponse_user_4 :: RemoveBotResponse
testObject_RemoveBotResponse_user_4 = RemoveBotResponse {rsRemoveBotEvent = (Event (Typing) ((Id (fromJust (UUID.fromString "00000db9-0000-06c3-0000-3e7700002e55")))) ((Id (fromJust (UUID.fromString "00005861-0000-0c2e-0000-224000000c19")))) (read "1864-04-17 08:23:56.905 UTC") (Just (EdTyping (TypingData {tdStatus = StartedTyping}))))}
testObject_RemoveBotResponse_user_5 :: RemoveBotResponse
testObject_RemoveBotResponse_user_5 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvDelete) ((Id (fromJust (UUID.fromString "00003fc3-0000-484b-0000-41a500005e9c")))) ((Id (fromJust (UUID.fromString "000073fc-0000-4e5d-0000-0bc9000068c4")))) (read "1864-05-16 15:54:41.772 UTC") (Nothing))}
testObject_RemoveBotResponse_user_6 :: RemoveBotResponse
testObject_RemoveBotResponse_user_6 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvDelete) ((Id (fromJust (UUID.fromString "00003fbd-0000-2503-0000-76c900005d1e")))) ((Id (fromJust (UUID.fromString "000049f4-0000-771e-0000-0be7000036fd")))) (read "1864-05-26 14:03:03.885 UTC") (Nothing))}
testObject_RemoveBotResponse_user_7 :: RemoveBotResponse
testObject_RemoveBotResponse_user_7 = RemoveBotResponse {rsRemoveBotEvent = (Event (MemberJoin) ((Id (fromJust (UUID.fromString "00000cb5-0000-7bfc-0000-22ff00006a32")))) ((Id (fromJust (UUID.fromString "00004159-0000-5dae-0000-3f9700003d20")))) (read "1864-05-20 22:24:36.346 UTC") (Just (EdMembersJoin (SimpleMembers {mMembers = [SimpleMember {smId = (Id (fromJust (UUID.fromString "00000011-0000-0008-0000-000c0000001f"))), smConvRoleName = (fromJust (parseRoleName "7kab6izo68rdglbpkcl_djpje5neb4zzac4j_qn_cgv3w9mfzs_"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000028-0000-000c-0000-004800000006"))), smConvRoleName = (fromJust (parseRoleName "criiivwdfevfqaiowh9e9qi22lat1nqymqtv56qiaf4kd394hpg25e8tjp_azgozuprk9gjcb0kcje49ghx53zcxp2mewj3lkt_9_33"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000022-0000-0022-0000-006600000063"))), smConvRoleName = (fromJust (parseRoleName "1siteb1gs"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000018-0000-0067-0000-004a00000004"))), smConvRoleName = (fromJust (parseRoleName "xv5zyrq06cd5au_e1g19189w9gnndht6zmx2arw7h0w5fcsglk5i7uq_le1y31im1_enaiw0injtwjsaa5wov9o"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000074-0000-0068-0000-003c0000001e"))), smConvRoleName = (fromJust (parseRoleName "tpn_0aema5oc74pkrx7nm91e5bc"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000008-0000-0035-0000-002e00000069"))), smConvRoleName = (fromJust (parseRoleName "c5v9jtw3xl7cu0e6twdb"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000045-0000-0042-0000-006300000058"))), smConvRoleName = (fromJust (parseRoleName "pdxf01p0sa4oyx4w544_9h2t46j16m0dqpv5h9t1acnmxr3k9kbeck7o3hquh472z0axly1giphq61wzb6degf_8s"))}]}))))}
testObject_RemoveBotResponse_user_8 :: RemoveBotResponse
testObject_RemoveBotResponse_user_8 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvRename) ((Id (fromJust (UUID.fromString "000040cc-0000-078e-0000-45af000029ee")))) ((Id (fromJust (UUID.fromString "00005c23-0000-7820-0000-57fa00005ce7")))) (read "1864-05-21 18:45:57.888 UTC") (Just (EdConvRename (ConversationRename {cupName = "\SOH\SOHlp\"N\1066816%;X&l\1079213>\SI'8\SUB\1057682\RS+`uYM(}\10917\f"}))))}
testObject_RemoveBotResponse_user_9 :: RemoveBotResponse
testObject_RemoveBotResponse_user_9 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvConnect) ((Id (fromJust (UUID.fromString "0000498b-0000-4dbf-0000-08e800007cbe")))) ((Id (fromJust (UUID.fromString "00000c87-0000-68b5-0000-3f8d000069a0")))) (read "1864-04-11 03:49:10.673 UTC") (Just (EdConnect (Connect {cRecipient = (Id (fromJust (UUID.fromString "00000007-0000-0005-0000-000500000007"))), cMessage = Just "I", cName = Nothing, cEmail = Just "t\DLEt\DC2\162091"}))))}
testObject_RemoveBotResponse_user_10 :: RemoveBotResponse
testObject_RemoveBotResponse_user_10 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvReceiptModeUpdate) ((Id (fromJust (UUID.fromString "0000710c-0000-2ea8-0000-6c20000045b0")))) ((Id (fromJust (UUID.fromString "00002042-0000-32e1-0000-547700006958")))) (read "1864-04-19 04:24:03.842 UTC") (Just (EdConvReceiptModeUpdate (ConversationReceiptModeUpdate {cruReceiptMode = ReceiptMode {unReceiptMode = 4388}}))))}
testObject_RemoveBotResponse_user_11 :: RemoveBotResponse
testObject_RemoveBotResponse_user_11 = RemoveBotResponse {rsRemoveBotEvent = (Event (MemberLeave) ((Id (fromJust (UUID.fromString "000012bc-0000-1dcd-0000-3ed500007033")))) ((Id (fromJust (UUID.fromString "0000720c-0000-0f0d-0000-7d0600000bfa")))) (read "1864-05-19 15:48:29.981 UTC") (Just (EdMembersLeave (UserIdList {mUsers = [(Id (fromJust (UUID.fromString "00000219-0000-3ed0-0000-5f3900003522"))),(Id (fromJust (UUID.fromString "00007be5-0000-01bf-0000-555200002520"))),(Id (fromJust (UUID.fromString "00004fe2-0000-1088-0000-2f7900002ede"))),(Id (fromJust (UUID.fromString "00006df6-0000-6941-0000-2cc200007ef0"))),(Id (fromJust (UUID.fromString "00001ab5-0000-40ed-0000-734300000b6a"))),(Id (fromJust (UUID.fromString "00007544-0000-1993-0000-5b59000072e2"))),(Id (fromJust (UUID.fromString "00004811-0000-3363-0000-02bb00001e1d"))),(Id (fromJust (UUID.fromString "00004b85-0000-0e36-0000-237d00004116"))),(Id (fromJust (UUID.fromString "00004178-0000-531c-0000-41820000356a"))),(Id (fromJust (UUID.fromString "00007b18-0000-24ec-0000-2a8b00006c2d"))),(Id (fromJust (UUID.fromString "00000fd0-0000-6a9e-0000-4e3d0000543a")))]}))))}
testObject_RemoveBotResponse_user_12 :: RemoveBotResponse
testObject_RemoveBotResponse_user_12 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvRename) ((Id (fromJust (UUID.fromString "00007f8b-0000-45af-0000-72d400004f06")))) ((Id (fromJust (UUID.fromString "00001612-0000-6db7-0000-4cbe00004394")))) (read "1864-04-13 09:27:09.672 UTC") (Just (EdConvRename (ConversationRename {cupName = "\t\1077869-9\132294?\1103841\1932\t\ESC\GS;\996541\156168\SI\1068548}\DEL\DLEffZ"}))))}
testObject_RemoveBotResponse_user_13 :: RemoveBotResponse
testObject_RemoveBotResponse_user_13 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvCreate) ((Id (fromJust (UUID.fromString "00007ddf-0000-403f-0000-08e8000049ce")))) ((Id (fromJust (UUID.fromString "000064c8-0000-59dc-0000-0cc100007516")))) (read "1864-05-09 09:37:14.042 UTC") (Just (EdConversation (Conversation {cnvId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000000"))), cnvType = RegularConv, cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000002"))), cnvAccess = [CodeAccess,PrivateAccess,PrivateAccess], cnvAccessRole = TeamAccessRole, cnvName = Nothing, cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))}), memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), memOtrMutedRef = Just "", memOtrArchived = False, memOtrArchivedRef = Just "", memHidden = True, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "f7l57cavxvv9tniv7ulihb31nqecb1j0msjxongsn4voirvos1rr9jtfn9fwfu29da4r3yy"))}, cmOthers = [OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))}), omConvRoleName = (fromJust (parseRoleName "zd1ugsfr6j3ijdm3s00g474u0ubnc6_44evd931y3tkburp89y14q6946r4fc4tg_e0k2fsm8"))}]}, cnvTeam = Nothing, cnvMessageTimer = Just (Ms {ms = 1225755019461060}), cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 2})}))))}
testObject_RemoveBotResponse_user_14 :: RemoveBotResponse
testObject_RemoveBotResponse_user_14 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvConnect) ((Id (fromJust (UUID.fromString "000036e5-0000-516a-0000-75ba00001355")))) ((Id (fromJust (UUID.fromString "00006018-0000-7860-0000-4ea000000f9b")))) (read "1864-06-05 20:48:54.213 UTC") (Just (EdConnect (Connect {cRecipient = (Id (fromJust (UUID.fromString "00000005-0000-0005-0000-000000000001"))), cMessage = Nothing, cName = Just "/\ETB\n\41691\&2\126243\150209", cEmail = Just ""}))))}
testObject_RemoveBotResponse_user_15 :: RemoveBotResponse
testObject_RemoveBotResponse_user_15 = RemoveBotResponse {rsRemoveBotEvent = (Event (MemberLeave) ((Id (fromJust (UUID.fromString "00007abe-0000-24d7-0000-1ce7000067bf")))) ((Id (fromJust (UUID.fromString "00007e25-0000-208d-0000-15ee00000cd9")))) (read "1864-05-24 23:35:45.391 UTC") (Just (EdMembersLeave (UserIdList {mUsers = [(Id (fromJust (UUID.fromString "00004f91-0000-00d1-0000-0bb300000042"))),(Id (fromJust (UUID.fromString "00004f47-0000-3328-0000-7a6a00004ac5"))),(Id (fromJust (UUID.fromString "00002f07-0000-36f4-0000-663a0000624f"))),(Id (fromJust (UUID.fromString "00002281-0000-7883-0000-3d7a00007ad6")))]}))))}
testObject_RemoveBotResponse_user_16 :: RemoveBotResponse
testObject_RemoveBotResponse_user_16 = RemoveBotResponse {rsRemoveBotEvent = (Event (OtrMessageAdd) ((Id (fromJust (UUID.fromString "00006759-0000-7ff5-0000-47c300000dca")))) ((Id (fromJust (UUID.fromString "00005042-0000-6dfe-0000-6d6400000448")))) (read "1864-04-11 00:53:02.183 UTC") (Just (EdOtrMessage (OtrMessage {otrSender = ClientId {client = "3"}, otrRecipient = ClientId {client = "7"}, otrCiphertext = "J", otrData = Nothing}))))}
testObject_RemoveBotResponse_user_17 :: RemoveBotResponse
testObject_RemoveBotResponse_user_17 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvAccessUpdate) ((Id (fromJust (UUID.fromString "00005f87-0000-3f0f-0000-06c000006b2a")))) ((Id (fromJust (UUID.fromString "00001308-0000-2f60-0000-369000003f43")))) (read "1864-05-10 07:03:48.881 UTC") (Just (EdConvAccessUpdate (ConversationAccessUpdate {cupAccess = [InviteAccess,PrivateAccess,InviteAccess,PrivateAccess], cupAccessRole = ActivatedAccessRole}))))}
testObject_RemoveBotResponse_user_18 :: RemoveBotResponse
testObject_RemoveBotResponse_user_18 = RemoveBotResponse {rsRemoveBotEvent = (Event (MemberLeave) ((Id (fromJust (UUID.fromString "0000545d-0000-73cf-0000-04e50000239b")))) ((Id (fromJust (UUID.fromString "000032cc-0000-2f43-0000-078f000011de")))) (read "1864-05-13 06:34:15.767 UTC") (Just (EdMembersLeave (UserIdList {mUsers = [(Id (fromJust (UUID.fromString "000073a4-0000-1f33-0000-03dd000032c8"))),(Id (fromJust (UUID.fromString "00007551-0000-6a14-0000-55c900005d62"))),(Id (fromJust (UUID.fromString "000068cf-0000-1192-0000-0f050000150b"))),(Id (fromJust (UUID.fromString "000027d4-0000-740c-0000-3de700001373"))),(Id (fromJust (UUID.fromString "0000364d-0000-3fad-0000-0dd600003cf6"))),(Id (fromJust (UUID.fromString "000069dd-0000-2ae7-0000-7cb900002f72"))),(Id (fromJust (UUID.fromString "000074db-0000-1944-0000-60d900004099"))),(Id (fromJust (UUID.fromString "0000751c-0000-4d4a-0000-36950000622d"))),(Id (fromJust (UUID.fromString "00001fea-0000-3a77-0000-5aa500001f0a"))),(Id (fromJust (UUID.fromString "000029fb-0000-0209-0000-237900003970"))),(Id (fromJust (UUID.fromString "00006a7c-0000-48d6-0000-4874000003d6"))),(Id (fromJust (UUID.fromString "00005ba0-0000-3b47-0000-000200006c92"))),(Id (fromJust (UUID.fromString "00005d5d-0000-24d2-0000-1f6b00001276"))),(Id (fromJust (UUID.fromString "00002960-0000-0cb7-0000-475700002af8")))]}))))}
testObject_RemoveBotResponse_user_19 :: RemoveBotResponse
testObject_RemoveBotResponse_user_19 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvConnect) ((Id (fromJust (UUID.fromString "00004945-0000-1121-0000-01610000316a")))) ((Id (fromJust (UUID.fromString "00000b9e-0000-7d63-0000-469700002434")))) (read "1864-06-02 18:38:44.295 UTC") (Just (EdConnect (Connect {cRecipient = (Id (fromJust (UUID.fromString "00000008-0000-0001-0000-000200000001"))), cMessage = Just "dr\SOf", cName = Just "m", cEmail = Just "G;"}))))}
testObject_RemoveBotResponse_user_20 :: RemoveBotResponse
testObject_RemoveBotResponse_user_20 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvConnect) ((Id (fromJust (UUID.fromString "000069f7-0000-6f7c-0000-299800004714")))) ((Id (fromJust (UUID.fromString "00006a00-0000-776e-0000-265c00000aab")))) (read "1864-05-26 18:04:43.871 UTC") (Just (EdConnect (Connect {cRecipient = (Id (fromJust (UUID.fromString "00000008-0000-0008-0000-000600000004"))), cMessage = Just ".\15320\RS\ESC", cName = Just "", cEmail = Just ""}))))}
