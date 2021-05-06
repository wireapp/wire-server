{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Event_user where

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
testObject_Event_user_1 :: Event
testObject_Event_user_1 = (Event (MemberLeave) ((Id (fromJust (UUID.fromString "00005af1-0000-32f4-0000-50fc0000556e")))) ((Id (fromJust (UUID.fromString "00001771-0000-6f63-0000-4c6a00003d62")))) (read "1864-05-18 18:05:58.236 UTC") (Just (EdMembersLeave (UserIdList {mUsers = [(Id (fromJust (UUID.fromString "00006dd9-0000-3aa3-0000-6a7b00007e5c"))),(Id (fromJust (UUID.fromString "00004feb-0000-32ee-0000-4c8a00002081"))),(Id (fromJust (UUID.fromString "00006c32-0000-5d4d-0000-5b2b00002b4d"))),(Id (fromJust (UUID.fromString "00007075-0000-18a6-0000-5c2300006ee6"))),(Id (fromJust (UUID.fromString "000012a9-0000-7e38-0000-1bad00004e76"))),(Id (fromJust (UUID.fromString "000021b4-0000-25a1-0000-4a7400001603"))),(Id (fromJust (UUID.fromString "000057dd-0000-40b3-0000-218b000022da"))),(Id (fromJust (UUID.fromString "0000660b-0000-7aa5-0000-4eb200006830"))),(Id (fromJust (UUID.fromString "000007c0-0000-0087-0000-02ea000063e5"))),(Id (fromJust (UUID.fromString "00006a0a-0000-2b5b-0000-02bf0000541f"))),(Id (fromJust (UUID.fromString "0000401f-0000-4724-0000-5fe400001548"))),(Id (fromJust (UUID.fromString "00004c17-0000-023d-0000-286c00000df8"))),(Id (fromJust (UUID.fromString "00006067-0000-31b9-0000-12210000662e"))),(Id (fromJust (UUID.fromString "00001423-0000-4682-0000-170900005608"))),(Id (fromJust (UUID.fromString "00000130-0000-4a02-0000-37b900007e6b"))),(Id (fromJust (UUID.fromString "00004747-0000-4b74-0000-41da000063bf"))),(Id (fromJust (UUID.fromString "00004be5-0000-6526-0000-16a600004eef"))),(Id (fromJust (UUID.fromString "0000486a-0000-6e49-0000-3f5700001521"))),(Id (fromJust (UUID.fromString "000021a4-0000-4aa3-0000-35b200002193"))),(Id (fromJust (UUID.fromString "0000056f-0000-487c-0000-0fbe0000530f"))),(Id (fromJust (UUID.fromString "00002ef3-0000-3572-0000-00f600007021"))),(Id (fromJust (UUID.fromString "00005876-0000-7c7e-0000-106d0000751c"))),(Id (fromJust (UUID.fromString "000016c6-0000-7842-0000-4f4f00003572"))),(Id (fromJust (UUID.fromString "000039e9-0000-7391-0000-005900000da0"))),(Id (fromJust (UUID.fromString "00005e91-0000-2cc9-0000-6c490000652c")))]}))))
testObject_Event_user_2 :: Event
testObject_Event_user_2 = (Event (Typing) ((Id (fromJust (UUID.fromString "00004adb-0000-0859-0000-146800006927")))) ((Id (fromJust (UUID.fromString "000066ff-0000-0b72-0000-25a200001979")))) (read "1864-06-07 01:13:23.541 UTC") (Just (EdTyping (TypingData {tdStatus = StartedTyping}))))
testObject_Event_user_3 :: Event
testObject_Event_user_3 = (Event (ConvCodeUpdate) ((Id (fromJust (UUID.fromString "0000239e-0000-37c1-0000-0fe6000019be")))) ((Id (fromJust (UUID.fromString "0000153b-0000-0ac4-0000-344a00007915")))) (read "1864-04-20 12:06:42.302 UTC") (Just (EdConvCodeUpdate (ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("6A30bMPxvwcmfjbrQGhb")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("Mfwu_dlKLVNY321y3")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}))))
testObject_Event_user_4 :: Event
testObject_Event_user_4 = (Event (ConvMessageTimerUpdate) ((Id (fromJust (UUID.fromString "00006d4e-0000-7b6a-0000-303300005641")))) ((Id (fromJust (UUID.fromString "00002e9d-0000-05df-0000-75ac000075c8")))) (read "1864-05-29 09:38:33.578 UTC") (Just (EdConvMessageTimerUpdate (ConversationMessageTimerUpdate {cupMessageTimer = Just (Ms {ms = 4697132917772223})}))))
testObject_Event_user_5 :: Event
testObject_Event_user_5 = (Event (ConvAccessUpdate) ((Id (fromJust (UUID.fromString "00001171-0000-5612-0000-72e600006f17")))) ((Id (fromJust (UUID.fromString "000061c1-0000-362b-0000-3dc30000384b")))) (read "1864-05-20 18:50:01.388 UTC") (Just (EdConvAccessUpdate (ConversationAccessUpdate {cupAccess = [PrivateAccess,CodeAccess,PrivateAccess,LinkAccess], cupAccessRole = PrivateAccessRole}))))
testObject_Event_user_6 :: Event
testObject_Event_user_6 = (Event (ConvCodeUpdate) ((Id (fromJust (UUID.fromString "00003e76-0000-6fc5-0000-44d000006635")))) ((Id (fromJust (UUID.fromString "0000085c-0000-2763-0000-3f0a00006e2e")))) (read "1864-04-19 15:37:11.341 UTC") (Just (EdConvCodeUpdate (ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("1ofPkZoqPQ9-fim09ibP")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("=0_nAy")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}))))
testObject_Event_user_7 :: Event
testObject_Event_user_7 = (Event (OtrMessageAdd) ((Id (fromJust (UUID.fromString "00005364-0000-1d59-0000-362900006ce7")))) ((Id (fromJust (UUID.fromString "0000798e-0000-1b65-0000-49a500002e5b")))) (read "1864-05-27 18:31:00.214 UTC") (Just (EdOtrMessage (OtrMessage {otrSender = ClientId {client = "12"}, otrRecipient = ClientId {client = "2"}, otrCiphertext = "\RS\1058774", otrData = Just "\SI\166371-\7639\SOH"}))))
testObject_Event_user_8 :: Event
testObject_Event_user_8 = (Event (MemberJoin) ((Id (fromJust (UUID.fromString "00007565-0000-645d-0000-4cf800006004")))) ((Id (fromJust (UUID.fromString "00003c11-0000-24d7-0000-12ec0000381f")))) (read "1864-04-18 13:06:25.523 UTC") (Just (EdMembersJoin (SimpleMembers {mMembers = [SimpleMember {smId = (Id (fromJust (UUID.fromString "00000039-0000-006c-0000-005d00000051"))), smConvRoleName = (fromJust (parseRoleName "1kp_nebsqboxeefnflqllwmwah86x09jpgx960s34l4pihvdycx9hpfzod2otpv1m8fo3algi4cz_z9h5vougaf5ypiq6_llfl2xzbkz4_g6ukn0"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000001e-0000-0005-0000-007e00000026"))), smConvRoleName = (fromJust (parseRoleName "7jw"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000059-0000-006d-0000-00560000005a"))), smConvRoleName = (fromJust (parseRoleName "42tf_spkkep0sdpa2qs86_mo9xtohe2xeuo9vfmvohndxs3adwwgws3cfn4yzcuxit6ply1uosqnck3x2atun8n75p3euv0"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000000f-0000-0079-0000-00390000003b"))), smConvRoleName = (fromJust (parseRoleName "1y88v7_yb_w014f3ud3_29rvg64to48om7km4iuw2d1jcg3dgkfxty0trv9kl0q_up9w50thogdil"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000008-0000-000c-0000-007400000034"))), smConvRoleName = (fromJust (parseRoleName "wvnib0x7cs_l8zy4sc8iqqduy_k3d7unijf5btig99sct3_ect_qudt881u6z65dxdgkc8jb0uyj_uu_j6mvhtss4ayr68982"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000006f-0000-0045-0000-001c00000016"))), smConvRoleName = (fromJust (parseRoleName "gpzfua91doin8v2kauy3jb96mswydynimmjf0qhp140yub4cghvrq39inmcptwa9ua09sqdc7n6hfx1oib_q2j43i_tng3lvwtyqtndegicmy1hue8xi9hmw"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000067-0000-0077-0000-004900000000"))), smConvRoleName = (fromJust (parseRoleName "ai0qhndf0aatzvanemr8ultvee1bravw2xiur45m6cu19k1j6owma9ms4ggh51sxr6hy8eebtyx5vq4x6swmt1nyo3g1hrm5q8fsl_g_icj"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000001e-0000-0029-0000-004000000037"))), smConvRoleName = (fromJust (parseRoleName "9zvj5c5"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000054-0000-006c-0000-000e00000031"))), smConvRoleName = (fromJust (parseRoleName "mbuitap6c8w60zekphe6_9nljny7xp5qfsje1f6dak65acyxm0j7fhar17bfqwz_lmyctc0u8zgkuvt8dhonm1p0nm03wo07ibrfohejtixqcjj2o1m2i"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000047-0000-0065-0000-001d0000006a"))), smConvRoleName = (fromJust (parseRoleName "et34we1qx0dd33qqchddiflpa8nqg0upcwklo_yh02b6ffj__2"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000021-0000-0025-0000-000a0000000f"))), smConvRoleName = (fromJust (parseRoleName "ciqedp9qs1k6u98i5i7oe0mnjap4f37c4tcn0ll7v4jlyblafiwqrrud4evrc29p86elkqrs5ghlyo7130untu7fx22_ioxs7ngvlwgccqz_jhddgmm65qcza"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000016-0000-0068-0000-002e00000021"))), smConvRoleName = (fromJust (parseRoleName "03jcnnyjfq6oz2x652s7zpv41jrndg82y1redqlampnct32q09uxwvgdvr_p9dgzllxao30inuboyjtfiwv67tarc_1q_l1x3xb16yjmupzcqj"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000016-0000-004f-0000-00120000004b"))), smConvRoleName = (fromJust (parseRoleName "op6vmk2l82_8tumt_mlvsuesykbondmdo7gaq864_uw2rygapq_tomjmy58or_l3q445gfs71zx7kd7xpnyy88wpclmxl8yu"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000016-0000-0078-0000-00150000007b"))), smConvRoleName = (fromJust (parseRoleName "6s37hpj96j9nryw8lzmpevls2tvxppdw75hy7gu2ftoy8ekj2ajt8h7u_8em"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000015-0000-0067-0000-001b00000039"))), smConvRoleName = (fromJust (parseRoleName "pskdlx7_yzghxw5m_2uglz0slavxhjpbjiw2znw1v4xm3z2snas6j6_g5zliio89a1vzj2_efvzr_kuxhj1jt0qugjut86t4hzthho70_3rjd72yhp5"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000048-0000-0044-0000-007600000068"))), smConvRoleName = (fromJust (parseRoleName "fw_pjkobnrt2vzcze_61ha8d3c0fr6kwpplyhcgx7gxfxpno353zolinfynlaobfshtbwdfaxskz3peyyxnl52zwyhwsi0o1g"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000068-0000-0009-0000-007900000070"))), smConvRoleName = (fromJust (parseRoleName "vt4_x24pfr_90vh5r2pdornha7uz4u8lyse_ztk2c6bbgxxrt"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000071-0000-006e-0000-00640000003e"))), smConvRoleName = (fromJust (parseRoleName "mrwgheu6sf4k8ind0l4ekm6v8nbmhsuxn8iugl9ce1a9n0rae3l328np_hl4"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000027-0000-0037-0000-004d0000001c"))), smConvRoleName = (fromJust (parseRoleName "nl8yqcrwpds__tmlggcbjec1wabhoipk4kn0_3x6660rgddhp9oidgnkvzcrhy8fb04o2wehed3gjn"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000001d-0000-001d-0000-002d0000002c"))), smConvRoleName = (fromJust (parseRoleName "qaifyyv9n2cbt0aiffi9ki2_q17up1br8m4s1at26vjeuvltbr7qjth7nu0pbcd78w6ve128bkl08fciashhmdcf94ecy5_m3mmscvp2l"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000001-0000-006a-0000-007100000017"))), smConvRoleName = (fromJust (parseRoleName "93kx6c8djw_fk25bak_sbj96ytq8i_7c1nx7r2fyp39ajenbc7gs59bsohknptqnu2c9bk"))}]}))))
testObject_Event_user_9 :: Event
testObject_Event_user_9 = (Event (Typing) ((Id (fromJust (UUID.fromString "0000449e-0000-70c6-0000-0db500005806")))) ((Id (fromJust (UUID.fromString "00002290-0000-2b2e-0000-28ec0000767b")))) (read "1864-04-20 07:54:45.109 UTC") (Just (EdTyping (TypingData {tdStatus = StartedTyping}))))
testObject_Event_user_10 :: Event
testObject_Event_user_10 = (Event (ConvCodeUpdate) ((Id (fromJust (UUID.fromString "000000fe-0000-3c21-0000-190b00005aeb")))) ((Id (fromJust (UUID.fromString "000027de-0000-4893-0000-611500003379")))) (read "1864-05-27 14:10:44.281 UTC") (Just (EdConvCodeUpdate (ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("Zycb_ImwPWHJ_=vh-QCP")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("_gisBUUTHbG")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}))))
testObject_Event_user_11 :: Event
testObject_Event_user_11 = (Event (ConvRename) ((Id (fromJust (UUID.fromString "0000073b-0000-33dc-0000-598d00002682")))) ((Id (fromJust (UUID.fromString "00001b9d-0000-748a-0000-7c3d00005d48")))) (read "1864-04-20 19:35:27.094 UTC") (Just (EdConvRename (ConversationRename {cupName = "\12503d\GS4/\9843%!\1000011(\1062551z\133315\a_J"}))))
testObject_Event_user_12 :: Event
testObject_Event_user_12 = (Event (ConvRename) ((Id (fromJust (UUID.fromString "00002cf9-0000-39d4-0000-1b9300001c45")))) ((Id (fromJust (UUID.fromString "00007684-0000-58ab-0000-163c00003b20")))) (read "1864-04-19 12:10:06.722 UTC") (Just (EdConvRename (ConversationRename {cupName = "\SOB \ETX\n\181512\11387C\SYN\119016A"}))))
testObject_Event_user_13 :: Event
testObject_Event_user_13 = (Event (ConvMessageTimerUpdate) ((Id (fromJust (UUID.fromString "00002a02-0000-7363-0000-3e7100005f95")))) ((Id (fromJust (UUID.fromString "0000340a-0000-0e3c-0000-584000006e1f")))) (read "1864-04-14 07:17:33.451 UTC") (Just (EdConvMessageTimerUpdate (ConversationMessageTimerUpdate {cupMessageTimer = Nothing}))))
testObject_Event_user_14 :: Event
testObject_Event_user_14 = (Event (Typing) ((Id (fromJust (UUID.fromString "00004a5b-0000-7cc8-0000-694000000a53")))) ((Id (fromJust (UUID.fromString "00007af2-0000-0041-0000-46de00000d03")))) (read "1864-05-11 07:50:47.199 UTC") (Just (EdTyping (TypingData {tdStatus = StoppedTyping}))))
testObject_Event_user_15 :: Event
testObject_Event_user_15 = (Event (OtrMessageAdd) ((Id (fromJust (UUID.fromString "00000265-0000-7f49-0000-340700003173")))) ((Id (fromJust (UUID.fromString "00007606-0000-3fad-0000-2420000037ad")))) (read "1864-04-22 06:59:04.829 UTC") (Just (EdOtrMessage (OtrMessage {otrSender = ClientId {client = "20"}, otrRecipient = ClientId {client = "5"}, otrCiphertext = "", otrData = Just "U|{\1059185\1082558"}))))
testObject_Event_user_16 :: Event
testObject_Event_user_16 = (Event (ConvReceiptModeUpdate) ((Id (fromJust (UUID.fromString "00006b2e-0000-1919-0000-2a7c00004385")))) ((Id (fromJust (UUID.fromString "00003da3-0000-71a7-0000-08fe0000490d")))) (read "1864-05-20 02:58:23.372 UTC") (Just (EdConvReceiptModeUpdate (ConversationReceiptModeUpdate {cruReceiptMode = ReceiptMode {unReceiptMode = -15851}}))))
testObject_Event_user_17 :: Event
testObject_Event_user_17 = (Event (MemberStateUpdate) ((Id (fromJust (UUID.fromString "00002a35-0000-3c0d-0000-7be400000e46")))) ((Id (fromJust (UUID.fromString "00000231-0000-4eb1-0000-31a200005dc8")))) (read "1864-06-07 04:08:06.589 UTC") (Just (EdMemberUpdate (MemberUpdateData {misTarget = Nothing, misOtrMuted = Just True, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -2}), misOtrMutedRef = Just "M2", misOtrArchived = Just False, misOtrArchivedRef = Just "\1109102s", misHidden = Nothing, misHiddenRef = Nothing, misConvRoleName = Just (fromJust (parseRoleName "l_ves4gebz3jf1j9kn8z9h879w2wpk6xkwmtz62fxmxin6h1hj0vhawk5ijhztopv95x09f_l1txdvqv9wzdbi81l9t0db8kt0zqv21wwj0oela"))}))))
testObject_Event_user_18 :: Event
testObject_Event_user_18 = (Event (ConvRename) ((Id (fromJust (UUID.fromString "000032e4-0000-1c8c-0000-3e6100001408")))) ((Id (fromJust (UUID.fromString "000016ee-0000-7d11-0000-517400001a5e")))) (read "1864-05-11 09:43:54.15 UTC") (Just (EdConvRename (ConversationRename {cupName = "\1037264\r\\k\1091160\SI\1058115\DLE\bJ\EOT\1042716(;Y\EOT\ENQ\1028591w\1027134\27636\a\DC1"}))))
testObject_Event_user_19 :: Event
testObject_Event_user_19 = (Event (ConvCreate) ((Id (fromJust (UUID.fromString "00003716-0000-4ef8-0000-4728000070d9")))) ((Id (fromJust (UUID.fromString "00002469-0000-2276-0000-5f1f00004479")))) (read "1864-05-02 08:39:16.696 UTC") (Just (EdConversation (Conversation {cnvId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000001"))), cnvType = One2OneConv, cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), cnvAccess = [InviteAccess], cnvAccessRole = TeamAccessRole, cnvName = Just "\178846\1087711\1086037", cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))}), memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), memOtrMutedRef = Just "", memOtrArchived = False, memOtrArchivedRef = Just "", memHidden = False, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "5wflfnfvg6i7u17c6es1yqzyr0vmrg7u6n2e8innu6wnprwy2n_3wenr3ny18f1udbkd6ky"))}, cmOthers = [OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))}), omConvRoleName = (fromJust (parseRoleName "wemkj44vqb17s3ame8b2823w4m70ncysbn_8xibxa6rlv8v_80gb"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))}), omConvRoleName = (fromJust (parseRoleName "4i3hgshbnsl7u_5w9lmhliovlu9epe2zipi3r4i494qclroyp4ydv_il5av8qij006y_3m0ih28ycfy84kvk74w2kle3u409yfpu7"))}]}, cnvTeam = Nothing, cnvMessageTimer = Nothing, cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 2})}))))
testObject_Event_user_20 :: Event
testObject_Event_user_20 = (Event (ConvAccessUpdate) ((Id (fromJust (UUID.fromString "00004e92-0000-0b1a-0000-7d6300003cc0")))) ((Id (fromJust (UUID.fromString "000065e0-0000-20ac-0000-63c4000023b0")))) (read "1864-04-19 21:46:22.569 UTC") (Just (EdConvAccessUpdate (ConversationAccessUpdate {cupAccess = [InviteAccess,CodeAccess,CodeAccess,InviteAccess], cupAccessRole = TeamAccessRole}))))
