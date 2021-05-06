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
testObject_RemoveBotResponse_user_1 = RemoveBotResponse {rsRemoveBotEvent = (Event (MemberStateUpdate) ((Id (fromJust (UUID.fromString "0000441a-0000-122e-0000-43780000773c")))) ((Id (fromJust (UUID.fromString "000018aa-0000-7b77-0000-59aa00005ca5")))) (read "1864-05-27 07:30:39.141 UTC") (Just (EdMemberUpdate (MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000001"))), misOtrMuted = Just True, misOtrMutedStatus = Nothing, misOtrMutedRef = Just "%\1065699", misOtrArchived = Just False, misOtrArchivedRef = Just "h?c", misHidden = Just False, misHiddenRef = Nothing, misConvRoleName = Nothing}))))}
testObject_RemoveBotResponse_user_2 :: RemoveBotResponse
testObject_RemoveBotResponse_user_2 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvCodeDelete) ((Id (fromJust (UUID.fromString "00002b32-0000-44bf-0000-50f4000020fb")))) ((Id (fromJust (UUID.fromString "00005873-0000-0c29-0000-75db00000d69")))) (read "1864-05-29 13:07:31.93 UTC") (Nothing))}
testObject_RemoveBotResponse_user_3 :: RemoveBotResponse
testObject_RemoveBotResponse_user_3 = RemoveBotResponse {rsRemoveBotEvent = (Event (MemberJoin) ((Id (fromJust (UUID.fromString "00002bec-0000-5e52-0000-6c13000059ac")))) ((Id (fromJust (UUID.fromString "00001438-0000-394d-0000-66ee00006187")))) (read "1864-05-27 18:31:32.074 UTC") (Just (EdMembersJoin (SimpleMembers {mMembers = [SimpleMember {smId = (Id (fromJust (UUID.fromString "0000006a-0000-0073-0000-000300000056"))), smConvRoleName = (fromJust (parseRoleName "5io95403o2e30hb85h16a96"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000001c-0000-0046-0000-004f00000072"))), smConvRoleName = (fromJust (parseRoleName "b0afazmopyif_8i0tm4t1dd_hn9h1rcm1kk0xc"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000030-0000-000e-0000-006a00000050"))), smConvRoleName = (fromJust (parseRoleName "txz_3b_ooyeq87yk6shybl3zbgdeavqs2_tauf_gl_eay7kv747nw0ra0ycromhqn_4cpabre9kzomjj0dowqdno7mhmwpdcpd_0bmnblsht_aomew0bxg2hrjt"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000073-0000-0037-0000-000700000007"))), smConvRoleName = (fromJust (parseRoleName "f9vo_cfdwli9shfffc4z7awylv4lf0hmnw3m3sfpx0iozfy66rpzqugwopzpt"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000069-0000-004a-0000-003100000066"))), smConvRoleName = (fromJust (parseRoleName "ibt_vt540jl6v9gv7ftcisvdsppsm_df1waj0t7o8fp5cx4kkwb84fwrs6ap"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000000-0000-0054-0000-004d00000054"))), smConvRoleName = (fromJust (parseRoleName "pawm9dzb28sm2sf8kx92hm4ms7qe29j1t1fga3khet1wzt1jx7kpxjzenknezqqqk6jqrais"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000047-0000-0060-0000-00370000002b"))), smConvRoleName = (fromJust (parseRoleName "4xvzirwuf3y6brnzg4bpu7bbotxom8iwlmnv37j7g_jicx_3m"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000002b-0000-0012-0000-006b00000019"))), smConvRoleName = (fromJust (parseRoleName "2waxjv2l3jnk65l5fd37g03bo6w0__swn43sifwf8jbt6wmlf4_g_ngj_pcqeh7c5wnmofjdra2cje1brhz7fwsgcl"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000010-0000-001f-0000-004800000054"))), smConvRoleName = (fromJust (parseRoleName "tnc4pd8vle0q19nhb0hp8j5phluvpexw3iph2xdes0l7z2msljo425eq3wjva98zu1_e_luubtiq7jjky72"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000007c-0000-005f-0000-000500000068"))), smConvRoleName = (fromJust (parseRoleName "8pir09967ccousxpodxietqxh5g7oyfc88v0ctrkd81h1c0kb3o0iytj25zzzyl9dt76u3_uni"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000017-0000-0027-0000-00620000004d"))), smConvRoleName = (fromJust (parseRoleName "zous175"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000030-0000-0073-0000-007e00000080"))), smConvRoleName = (fromJust (parseRoleName "3l842k1f90kpjszaanvpcr4q6iaazuvj1k55ktx4ce8tajde"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000034-0000-003b-0000-000900000000"))), smConvRoleName = (fromJust (parseRoleName "5omosi7vnqfb6o1dflsk90z5kb2"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000000-0000-0047-0000-00470000005a"))), smConvRoleName = (fromJust (parseRoleName "jybg415cta3c8ec4dactcne29fkfz6ak5vc3jm3jyao8k5qnonusmrx7g01cphdabgk"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000005c-0000-0040-0000-006f00000011"))), smConvRoleName = (fromJust (parseRoleName "hs9z1rxx0cbgofw1zzfd0sj16rxnrk5zvcrlk60k2tb7ph4tpis"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000000-0000-002b-0000-005300000046"))), smConvRoleName = (fromJust (parseRoleName "hlrrn92_r1ny4zfvaq8vfohktir2zcytojyysvz9z0ssvtw_waftf8mp4wilfzuuqy8iti9o_06obfu3nw6z1_7g5t1bp2uvnflvi32carhlxfk88b"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000024-0000-0066-0000-006d0000007b"))), smConvRoleName = (fromJust (parseRoleName "js9bk67g7x_jxgktc2t966moq5tc7m6drwebeilqm62htaafj3rwtidz_zdk4erkbbbio90gp7ly5xluaqcmfp874ppg3160xsnm260q072kg0zkywkqzx1j"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000043-0000-004e-0000-00710000005a"))), smConvRoleName = (fromJust (parseRoleName "jrfij77yed9otwz6nkejgcn8ezxiqk4aj1c1pty9s70x0a78k1qw4h_0dulmtp0ykz4_jafnvp593s6dj57gt7lm2fdgephtdvqy456tyx6s"))}]}))))}
testObject_RemoveBotResponse_user_4 :: RemoveBotResponse
testObject_RemoveBotResponse_user_4 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvRename) ((Id (fromJust (UUID.fromString "0000416e-0000-1173-0000-779d0000433b")))) ((Id (fromJust (UUID.fromString "00000e79-0000-77bb-0000-755c00002fa1")))) (read "1864-05-10 15:49:36.896 UTC") (Just (EdConvRename (ConversationRename {cupName = "\DC4q\1083127w\US{;+r\DEL^\1052023\&1F\SItG\DLE\4203\RS"}))))}
testObject_RemoveBotResponse_user_5 :: RemoveBotResponse
testObject_RemoveBotResponse_user_5 = RemoveBotResponse {rsRemoveBotEvent = (Event (Typing) ((Id (fromJust (UUID.fromString "00001876-0000-4db3-0000-486a00006b65")))) ((Id (fromJust (UUID.fromString "00004dcf-0000-0135-0000-2a6d00000125")))) (read "1864-06-01 10:10:33.383 UTC") (Just (EdTyping (TypingData {tdStatus = StartedTyping}))))}
testObject_RemoveBotResponse_user_6 :: RemoveBotResponse
testObject_RemoveBotResponse_user_6 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvConnect) ((Id (fromJust (UUID.fromString "000024aa-0000-3a4d-0000-4eb600000397")))) ((Id (fromJust (UUID.fromString "00007190-0000-6825-0000-625b0000053a")))) (read "1864-06-08 15:07:06.833 UTC") (Just (EdConnect (Connect {cRecipient = (Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000400000005"))), cMessage = Just "n", cName = Just "aDt\144950", cEmail = Nothing}))))}
testObject_RemoveBotResponse_user_7 :: RemoveBotResponse
testObject_RemoveBotResponse_user_7 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvDelete) ((Id (fromJust (UUID.fromString "00000bec-0000-4016-0000-603a00006b53")))) ((Id (fromJust (UUID.fromString "000023f0-0000-0d4d-0000-775100001f3c")))) (read "1864-04-12 18:28:00.005 UTC") (Nothing))}
testObject_RemoveBotResponse_user_8 :: RemoveBotResponse
testObject_RemoveBotResponse_user_8 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvConnect) ((Id (fromJust (UUID.fromString "000040fe-0000-243a-0000-520300005668")))) ((Id (fromJust (UUID.fromString "00005a6b-0000-6a74-0000-269300003ea9")))) (read "1864-05-01 22:21:29.873 UTC") (Just (EdConnect (Connect {cRecipient = (Id (fromJust (UUID.fromString "00000005-0000-0003-0000-000200000007"))), cMessage = Just "u\EM", cName = Just "KV_{eLi", cEmail = Just "_"}))))}
testObject_RemoveBotResponse_user_9 :: RemoveBotResponse
testObject_RemoveBotResponse_user_9 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvCodeDelete) ((Id (fromJust (UUID.fromString "00005c05-0000-03b5-0000-4b1b00002b95")))) ((Id (fromJust (UUID.fromString "0000596b-0000-0202-0000-2fe900000c3c")))) (read "1864-05-07 14:03:49.845 UTC") (Nothing))}
testObject_RemoveBotResponse_user_10 :: RemoveBotResponse
testObject_RemoveBotResponse_user_10 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvReceiptModeUpdate) ((Id (fromJust (UUID.fromString "000002c6-0000-54aa-0000-37110000258b")))) ((Id (fromJust (UUID.fromString "00004a47-0000-7d74-0000-59060000550e")))) (read "1864-04-22 05:05:46.44 UTC") (Just (EdConvReceiptModeUpdate (ConversationReceiptModeUpdate {cruReceiptMode = ReceiptMode {unReceiptMode = -4422}}))))}
testObject_RemoveBotResponse_user_11 :: RemoveBotResponse
testObject_RemoveBotResponse_user_11 = RemoveBotResponse {rsRemoveBotEvent = (Event (Typing) ((Id (fromJust (UUID.fromString "0000179e-0000-01a6-0000-572a00005b94")))) ((Id (fromJust (UUID.fromString "00006857-0000-6707-0000-19830000300b")))) (read "1864-04-27 03:18:20.956 UTC") (Just (EdTyping (TypingData {tdStatus = StartedTyping}))))}
testObject_RemoveBotResponse_user_12 :: RemoveBotResponse
testObject_RemoveBotResponse_user_12 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvCodeUpdate) ((Id (fromJust (UUID.fromString "000031c1-0000-49ab-0000-5a420000199f")))) ((Id (fromJust (UUID.fromString "00004916-0000-35fa-0000-69ee0000252a")))) (read "1864-05-22 07:06:28.383 UTC") (Just (EdConvCodeUpdate (ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("k5BFlGlD_DauCrC0UbF1")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("PY7khtoS31nc")))))}, conversationUri = Nothing}))))}
testObject_RemoveBotResponse_user_13 :: RemoveBotResponse
testObject_RemoveBotResponse_user_13 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvMessageTimerUpdate) ((Id (fromJust (UUID.fromString "000054b5-0000-50f1-0000-2e8a00000245")))) ((Id (fromJust (UUID.fromString "00000557-0000-48a7-0000-1c0000003c47")))) (read "1864-04-14 16:14:08.404 UTC") (Just (EdConvMessageTimerUpdate (ConversationMessageTimerUpdate {cupMessageTimer = Just (Ms {ms = 7036531193319545})}))))}
testObject_RemoveBotResponse_user_14 :: RemoveBotResponse
testObject_RemoveBotResponse_user_14 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvConnect) ((Id (fromJust (UUID.fromString "000006c1-0000-6eec-0000-5bee000050f5")))) ((Id (fromJust (UUID.fromString "00004cdc-0000-030a-0000-0b4a000034a6")))) (read "1864-04-21 14:06:18.285 UTC") (Just (EdConnect (Connect {cRecipient = (Id (fromJust (UUID.fromString "00000005-0000-0005-0000-000000000003"))), cMessage = Nothing, cName = Just ":", cEmail = Just "7C"}))))}
testObject_RemoveBotResponse_user_15 :: RemoveBotResponse
testObject_RemoveBotResponse_user_15 = RemoveBotResponse {rsRemoveBotEvent = (Event (MemberJoin) ((Id (fromJust (UUID.fromString "00003c9b-0000-5846-0000-463c00006717")))) ((Id (fromJust (UUID.fromString "0000418e-0000-66bd-0000-15de0000710d")))) (read "1864-05-08 19:52:46.254 UTC") (Just (EdMembersJoin (SimpleMembers {mMembers = [SimpleMember {smId = (Id (fromJust (UUID.fromString "00000050-0000-0037-0000-005100000059"))), smConvRoleName = (fromJust (parseRoleName "t3dfsw7dm4ugithtwd1ps9bwjajwg3hljctp1_bbofxyg4b2ug69m0ddplu7tpcnyylzjclayb14nhprn74nztxn4q53sghbv1sgco0o1k"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000030-0000-003e-0000-004400000006"))), smConvRoleName = (fromJust (parseRoleName "s8juk25_k73zsriyng34id"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000059-0000-0005-0000-000d0000001b"))), smConvRoleName = (fromJust (parseRoleName "e3l9hud"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000058-0000-0059-0000-00300000000c"))), smConvRoleName = (fromJust (parseRoleName "5re3xxibf"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000001-0000-0078-0000-004e00000048"))), smConvRoleName = (fromJust (parseRoleName "cyxlkltvr5kq0xoc27iard0egizki863mhpvrfvnb_ftfn_h66rr11qga2z3nlnohjzz37l8gbrbzyl2sae9zlq8iskkp0ie9bjx2svd12c9vj4k_00n5zvnto0aa4"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000005a-0000-004c-0000-005300000037"))), smConvRoleName = (fromJust (parseRoleName "9hj0f0iu7lcl7byu9egq6kqw1biv7fh_0wk_el80n_t8obfxpirh4lk3h18r6kj7xrzh3l"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000005-0000-0004-0000-001100000030"))), smConvRoleName = (fromJust (parseRoleName "7rcwais2ijt_q6we6kbbv36i"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000076-0000-0027-0000-003000000060"))), smConvRoleName = (fromJust (parseRoleName "v7nxejork_twe04mftm8js_99if2q69jt6wnamwc1yvcdgjz3vybz01ayi8a89c0"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000005c-0000-0027-0000-00020000007e"))), smConvRoleName = (fromJust (parseRoleName "uhte2p_7z7jnjjc50j058uu9z5pvweakpexdf4m1kgzw1b61k3b0329fj96f138zc8uak8et8m1ebroooptqwyol9gqj_l4iqk8lj9qrrebhvt7750xilx"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000005-0000-0010-0000-000a00000049"))), smConvRoleName = (fromJust (parseRoleName "xyix3yu6pz32ic0z5gofm1egbgwyn9hl7rysabzuxayw2"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000059-0000-0011-0000-000500000021"))), smConvRoleName = (fromJust (parseRoleName "wowkvcgu9jvsnrgze7n3wjggq1tk6_n52ur0afo5dqrfz6skaa2f8u9j00nr0p1_tvvmapjgxq8y7cgu85079t35f7y8cxo4lm_34amsyof_d"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000061-0000-003f-0000-003e00000019"))), smConvRoleName = (fromJust (parseRoleName "5cti2"))}]}))))}
testObject_RemoveBotResponse_user_16 :: RemoveBotResponse
testObject_RemoveBotResponse_user_16 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvCreate) ((Id (fromJust (UUID.fromString "000009ba-0000-6b8c-0000-1d3200006a7d")))) ((Id (fromJust (UUID.fromString "00004397-0000-29fa-0000-26d200000de6")))) (read "1864-05-29 16:05:00.798 UTC") (Just (EdConversation (Conversation {cnvId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000000"))), cnvType = RegularConv, cnvCreator = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000002"))), cnvAccess = [], cnvAccessRole = PrivateAccessRole, cnvName = Just "", cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), memOtrMutedRef = Just "", memOtrArchived = False, memOtrArchivedRef = Just "", memHidden = False, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "es189p89ci8ewaet3d3vbyz0zxp4r8vabj7p95dnrb4pmgi5bapj5jqebgvpiep1e34_rbcd5vq61bwsh2izrmqoubjxiq_h1_klxtqw7_p4tm2qb"))}, cmOthers = []}, cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000002"))), cnvMessageTimer = Just (Ms {ms = 8511282435810351}), cnvReceiptMode = Just (ReceiptMode {unReceiptMode = -2})}))))}
testObject_RemoveBotResponse_user_17 :: RemoveBotResponse
testObject_RemoveBotResponse_user_17 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvAccessUpdate) ((Id (fromJust (UUID.fromString "00006c1c-0000-6a32-0000-263d000028b6")))) ((Id (fromJust (UUID.fromString "00001390-0000-1e26-0000-5cd40000568a")))) (read "1864-05-19 06:51:06.978 UTC") (Just (EdConvAccessUpdate (ConversationAccessUpdate {cupAccess = [LinkAccess,PrivateAccess,InviteAccess,CodeAccess,LinkAccess,CodeAccess,LinkAccess,CodeAccess], cupAccessRole = TeamAccessRole}))))}
testObject_RemoveBotResponse_user_18 :: RemoveBotResponse
testObject_RemoveBotResponse_user_18 = RemoveBotResponse {rsRemoveBotEvent = (Event (MemberJoin) ((Id (fromJust (UUID.fromString "000021b3-0000-0c9c-0000-056000005776")))) ((Id (fromJust (UUID.fromString "000026eb-0000-19d7-0000-7daa00001ffd")))) (read "1864-06-06 13:41:03.509 UTC") (Just (EdMembersJoin (SimpleMembers {mMembers = [SimpleMember {smId = (Id (fromJust (UUID.fromString "0000007b-0000-0022-0000-00700000006c"))), smConvRoleName = (fromJust (parseRoleName "6y0socr69p5d8d9zq7zt0qnm4_v798q64_xduq916j8o1a059xj"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000079-0000-005c-0000-006f00000054"))), smConvRoleName = (fromJust (parseRoleName "vocd34fh5latzyb0j6z7madyq3qwbn_w_ubuqsx8d0womrb2zxrz37dp"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000028-0000-0077-0000-001c00000054"))), smConvRoleName = (fromJust (parseRoleName "154m2s3vvks6sl1ype16xeevdsmz74mq08i7nqbsxrcvbu0kvx1zi9qkr2z_aldqt11g31j3jbj69y3kan_iuujbiufgrgm8"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000001e-0000-0008-0000-002600000047"))), smConvRoleName = (fromJust (parseRoleName "2d9xnjf3us7f1yuskvq6wqlv8pgmy5_vp5bmnohqiml4v2a9_hi19i4gsq85nsdd_utdt5imwne"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000014-0000-0054-0000-003f00000070"))), smConvRoleName = (fromJust (parseRoleName "hgisq03p95akfmbibg_m"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000006b-0000-005c-0000-006a0000005f"))), smConvRoleName = (fromJust (parseRoleName "v3fo8vnvjf2at8o4p9gy5xkrkz1183xjlurc5o6_cxleuzfosno4jncf4vwqy87iyyym18gfaa6na8r6n9wmn7sci7p5kelop"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000035-0000-0036-0000-00320000007a"))), smConvRoleName = (fromJust (parseRoleName "x7qogw6rbampnsu0"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000005f-0000-0046-0000-004900000010"))), smConvRoleName = (fromJust (parseRoleName "bbzdlzkcy3w70d9_ypnw7j9gq1jx5orf6zp13czbpsqh34bj10h48z7l267p1aaio2owi1mz8fawhkvn51e91ntp7glws0"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000001d-0000-0032-0000-004e00000038"))), smConvRoleName = (fromJust (parseRoleName "3mpa7m0e01jd1xdrv"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000005d-0000-0068-0000-006800000018"))), smConvRoleName = (fromJust (parseRoleName "nfuqlcagw_snpzgxx76k1jxzd2vz26hhaawb4g5waa7uhiny__q3wudmtwle7mp61qa1vpk19t6kxymjbtlnpk5z7lujqw5b1o6q7b3l_zp59sz0at6at"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000043-0000-0000-0000-001a0000004e"))), smConvRoleName = (fromJust (parseRoleName "haoyxi7m6nkmen5djh4"))}]}))))}
testObject_RemoveBotResponse_user_19 :: RemoveBotResponse
testObject_RemoveBotResponse_user_19 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvConnect) ((Id (fromJust (UUID.fromString "00003f9c-0000-3fb7-0000-2aa3000009d0")))) ((Id (fromJust (UUID.fromString "0000450a-0000-4ba4-0000-075700005716")))) (read "1864-05-31 06:49:07.563 UTC") (Just (EdConnect (Connect {cRecipient = (Id (fromJust (UUID.fromString "00000005-0000-0006-0000-000300000005"))), cMessage = Just "{F_`d\1034998", cName = Nothing, cEmail = Just "\v\NAK%iM\30565"}))))}
testObject_RemoveBotResponse_user_20 :: RemoveBotResponse
testObject_RemoveBotResponse_user_20 = RemoveBotResponse {rsRemoveBotEvent = (Event (Typing) ((Id (fromJust (UUID.fromString "00001fdf-0000-35fe-0000-68bb00006ced")))) ((Id (fromJust (UUID.fromString "00007ccf-0000-06a5-0000-0008000036f9")))) (read "1864-05-04 22:52:18.034 UTC") (Just (EdTyping (TypingData {tdStatus = StoppedTyping}))))}
