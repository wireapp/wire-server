{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ConversationRolesList_user where

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
testObject_ConversationRolesList_user_1 :: ConversationRolesList
testObject_ConversationRolesList_user_1 = ConversationRolesList {convRolesList = []}
testObject_ConversationRolesList_user_2 :: ConversationRolesList
testObject_ConversationRolesList_user_2 = ConversationRolesList {convRolesList = []}
testObject_ConversationRolesList_user_3 :: ConversationRolesList
testObject_ConversationRolesList_user_3 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "qivjyo15083kdcfd71xdt5txad6nm60j40gwyadieiuqolf0fhx22yq")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "l0aaz1l09w7e9jjwtwlu4sglbutu9ro2xnudhskbo_a4bd82djdmyxl1pn")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "el02t9e_zlro1eqnxfxp4ku9bt61kf6fcydzii17s72shr0cv_oy6vvhhjn653kc320qwuskaaiymjwl1mr53h")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "tzm9a_108rwg_xhugqbs4dpxvv8ogt6moh5foeqw_wq703xckxtlzvx3rung5kmhsesup3cr0r5wcxismz10ivlkntfxvxg3yudzyt5bjmfpkord0hqyq2i__")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "5i_kc54a2lmdainpykf6dvlj5ger3w3gs7budeyuvkltpldz2jkfzbug1kasnp373or_q6uzelgw3f7sf69twycg_a20fi6r5rbune4br3teprox610syixhk8u5has")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "i9ot5sogmopbrko4hdu0h9yhewjaalpu3kz5i4sjdguacq7fnnq7mo265misule75y5jyrjkato9tzz9kv1ld2ezd7wh7u")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "nghr1m_0fd_a8titph0fwnz9c1qn6gr4vccw1e6rphsh156ghgco")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "kyjfgemwhinwj74fy_h38n83gzt3efuwxyqx_xtn576lzo9msje7qowkgg15dlj_olx8b9m90h8gh8k850ydzu5")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))]}
testObject_ConversationRolesList_user_4 :: ConversationRolesList
testObject_ConversationRolesList_user_4 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "0hy2bm9")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "lndj3i_cj237pz4jjrvq1nu")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "_65g43vomltmlim")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "zj_973mbc0mq8abo5882pqxs2bevam6oxfr7vxhafnweagt04b7eczzakk6elth8dy24_y538g_c51")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "m0b7lrwkxj1a3u")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "0enlf0inamihjpgkxzwn9v80tvhenkahhv1k18x7e8w47dfh")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "scop6vtiuzr5r61yme1ummuuoscibbf2iqwo1tle60_yz")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))]}
testObject_ConversationRolesList_user_5 :: ConversationRolesList
testObject_ConversationRolesList_user_5 = ConversationRolesList {convRolesList = []}
testObject_ConversationRolesList_user_6 :: ConversationRolesList
testObject_ConversationRolesList_user_6 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "55axgnz4iuvub3auzzjhrxr5tu27zk8wzgk3kspt7ma8r8wv64u7fipcctbgo1t5")) (Just ((Actions (Set.fromList [RemoveConversationMember,LeaveConversation])))))),(fromJust (toConvRole (fromJust (parseRoleName "i5yd8etfvs8ljb")) (Just ((Actions (Set.fromList [AddConversationMember,RemoveConversationMember,ModifyConversationAccess])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "7z1nfwi3tmxtv4qg9f40velx2_d8ybrn1ktv4igll4x4_h9t4p2vo5y_t2sp6l9_qjp1f6pnspjke6gr0q_")) (Just ((Actions (Set.fromList []))))))]}
testObject_ConversationRolesList_user_7 :: ConversationRolesList
testObject_ConversationRolesList_user_7 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "30atgq4hijpc9kupt8q")) (Just ((Actions (Set.fromList [LeaveConversation,DeleteConversation])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "9l680btx88k5d5ve45cbhq8")) (Just ((Actions (Set.fromList []))))))]}
testObject_ConversationRolesList_user_8 :: ConversationRolesList
testObject_ConversationRolesList_user_8 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))]}
testObject_ConversationRolesList_user_9 :: ConversationRolesList
testObject_ConversationRolesList_user_9 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))]}
testObject_ConversationRolesList_user_10 :: ConversationRolesList
testObject_ConversationRolesList_user_10 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "x208euhjhmezojqw8bu2rcixxm02w6b1e2pccna0mqatanv4j1fqc8k1yv62x9rar2k9723dofn_85jkz7c6jwyz7zuabqt0ek04odj48c5q436fv0qon6")) (Just ((Actions (Set.fromList [ModifyOtherConversationMember])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "9r5edzyq8gnxggtl4d94mokqk54l4kn1l95q4ea0jabl8oqvmopcqf92om4d7tnk2d3twqio641eoxwamocr5j8vuhx4622woh4i0b_y7b98tcm8u5bf4ep00s0lvj")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "m4g39cytzqfr6auc3znv_i5k20jomamdhohjsrzeywcr5nlj2n_86e7b8e5qpub6q0xxrysf0o4_txvpykx6t4tp_nb7sfnzh7__j3deqt3ph2xattdngesz20fph")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "490g")) (Just ((Actions (Set.fromList [AddConversationMember])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))]}
testObject_ConversationRolesList_user_11 :: ConversationRolesList
testObject_ConversationRolesList_user_11 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "985gduphqw2fhyelh0obu1k61ceefq63i1s70o5jm3ogy")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "0en7cqghsz07v7y6eijlkcsmyazie9itg0mkojvqnm3zlo0b9b8zw2q70mzi27msac1e072urgcuz_wmu8pvlynf0_1bxs114_fa1p9ryz5lyxs8b03_ghnr1dafs")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "g7ikhe9c3wjw4b0y8wopwxktr57_lr7cs_19ple3")) (Just ((Actions (Set.fromList []))))))]}
testObject_ConversationRolesList_user_12 :: ConversationRolesList
testObject_ConversationRolesList_user_12 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))]}
testObject_ConversationRolesList_user_13 :: ConversationRolesList
testObject_ConversationRolesList_user_13 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))]}
testObject_ConversationRolesList_user_14 :: ConversationRolesList
testObject_ConversationRolesList_user_14 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "g3t2800cog9ed0xv6umyqth28kegbrh6slbvv9z_of6n7pkv5yp4cs_pkr3t64ul7a9qfdcsj9du")) (Just ((Actions (Set.fromList [AddConversationMember,RemoveConversationMember,ModifyConversationName,ModifyConversationMessageTimer,ModifyConversationReceiptMode,ModifyConversationAccess,ModifyOtherConversationMember]))))))]}
testObject_ConversationRolesList_user_15 :: ConversationRolesList
testObject_ConversationRolesList_user_15 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wi4vzxrcn6k540awous4gt7wodt0bnqho9q9ne_ss4n8bhj1m9scei9wdoe24fl9ul2fxenpnyvy_qivh40v654154f9_hle4x3s8ey")) (Just ((Actions (Set.fromList [AddConversationMember,ModifyConversationName,ModifyConversationReceiptMode,ModifyOtherConversationMember])))))),(fromJust (toConvRole (fromJust (parseRoleName "z69ojqszl9ed_zm4w0z7iua09n58picshqy4qi4plotd93dvc2xh7h25mlfrerfo4ifcbx6w1yel4v75wbxqggigchz66ok84jktneh")) (Just ((Actions (Set.fromList [RemoveConversationMember,ModifyConversationReceiptMode,DeleteConversation])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))]}
testObject_ConversationRolesList_user_16 :: ConversationRolesList
testObject_ConversationRolesList_user_16 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "norbkea9e8y4lakig16zh7u8nfzu7e48ssoq7m8q23j8qbkhaokrdm2kvn")) (Just ((Actions (Set.fromList [DeleteConversation])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "fvxy5gp97v_4aityerimszyqqyunqbrufskzue4teupm4_227ehmksxctahx9szrzlo7yio6tjoe989")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "0e6m6txi77fvbx58luvkv526ga_9dhcra9")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "e2md0c51e108kljuo8m3dfvqn2zs_ernl3feycx3hl8veapsm4")) (Just ((Actions (Set.fromList [AddConversationMember])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))]}
testObject_ConversationRolesList_user_17 :: ConversationRolesList
testObject_ConversationRolesList_user_17 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "9uqrgukf5c0dny3opvxeqqrszmzckakmgy6xj5pn8bbaf0_h6rnvckri8htm4uvawq85q20z1jh756mjo2so980ydf9q45jzbfl9169dsbmp6h08ur3v_z01hltex0ap")) (Just ((Actions (Set.fromList [ModifyConversationMessageTimer,ModifyOtherConversationMember,LeaveConversation])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "ra9t0ha10xdj8n0of35672ove9zx7x20h2uc40pwtiscq1t3itujp3uljjklsdq46gcxhafx490933damp7s7y8jqx0fmv8xdl5bqv3f49v")) (Just ((Actions (Set.fromList [AddConversationMember]))))))]}
testObject_ConversationRolesList_user_18 :: ConversationRolesList
testObject_ConversationRolesList_user_18 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "lcnd2mdnui5w0krn7lhinj9nozuhplzqv_w_f2ybbcghm832218fvea6bnas5ryuobmvrwzdull1t43h489ox")) (Just ((Actions (Set.fromList [ModifyOtherConversationMember])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "kdwql8kfnfzxwgy8u5hqlo3ncdp3jzrlv")) (Just ((Actions (Set.fromList [DeleteConversation]))))))]}
testObject_ConversationRolesList_user_19 :: ConversationRolesList
testObject_ConversationRolesList_user_19 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "5evbr6i5debrl")) (Just ((Actions (Set.fromList [ModifyConversationReceiptMode])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "j4w317netxe4wuugi97vsq82h_sjnr8lx4ggcig43tf3aom7017")) (Just ((Actions (Set.fromList [AddConversationMember])))))),(fromJust (toConvRole (fromJust (parseRoleName "7yshqpmpk_59yrlici3lh1")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "1oxsgu2grgt9dhfb78sgndpnhu2d0_9dxrl5a6m6hw4bjru3izuqh1gdkztsofhmxedqm5ubjffpeq7c9mg4515g22vo_etycup62h00")) (Just ((Actions (Set.fromList [DeleteConversation]))))))]}
testObject_ConversationRolesList_user_20 :: ConversationRolesList
testObject_ConversationRolesList_user_20 = ConversationRolesList {convRolesList = []}
