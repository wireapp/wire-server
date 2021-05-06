{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.SimpleMembers_user where

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
testObject_SimpleMembers_user_1 :: SimpleMembers
testObject_SimpleMembers_user_1 = SimpleMembers {mMembers = [SimpleMember {smId = (Id (fromJust (UUID.fromString "00000080-0000-000d-0000-002b00000044"))), smConvRoleName = (fromJust (parseRoleName "rnm9pkn7u_jidybiaxb0bbb_goonrxa_kmdmrh_c7_t37ec_w89hg1zafbf3niqjtmylp"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000043-0000-0044-0000-007b00000067"))), smConvRoleName = (fromJust (parseRoleName "yzb0d2y8x4db7ik9ro25zbf3magmthnegjueigx7t8je3_6ho76f1rbot1f5b419ys9vd6ijstf8sglw4o"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000026-0000-0064-0000-002a00000016"))), smConvRoleName = (fromJust (parseRoleName "mg03vn2lum9_l0c5jg7osjytxk9cvd_ryb5nr3lv89aqp5087r"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000072-0000-000e-0000-005500000066"))), smConvRoleName = (fromJust (parseRoleName "qbnfenal6btw3b7j1feqyhziv0jtr1dk8sayl2sl_gtfbm_567riezz8s90mk9lxx2_85cxu1xpv1hc0nw1rzc92nzwz259npvp0vz"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000029-0000-0080-0000-00110000002c"))), smConvRoleName = (fromJust (parseRoleName "bkaup8fwvgq7poxeakgd8c8p24w1ko"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000037-0000-0036-0000-002e00000000"))), smConvRoleName = (fromJust (parseRoleName "6cnpib3ma072hjtbilyxyqaglj9e95rug68b8xh8bp"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000007c-0000-003e-0000-00460000007b"))), smConvRoleName = (fromJust (parseRoleName "rrzu0p5wtuj3n_5f7nzkbznzrsbo1b81f1g9cqy69twez0w9vhulnxt784gln5wjpz8kwexqlq_cyo5f7nw7dcd"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000006a-0000-000a-0000-003c00000024"))), smConvRoleName = (fromJust (parseRoleName "bkj2j_5lm8l3p3rslvwqhiq__nf3lfsxe_mzzykpb"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000068-0000-0077-0000-000b00000064"))), smConvRoleName = (fromJust (parseRoleName "qaav4k96l5wvvnbm2n82w7xoq0uw0ucbwfkafmu6xvogy8b0dz14sxnvnhk3nkhu"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000068-0000-0035-0000-002200000046"))), smConvRoleName = (fromJust (parseRoleName "8benkw1hkfwyguhnrc8vr_mpzqa115n6omzdlvkl4_1cgg6266fuzj_4rem_ma80m9fkapm428y5aox1bf_1lf70xx6gmxaax_xeonfb1l5qlhd"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000000b-0000-0039-0000-001500000041"))), smConvRoleName = (fromJust (parseRoleName "2l_f4uvxyw9yyu06b4vc0pfvlxinayqj4x1vt8m0"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000076-0000-004c-0000-004b00000039"))), smConvRoleName = (fromJust (parseRoleName "e4xadh4tiuvv78rw6iunzu1puj1p457hdk0jr"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000000f-0000-0030-0000-005a00000012"))), smConvRoleName = (fromJust (parseRoleName "rvjoc7f23oxz6dn4ucvqg1_z5jhrzej5uk9zhty_f8p_hg8shcd"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000007d-0000-000d-0000-000600000047"))), smConvRoleName = (fromJust (parseRoleName "rut9uxujnkc8ru5s306a3svpv0ymaxxs7i5pabt2iykr_vdpedqd3ko3hq82d7m39cncd9okiz_lmk18y093lzf5r20q4qdehhv08eq72bil"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000051-0000-004b-0000-007c00000048"))), smConvRoleName = (fromJust (parseRoleName "4zur7o0b2xldn59ccuueleyaw0ofx71p0c1o4yy7vj6_dnxy4kzits69isuzmsriy2xsyp8nfhs_64a69pg5s49a6rq70jd75uvs9y89j"))}]}
testObject_SimpleMembers_user_2 :: SimpleMembers
testObject_SimpleMembers_user_2 = SimpleMembers {mMembers = [SimpleMember {smId = (Id (fromJust (UUID.fromString "00000000-0000-007c-0000-000300000026"))), smConvRoleName = (fromJust (parseRoleName "uhhfo_a47bi3sow0l9r67q8_m5nqz2y6j2xjdwa0ayz9do47ao3ivpa1dq_o2tjl9pjnh_nou54_q_2j7z7oo"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000006e-0000-0065-0000-00120000004e"))), smConvRoleName = (fromJust (parseRoleName "c5lhz1bps45zmv1muukimhcr_x79v"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000003a-0000-0078-0000-00670000003d"))), smConvRoleName = (fromJust (parseRoleName "virvuovv6nt3x25lsufol72b_wpchpreqz653gaix7boggmkhgo1k1_r9ylhmvtrdi5nnan0zb77z_0e4vb6"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000054-0000-001d-0000-00250000004b"))), smConvRoleName = (fromJust (parseRoleName "c0uurfdjo2hc_r80kn11tjzrexq6lyx1ggfm"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000006c-0000-0038-0000-002700000059"))), smConvRoleName = (fromJust (parseRoleName "yhd51dofcualcfek5bj5ihntuaxb02uoxb13"))}]}
testObject_SimpleMembers_user_3 :: SimpleMembers
testObject_SimpleMembers_user_3 = SimpleMembers {mMembers = [SimpleMember {smId = (Id (fromJust (UUID.fromString "00000072-0000-007f-0000-007600000031"))), smConvRoleName = (fromJust (parseRoleName "35cq_441r_kepoymbh4j_1kcpr437f1i3cv5gn5zbisy7a_u2u191t78ipp8i1wkexkhxs0xmb9b8l3vcnbzbqc_k6mlzpmv40n6q6wjityvilv"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000003d-0000-0064-0000-005c0000003d"))), smConvRoleName = (fromJust (parseRoleName "xbc4a4i3thmvm_ecweuft8201pkrai9fjyxr8kkk4xhkni3hxatu37qs6w_xo2vhxu6zfufsfkyv5uouasma6nersgpj3b3pszc6ouhxg4a_ydplf5"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000019-0000-000e-0000-002f00000020"))), smConvRoleName = (fromJust (parseRoleName "dw77o238lp04pe5dfggwbgk3g6ibah4zrjcbk_c_ojj1aphji7bmr2k6hzpvg3we9_jlopwhclkskhehx79"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000001-0000-0051-0000-004b0000004b"))), smConvRoleName = (fromJust (parseRoleName "ybg3ivbzwqnts0wdj0nqqg_mnkllqf3pt3qgzojd9l2aipz5yx1fqmftaof6u22zn4xxsq3d"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000068-0000-0015-0000-006900000055"))), smConvRoleName = (fromJust (parseRoleName "vkphzm3hsk7uurm9otw"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000040-0000-000b-0000-006a00000013"))), smConvRoleName = (fromJust (parseRoleName "x781q5006m5n7eyr9jhx0bnl3c0ja13y1fb94iz49ccjqi1d4q11ufoevp3ernfmqg_6dm41p71mdp_n7i8tq8kohfzejsne"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000018-0000-0023-0000-002800000076"))), smConvRoleName = (fromJust (parseRoleName "wkuv9n2pcgjr88hqd_zon81eipu304r0do_y3econ0a2wnoiac4luz0r9t43dhoj0xys8c"))}]}
testObject_SimpleMembers_user_4 :: SimpleMembers
testObject_SimpleMembers_user_4 = SimpleMembers {mMembers = [SimpleMember {smId = (Id (fromJust (UUID.fromString "00000012-0000-0054-0000-005400000073"))), smConvRoleName = (fromJust (parseRoleName "cl0umxmp0"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000073-0000-0076-0000-00120000005c"))), smConvRoleName = (fromJust (parseRoleName "zhenkojlwgsu0dc1le7wdizsaadwd1oyswokwlr_92qm6zey3e21z18zxzhoblteyv4fmhtm_a3qfoftk2rts44c6uiwptkkvf3ndq039nc7e5dty9wnt7sys"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000006f-0000-0024-0000-003700000018"))), smConvRoleName = (fromJust (parseRoleName "i48v9rp7"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000004b-0000-0023-0000-002c0000003f"))), smConvRoleName = (fromJust (parseRoleName "rm"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000060-0000-0026-0000-006300000075"))), smConvRoleName = (fromJust (parseRoleName "i9dh4uxofjjvgu7g3nb93mfkucgw9bd66di68u19b8y565co41qj7wb1lq4rni1g2oycsd3vnjrdf9_76lgg0pp6pxusg2hcqp3uqzdnuzaoj"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000035-0000-0063-0000-001d00000036"))), smConvRoleName = (fromJust (parseRoleName "rvuh6s3x6tth71rn33ppditi13p2tmgiqwpvjg1wma0hqd5f3xtwom2nc9l0hkwk9bngwaclhw95a1o7uklq6ljhp9bi85"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000056-0000-0050-0000-002b00000029"))), smConvRoleName = (fromJust (parseRoleName "dmhzck9j4sm781nfz8tomanpwvr2cqc3ag5y88hxlmraz87xqd1auhfn_73ffbisxo9ra0_5hbxs0tjawj19pur"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000004-0000-0076-0000-004b0000004e"))), smConvRoleName = (fromJust (parseRoleName "294uhda1dfuoypmm8beh6d_l0elsrjf6me9r1qci9yu6xobj2dtn3xavz8rlyvlepq6h05_wf08ir_xogatui6mz78f481_nom_ob2xh2"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000001b-0000-007d-0000-003700000047"))), smConvRoleName = (fromJust (parseRoleName "9xs5td04zfri6lw7nn9roldb5lzlcvvx59obu4blnjvhyypirnx4hu98kn09ftla04p6zna311bu_lr34v_ho2fe2bbwg60z282vebgc8cdar8aaqf1_8l"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000053-0000-005c-0000-003f00000060"))), smConvRoleName = (fromJust (parseRoleName "tj8jb_y11fgpdjdec81il3xfvb_k13p6nhknlrkggl5wc5yau73c"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000001e-0000-0007-0000-000e00000035"))), smConvRoleName = (fromJust (parseRoleName "sj57z6zo1_n6vjzcnh9xj6pri48vre3ehyd24rg1ud2ksnd_hkrollf73uf5sjczhg_6te2mjv1d5coogpsrk170p8_l3ha_qhks6e_bn8novp7n2y2dcx"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000003c-0000-0001-0000-003800000037"))), smConvRoleName = (fromJust (parseRoleName "eljc9izvcl_408ue5xnz0uorokc1eytysgzg_a_qyi4hl_oz2dok3mgfd9f57m3h7knn74inhw_q4rbcnzzkq3_zbbbq"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000049-0000-0031-0000-001000000015"))), smConvRoleName = (fromJust (parseRoleName "nzhmuu03oxrvb_38xpl7sf1skiujr_zf79wuqgq3ido7cf935sna2vs7l5wcsywz3ruax6eo150h"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000035-0000-0065-0000-00530000002b"))), smConvRoleName = (fromJust (parseRoleName "1b1z2oc693e0imvze_i6njakal3gbob9uj4w8i5y17qdv7v48x9ow"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000004-0000-0075-0000-000900000005"))), smConvRoleName = (fromJust (parseRoleName "3mg4cn6mp3w4tykhv2rcjfptty662h6lw1x6rtba606vdh7nls5fkuolwlv1cbzxo109fpvhjxfk5vluas74ypvoz02g5hz4dzcwka85dpg26zup52b"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000001f-0000-0040-0000-00190000003a"))), smConvRoleName = (fromJust (parseRoleName "gt2q6a48sedz5dfmc19ghs_r5kvjhh"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000001c-0000-005f-0000-002d0000005e"))), smConvRoleName = (fromJust (parseRoleName "g1ige9zguym9gkddnei1sp7lc28bjawyhbo8448rszg82mdb1hcyo4oppq9stwy8u2lubvjmbwnb9br_tez63gbfezh2v4khmeilk70riwfr0gvinr2"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000002c-0000-0027-0000-00220000006e"))), smConvRoleName = (fromJust (parseRoleName "mg_fe37oi12eonnbh6i3s7fgazn7fz0uxnojjcyg__w6w8ph0w4qs_na7ifuafnjq92qlyfc9np6gfydq84e1uarkspwn1unsni7nf822mc5eo"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000041-0000-001f-0000-00530000002a"))), smConvRoleName = (fromJust (parseRoleName "dr7u_97e_ba_q8d88_vplu7fdkh3n2k6ayltz7ddzfbh8fcf6_0_8n066yrbgrqna6p9fxurv18ajiu89i1o"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000003b-0000-006b-0000-004400000050"))), smConvRoleName = (fromJust (parseRoleName "svlu2yli9qdvsbfcnu61ps_z8_4kztd6aommcd6zhkzl6bz0cpazoq5q9"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000008-0000-0064-0000-000200000071"))), smConvRoleName = (fromJust (parseRoleName "rduyy427tld6bezm6z4ize65ja38_x2n72qap7vk1f7uxnpduafu318l2i86scxz808px2l"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000045-0000-0080-0000-005100000040"))), smConvRoleName = (fromJust (parseRoleName "dly51bnofrtqke8r1usu70vex6mpt59vx_8zu6sprhsqxp_i9svvj0go1d_"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000006d-0000-0063-0000-003d00000078"))), smConvRoleName = (fromJust (parseRoleName "ly37p5ktwi6cay4uypsidzzwbrr2pnfeq0wsxrt8u88hyusinda9d86hv7g95za2j769tt9gh9e4il57e"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000028-0000-0050-0000-00000000006f"))), smConvRoleName = (fromJust (parseRoleName "u4rlokkbq8bx5cku3ae1ip8ktus3lijn8g4j1lr2oioimhq4zojnyyivsr4r05_bdhurs_5obyzponmtcido2saxx8flixjdg7man14egflzqu0m64m5q"))}]}
testObject_SimpleMembers_user_5 :: SimpleMembers
testObject_SimpleMembers_user_5 = SimpleMembers {mMembers = [SimpleMember {smId = (Id (fromJust (UUID.fromString "0000002a-0000-0018-0000-002f00000068"))), smConvRoleName = (fromJust (parseRoleName "al7y35yidtfsx2xs40jrbt279r1dvjgjtuj72_q1t3h_6wlkx_mmhpgyx7"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000020-0000-007d-0000-00140000007a"))), smConvRoleName = (fromJust (parseRoleName "kkbd1bnksly0oqkf0xjhp8yvejnm24fu9dl7_7veje338tn6"))}]}
