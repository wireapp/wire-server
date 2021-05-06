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
testObject_SimpleMembers_user_1 = SimpleMembers {mMembers = [SimpleMember {smId = (Id (fromJust (UUID.fromString "00000074-0000-0032-0000-00180000001f"))), smConvRoleName = (fromJust (parseRoleName "4_9yuxkmty8m9oaz1wkeebweximjrvufc5st6bick13tmvu_fjkb3ftg21hdu4w4tb3j_"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000007d-0000-006e-0000-007f00000029"))), smConvRoleName = (fromJust (parseRoleName "0_h7qg_zgld468sqrt1u94bjxypo2axd3klpab4ugxiu5i5qll_e_y5k03j8fc43krh7gnlbligjqatla_gjsk0d0r1lhbk7hgwm8snyv4lq4vu"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000032-0000-0046-0000-002100000072"))), smConvRoleName = (fromJust (parseRoleName "nhn8a5neerocn5f5wtayb4ksus0fcp6nwckbg9ajni"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000068-0000-0009-0000-007400000056"))), smConvRoleName = (fromJust (parseRoleName "uomkj8ohw"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000077-0000-001e-0000-003a00000034"))), smConvRoleName = (fromJust (parseRoleName "h6vqey8r0kk_svt0fdu9qqxc5l"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000003-0000-001c-0000-006500000033"))), smConvRoleName = (fromJust (parseRoleName "tavx2dg8wxy70wkwp5os4raioghrvb86o5pqrt9oyfodcrg_t5mjcxzr1101"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000049-0000-0003-0000-005200000079"))), smConvRoleName = (fromJust (parseRoleName "hrdt5svb4jb_sf_mhan6jkj02bupi9sbovua3cjxojxk1sp69xi19__rqv6pkfhr3f2pdtr46bcb_56gfk714ztqoeiethrgtq6bfbt1b"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000061-0000-001b-0000-002000000073"))), smConvRoleName = (fromJust (parseRoleName "im86jvlr0bbw6mm9jjrk_e5osuql0x0zd3v1868qclb3bvf_152tf9lwj7wqioaw90_l"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000027-0000-002b-0000-000c00000072"))), smConvRoleName = (fromJust (parseRoleName "49_5w2qlp0pt3xtth84cx6yjcenlzhcm7t4ydpcdkvmozk9pbss4x8d7d1s6r6q7cp2typfopg1fqfc"))}]}
testObject_SimpleMembers_user_2 :: SimpleMembers
testObject_SimpleMembers_user_2 = SimpleMembers {mMembers = [SimpleMember {smId = (Id (fromJust (UUID.fromString "00000015-0000-000e-0000-004c00000050"))), smConvRoleName = (fromJust (parseRoleName "5p0suczi_m1lyl86_kmyz81"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000028-0000-003a-0000-003800000008"))), smConvRoleName = (fromJust (parseRoleName "_f_ui3yl5c1o9rst51qr65katttv5_cv3l5fen4h0z4ygpibsumjch2vvn8vjt8ns53ewa_yx_"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000012-0000-0060-0000-00460000003f"))), smConvRoleName = (fromJust (parseRoleName "r5j8t0jnm4zw1f24oi0cw2rh20uv9yqq4h3kuk9zu87uhvvf6y_btp3jc"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000014-0000-0010-0000-007900000033"))), smConvRoleName = (fromJust (parseRoleName "sihalkoe"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000015-0000-0063-0000-004c00000036"))), smConvRoleName = (fromJust (parseRoleName "7woagcuug413uc1qoi664t8xtay8mb2x2g7z5ebj52q9x8w3qmvi785mngy6dk2fpoexy3v8ko0us0fp312eq85dqn0sgb8wjrgsil_k6ht4oev6oi712c"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000018-0000-001b-0000-001b00000032"))), smConvRoleName = (fromJust (parseRoleName "xd65zn_vlnvnzdvfabyl56iwdjqnbgvfedsojwgh22a5jx0kzz__opgm_uiwsf2cen_4byicb3m6lgmaesl0djcnd6344fxjf91e8nk4zl4wu3"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000058-0000-005f-0000-00080000006e"))), smConvRoleName = (fromJust (parseRoleName "_i5zc59lblt4hnzhdga6w0blpn7o6ot0foj721fsazxn5p438e50hkygtcqhx163u0o4n_gizj_r9bkxvw9r0g71"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000022-0000-0046-0000-003c0000003b"))), smConvRoleName = (fromJust (parseRoleName "yl2q7l68234tx8jdp17wx3qd9_55hojr0z38wh90yytoy7s3hu37ovbq7ze"))}]}
testObject_SimpleMembers_user_3 :: SimpleMembers
testObject_SimpleMembers_user_3 = SimpleMembers {mMembers = [SimpleMember {smId = (Id (fromJust (UUID.fromString "00000064-0000-0014-0000-00730000001c"))), smConvRoleName = (fromJust (parseRoleName "2cgyn08jtge862fpg5nw_bgwx6m95pku9ope6f013_11915ud5p5mty62keie29ifz2hih56s0a252s9_lczh"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000065-0000-0035-0000-007d0000007e"))), smConvRoleName = (fromJust (parseRoleName "d0"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000007d-0000-007d-0000-00230000006d"))), smConvRoleName = (fromJust (parseRoleName "h0txm20uyx3425iow_bst0i7duiawb8ddbdn6mdrovcq14n4389rur756if5dm0xq9d1s_j7ycm_72ad20zr_mm0434x"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000013-0000-0079-0000-00340000002c"))), smConvRoleName = (fromJust (parseRoleName "e7kwtdqvcyju5cjmv8v85bkzr97__7mxzc9x"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000003d-0000-0067-0000-001b00000062"))), smConvRoleName = (fromJust (parseRoleName "l5z3r32ve_b7cn1pltpieltxvl9xfitzhmgez8dw_yg3ner8hto6t_3it2_vjmhx5do9zn5s0ehhft11vt"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000048-0000-0028-0000-007d0000007b"))), smConvRoleName = (fromJust (parseRoleName "kcc46i5ba8kc_pwcfue5_8wps9wlh9jao62oi9wuwdhtdx2k23utpv3yktasg_053263s8d4sgmk1wthg9ertt7n4o3uazfrhyjnt_boca12jf9fiia3a9oydl"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000058-0000-0048-0000-00370000005b"))), smConvRoleName = (fromJust (parseRoleName "xhcrp0t3b723ioy4f3i9fx7zhk1lx7g6lj8y541xb3d8noaacuyo90x7h35b"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000064-0000-003e-0000-00530000004b"))), smConvRoleName = (fromJust (parseRoleName "ewqeynonh8b5ke2hvk_yujeclz8jubtcxrjr"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000000b-0000-003b-0000-002d00000059"))), smConvRoleName = (fromJust (parseRoleName "_i2ju_svpb21hxx3t75nac0dl0d_qc98lk544px88fuojxbohmm2ngmvr80us9okeq55yee6bl_68k75uxsa1v18c2hywp0okskgbiygzwbt3x3eeghu_qngbzax9mez"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000006-0000-001d-0000-00170000002b"))), smConvRoleName = (fromJust (parseRoleName "1uenq"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000061-0000-0003-0000-004f00000054"))), smConvRoleName = (fromJust (parseRoleName "ytsn1c4u5lzgf2zh7jhx824gubk1mpm_8minz93mrkwq6byepphege_oqb68"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000051-0000-0037-0000-005d00000019"))), smConvRoleName = (fromJust (parseRoleName "ubnx2wfkh59adb1nofiwn6k0n5sswkad0zvl5b7iojad08l2bvlb8cbj0rqli68cp9_ncdg23xhmu"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000058-0000-006b-0000-004f00000038"))), smConvRoleName = (fromJust (parseRoleName "dds3eze96htvu916nq54mk9zzpryrz92s"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000077-0000-006b-0000-005500000068"))), smConvRoleName = (fromJust (parseRoleName "r1uh5ae4hyg2um8aasgm4n9zifn3g1y27pt5qfdkq87cu6gqzpaydp6yqc_2xxb2"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000072-0000-0053-0000-006300000019"))), smConvRoleName = (fromJust (parseRoleName "cy21jvkfvx09w8cd"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000005d-0000-005e-0000-002900000067"))), smConvRoleName = (fromJust (parseRoleName "h7o7zvmeimtir9j2qmrp16vm4v_nluike6aekk1vob23bvn1ev6rvzo32x5r37hi7a41fkbqesmjk81g_uwa44dgs26l6o5pj4_dpg3nr0x7"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000018-0000-0033-0000-00390000000b"))), smConvRoleName = (fromJust (parseRoleName "jseqcxxyifbia2r41c5we6re9ydlc25ax0mq0d1uzuf05tjn"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000010-0000-006a-0000-00500000007b"))), smConvRoleName = (fromJust (parseRoleName "d9undtkcc_7vn5knu4eyqaeynw16tnp0cgcxlyhm3e7jkt78jg1r4u4a3tjzbwzhynt"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000056-0000-004e-0000-003800000030"))), smConvRoleName = (fromJust (parseRoleName "j_s5g9nq2wntp_kmqk2u0uf92yr8eugkxwkqcpz_amhvondpx0mnkz44zrnfa8j_76eaeq"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000004c-0000-0055-0000-005800000040"))), smConvRoleName = (fromJust (parseRoleName "_4807e6xcuke849kdkd6xvmy7pzf8j1v85aen5faqizn28ivz10imsgtw735iqloonl81"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000004b-0000-004e-0000-004900000018"))), smConvRoleName = (fromJust (parseRoleName "ulcnzek"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000000b-0000-0033-0000-004e00000023"))), smConvRoleName = (fromJust (parseRoleName "an5dwdyfckrt03n54csf4"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000056-0000-007d-0000-00510000005c"))), smConvRoleName = (fromJust (parseRoleName "pwkbo6vysbxzc4kr1ctd2kia10yvcyyk9ypltp2fujyqljto97ayhapmrrbf0snqvhpfhbzc"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000001-0000-003e-0000-00360000000e"))), smConvRoleName = (fromJust (parseRoleName "_avue179fys01sfwx2_q12_a6awtyg22k7bt5_04lyfnm2f0vrjz3rd"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000024-0000-0024-0000-001300000065"))), smConvRoleName = (fromJust (parseRoleName "_z9pl3ko5umss09aahjbx5nnw6l23y3ct_cb7d1jdphsw0fk3j7xnkxitrt6o8ak7s_rxyd8lbf9rkxhm455hvsh997sszjwy6o93vd"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000011-0000-001f-0000-00640000001a"))), smConvRoleName = (fromJust (parseRoleName "d0s6eud3ctfrqazwoi1omm9i0p6t8fpc8kxteu4hcie9kk85yrz_8p0dbxm5tv"))}]}
testObject_SimpleMembers_user_4 :: SimpleMembers
testObject_SimpleMembers_user_4 = SimpleMembers {mMembers = [SimpleMember {smId = (Id (fromJust (UUID.fromString "00000069-0000-005b-0000-004f00000060"))), smConvRoleName = (fromJust (parseRoleName "42r5ph5r2sza14l_7o_4sm1390w5mka4"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000061-0000-0000-0000-002f00000076"))), smConvRoleName = (fromJust (parseRoleName "2fiy8kh7a_28o7_civrh6_o539_hwvv5tidcttc88dwbou30xdb7v4pcmp84glcpurh85pydg_"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000038-0000-002f-0000-006a00000057"))), smConvRoleName = (fromJust (parseRoleName "s7pznrhqiao44igho38cx0e6uy4ay9ll0nrt9xyr1ek3grjeujd3qq687nck595bawcpicr8gi0p55zgfehqykv4fq5xueu15m30syivw32hbln0s5pf8"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000070-0000-000e-0000-007f00000008"))), smConvRoleName = (fromJust (parseRoleName "ftgu4z59jggnrpvhh_icjmtzk_n5bptqvqerg0ajjsi9vvfjsznuxkkk"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000041-0000-0007-0000-007e00000015"))), smConvRoleName = (fromJust (parseRoleName "kupmtu3mx864qhut4zzdimakg3vxpib0y6pdv3mpo61et5wfaayq3numq17"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000071-0000-0074-0000-007500000039"))), smConvRoleName = (fromJust (parseRoleName "qn089lff4pl4u"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000001a-0000-005a-0000-00710000005d"))), smConvRoleName = (fromJust (parseRoleName "u1zispgo2twp51z2klw8o7txbvz7cbkrwff3m830grlvw_q"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000008-0000-000c-0000-00180000003d"))), smConvRoleName = (fromJust (parseRoleName "w9a5iflb2inbk3pzvx9k26ylw6xwimqyjioeiov8objv12v0cba6ctbwfpfql2a28gk3kpr5pq_yy43mri9sbobc"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000063-0000-005d-0000-000900000024"))), smConvRoleName = (fromJust (parseRoleName "hg8rjl472iemgj0qi7yw8sqwmpjfrl51ijsxrj911v2u0mmdn5sr"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000004d-0000-001a-0000-007300000050"))), smConvRoleName = (fromJust (parseRoleName "6co4u4e912a9rvisjvb3274vh9u4tkvnuuldrvzxnj"))}]}
testObject_SimpleMembers_user_5 :: SimpleMembers
testObject_SimpleMembers_user_5 = SimpleMembers {mMembers = [SimpleMember {smId = (Id (fromJust (UUID.fromString "00000022-0000-0069-0000-002c00000024"))), smConvRoleName = (fromJust (parseRoleName "cmnf8u_fzunqb3qlv9qtnaymvc"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000055-0000-004a-0000-00190000004e"))), smConvRoleName = (fromJust (parseRoleName "phjb2vcav9jb8zc7jezvh26hfj56x5gf748j8w0xnotk_yj4o6e1ln5wnxrwwxxhpnqgnbmktr4e7zoj"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000007e-0000-0040-0000-002200000060"))), smConvRoleName = (fromJust (parseRoleName "variy2qpwv5hdc_y6z5mfvm56dtmwz43ravwmv0qw0q7fn3tq8e00r00e90u7yl772ekbpc87ie"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000000f-0000-006c-0000-00150000001f"))), smConvRoleName = (fromJust (parseRoleName "1bmq4x24m2zj_x4cnl4cltj8sqiul8gvsf3c8ksswa_7qwpc0l5n30syzbysn8rugkgjw"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000051-0000-0060-0000-004400000073"))), smConvRoleName = (fromJust (parseRoleName "y_nodxnnsdcx4k77c7yi"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000079-0000-000a-0000-002d00000065"))), smConvRoleName = (fromJust (parseRoleName "vwc7py7fxxkrzcv1h5ldh45o6q3qvztam6d82ybaz1qirrjod3t3piw8a2kxmy8krjw7lx4qjiv1_m0aygb18zst1b6io2xve"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000003c-0000-004d-0000-006200000036"))), smConvRoleName = (fromJust (parseRoleName "648an6newe9sqh4xzy2d6wb6t_4lnt7x2rhxfyb_g4wywi_k0npf0yjy0cyhu966i7602dqo2vy6e7yl5uhhia6d36neg5z07z5so6rvhq94mzhcqgbropad4b943v3"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000027-0000-003c-0000-007f0000001f"))), smConvRoleName = (fromJust (parseRoleName "44p3y5luzlhk3_kqxljzlqr6rkljqv7jww803h1v6clv6taw_6n48eatt2n5rpxvhlac_r7u390svzyngz5s4p25w3uygjy7jy_6hn32xuj3jbl8zjy5cft6xlsa"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000023-0000-005a-0000-007f00000020"))), smConvRoleName = (fromJust (parseRoleName "2w5rcxyyncf5jg89gowoodilb8iaav160nujajibnt98vp6gdywn05pnx82xs0q8jgtg4pon6exrn1amydhuk_tg9c53o1hpi1kfci92e7gaotlqpstbwci6cgh8_5u2"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000030-0000-0010-0000-001d00000045"))), smConvRoleName = (fromJust (parseRoleName "8nsffu9e8mr_6o7b4548kjz690r4_q4cyc6"))}]}
