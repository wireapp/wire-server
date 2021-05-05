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
testObject_SimpleMembers_1 :: SimpleMembers
testObject_SimpleMembers_1 = SimpleMembers {mMembers = [SimpleMember {smId = (Id (fromJust (UUID.fromString "00000052-0000-003a-0000-007400000043"))), smConvRoleName = (fromJust (parseRoleName "jj9dbovj0g163ys0nm2tdbaiw6wvvhex1wegh066u5lezps_xp_9v9ljmrddd_ddkocnldzn3lt9nc"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000007d-0000-0049-0000-003300000057"))), smConvRoleName = (fromJust (parseRoleName "88s_spvw61zicxdcfhm42l4_fe93u55"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000003a-0000-0019-0000-000900000025"))), smConvRoleName = (fromJust (parseRoleName "f4mq6_nwg8b6a5se7uzvwoyw8s"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000049-0000-001c-0000-006a0000001b"))), smConvRoleName = (fromJust (parseRoleName "v6fwzsuw9wtm4wmz936fxlevm291joreqe4d7uhted4s91k0ma1xop36piruppw85mwwod62kawa7pk"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000000c-0000-0041-0000-000600000074"))), smConvRoleName = (fromJust (parseRoleName "z3wbn357gc6533omiuvrllxpl24ydzhqfj90cmlprngvimxs80jgtda70fqrfx20diwc_ej4lnpcqihkot6n3xuvptu76ggou0kewd7"))}]}
testObject_SimpleMembers_2 :: SimpleMembers
testObject_SimpleMembers_2 = SimpleMembers {mMembers = [SimpleMember {smId = (Id (fromJust (UUID.fromString "00000024-0000-0061-0000-004100000013"))), smConvRoleName = (fromJust (parseRoleName "_0zx600p2b04n4talqtf96w_yrwetbvw0jj3y09xf5ue"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000020-0000-0058-0000-00760000003e"))), smConvRoleName = (fromJust (parseRoleName "jy_gqq7qk0j49mdtbu6framynwrxflnkn0nfi2lmg5y4tyq07tjxkws1q0avm598ks"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000040-0000-0015-0000-00260000002a"))), smConvRoleName = (fromJust (parseRoleName "kc9523gkt7o0s2d6j4xw4wb5c2o9pnfl9miihlv2rhqmzv52m_mz0w8hqnrlkjku6pjg1u9ukoxwr5rawetijitfgytq63mwis0a6vi3c0g1bc5voq1oo7tgz_nfs"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000006b-0000-006b-0000-005900000024"))), smConvRoleName = (fromJust (parseRoleName "off3y2q2ks2qud0_gw37i2zpb2xs5umnwpyog47jg2twz0a"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000018-0000-002b-0000-000100000010"))), smConvRoleName = (fromJust (parseRoleName "_7gq1bqboddkubsgpfbl0si0wmyfn7n7_w6g98fu18flw"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000006a-0000-001a-0000-004e00000048"))), smConvRoleName = (fromJust (parseRoleName "o2cdt4whmpwmr49uydd8b5rwgbm66gtgzzgnxwoxbk_9o_rdb3z29xst8x7lzxtsawdhsiclyg66sqrn0wt41unmo"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000006d-0000-005f-0000-005600000032"))), smConvRoleName = (fromJust (parseRoleName "45sdjnlthn23t_cj5frpl4153u7td_539h6ghbt82tmsiqhfg5qpop_sbh86tt3bxlq9g2rcaa3b9l2fs2q1bmjw4syjsk0baysgdtcala"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000004e-0000-0014-0000-008000000036"))), smConvRoleName = (fromJust (parseRoleName "gb4to_gamnl93prb759b79d7jg9wzk5pml1qadij6rp2c6d5cd17846el7wql_xu9rd_jolobadbbhrqme6qy_u77x1mc0d163jp3nrgkkbbc01m9r5l3p1wjvh9h1"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000051-0000-0077-0000-003f00000071"))), smConvRoleName = (fromJust (parseRoleName "ip3bgz055b870w028gt7yds0iznjrc7ak2h0f_ld23ick3dlhd7_wz461bfbgz233rwjr923ftfdwhg704pv72vz_z1lrcvmxuqfjl"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000027-0000-0006-0000-003b0000000f"))), smConvRoleName = (fromJust (parseRoleName "2xtunvwy9rr04dspg5yeddgkro6wesknrxpcy74f5bnltzbwyy0hi1_ng0fjtve85l97bmiswr9gf66hio31kmpb3tyn6jrjk_brd079_8hiftn51"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000004d-0000-006a-0000-003f00000071"))), smConvRoleName = (fromJust (parseRoleName "qyhql3hs0l6_gcdpn2retlv20oxr3rki7mr6fdvq4i2opwbp5xb0oq6hwajiqw05wvxa53enwscxhp0m8j1ml"))}]}
testObject_SimpleMembers_3 :: SimpleMembers
testObject_SimpleMembers_3 = SimpleMembers {mMembers = [SimpleMember {smId = (Id (fromJust (UUID.fromString "0000000d-0000-0043-0000-00250000001a"))), smConvRoleName = (fromJust (parseRoleName "xb7vpmfusabjhua4o9owx9gn_wi4ce81umzt0hqf7t91g4stbmq03zltylxwyd0lc8fe0nfyfppgd6jk"))}]}
testObject_SimpleMembers_4 :: SimpleMembers
testObject_SimpleMembers_4 = SimpleMembers {mMembers = [SimpleMember {smId = (Id (fromJust (UUID.fromString "00000051-0000-002f-0000-007400000064"))), smConvRoleName = (fromJust (parseRoleName "si4fw3df0t__rq3bynd9bc360ddw714uukqfjnle1veiynrdow181ce6oon6a5na6ukw"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000021-0000-0041-0000-00620000007b"))), smConvRoleName = (fromJust (parseRoleName "052kam10m1g8qbzj_mgeru"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000041-0000-0030-0000-006200000045"))), smConvRoleName = (fromJust (parseRoleName "zbw___o9s_303hv8q242vy15oxs9xjwhzriyt2z7mkp15gd2y1skjfjiruzk8cdsyq8kmmuzzswua5hhz7o371hzxojgt9ukmsjmshyfxnd7egy"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000023-0000-005b-0000-003c00000069"))), smConvRoleName = (fromJust (parseRoleName "jwj_sr_b4i8azv1r0tie6sg7wzy3rge7cy6bbyxj7_g9okb9rintxinvgf9pi5si5e_2qpy0gl4c14enmesfyj4ddxvb2"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000070-0000-0008-0000-000d00000038"))), smConvRoleName = (fromJust (parseRoleName "egunttzjpjtuah_f7"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000016-0000-0056-0000-001000000048"))), smConvRoleName = (fromJust (parseRoleName "75l3oadvyttl4_3z8m9lc5od2oo9oca59vvp0fmzsbzj8o1b8wqf1ei68xcrzjiqje96yj1d2amgt_anumwerdo1z65o0x87e6qhu8qk_ew"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000003a-0000-0010-0000-007300000053"))), smConvRoleName = (fromJust (parseRoleName "k_c3mgkb6"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000080-0000-0041-0000-007d00000040"))), smConvRoleName = (fromJust (parseRoleName "pmhqhm2vjljbmpi_ioaeh3ih18qx8g8r8wycpffkuta_7qpau22_uopj7jy66jo8h"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000051-0000-001e-0000-00060000001e"))), smConvRoleName = (fromJust (parseRoleName "jqw2b3qqy0h4up5y8w96_fbb4k_g2v02gva"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000077-0000-0014-0000-007900000064"))), smConvRoleName = (fromJust (parseRoleName "tj3pw4z57popueydrnhcatc5vbs02hz9k9a8_60kz0zz75iuhr4hsd_d_f133opfkdzep0pagwlhlvw5phymh4f3chtkxfsxv6gbsq9x4ab1l"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000001b-0000-002f-0000-000c00000019"))), smConvRoleName = (fromJust (parseRoleName "z58lrk82ka2jons8yq29hda6rh_9fpce7x27xtpbp10auuh_bsyxls_3getx8c270ffj07t83xz5kxww"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000031-0000-003c-0000-002800000014"))), smConvRoleName = (fromJust (parseRoleName "m6xbba6c8a1rk7x_pvvezxa3t06_of2pkfryim_tgurah6njxligmsjnnuowbihni7e7utqt8o8zu2j8qxmbh104ge1rh2gkvqlsbktl8"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000002b-0000-0078-0000-002800000004"))), smConvRoleName = (fromJust (parseRoleName "22p1l20x46qnc2f53s9iqnug7ojfadbru0k0h5rghxlal_us240don7qbhxsp1v"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000037-0000-004f-0000-007500000069"))), smConvRoleName = (fromJust (parseRoleName "vz5ha86nnpdl5myt3"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000057-0000-0017-0000-002900000067"))), smConvRoleName = (fromJust (parseRoleName "f6ee0yxnkoi2bb5caj40sqjcw5g2gnrvqlrabnb9v6wh1fv53e"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000050-0000-0062-0000-003100000036"))), smConvRoleName = (fromJust (parseRoleName "qsh3ploxkwmlv"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000035-0000-0072-0000-003800000047"))), smConvRoleName = (fromJust (parseRoleName "pf5aqz9jmjyp0f_xl5myxp27yhufjf92bsa597kir6uo1qdjbvq45gglh8"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000019-0000-0016-0000-00780000003a"))), smConvRoleName = (fromJust (parseRoleName "tr2r_edpstim5hyxsdtoef3vrp8s6hd1sd_7g1ocqv19o6aqallfdyii7o1cgfipg6494vtys2ll"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000006c-0000-0043-0000-000600000018"))), smConvRoleName = (fromJust (parseRoleName "hluy752wfix6efh77hjkoh0jcl7ij1qbkpw9b9c2ptjgldn67mb8m9dpxk3oa5cd6jyro8pgmnms2f6dhgiezpn311x7ogn"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000005-0000-006a-0000-000d00000040"))), smConvRoleName = (fromJust (parseRoleName "8ox_1xjumq6gny0qbb4"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000060-0000-0022-0000-005f00000076"))), smConvRoleName = (fromJust (parseRoleName "17blryqe8khvdok7ya_u_esfwmi7vxnls87bgz2ghg304cg8frqtnrn_9kfjyzdok9zv2debuzgjs7bdgpft0h7f2"))}]}
testObject_SimpleMembers_5 :: SimpleMembers
testObject_SimpleMembers_5 = SimpleMembers {mMembers = [SimpleMember {smId = (Id (fromJust (UUID.fromString "00000019-0000-002c-0000-004400000019"))), smConvRoleName = (fromJust (parseRoleName "sns3zojzfoa9o53r_otd9tm5il4eyxot31pj_v6y0ito8fjtg_wq9ebmhyrom8ahovnvtb82pnv5w7y"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000063-0000-0062-0000-001a00000048"))), smConvRoleName = (fromJust (parseRoleName "q7jnn1s"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000059-0000-005f-0000-002e0000001e"))), smConvRoleName = (fromJust (parseRoleName "rzb__unf1vjwisf5_xqkwypmk1qq_gh76pj2myi"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000006e-0000-0070-0000-000f00000066"))), smConvRoleName = (fromJust (parseRoleName "fkgpsr3asis_mksz1d94jcr9es8dr4dmzk"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000004e-0000-000c-0000-00710000006c"))), smConvRoleName = (fromJust (parseRoleName "z5ft78xbmnw2fbw83qyzask8gj88cb35z47oqkv8_jbe0bm6aedkv1c4mjlxz83y54lqx4ax551367yk84ca3c6521vt4n43e3lom"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000038-0000-002f-0000-00470000001a"))), smConvRoleName = (fromJust (parseRoleName "0xgojpt_ce6boufiyfc_epyoylcz_ori12asbsdxd8c11dwfqaengdgt0rj86y8u3kx8wylu_bqxm7"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000003c-0000-0062-0000-004d00000070"))), smConvRoleName = (fromJust (parseRoleName "e4fuzjaglh0v173ggeic"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000007-0000-000a-0000-004500000011"))), smConvRoleName = (fromJust (parseRoleName "zge2q1tye_1ajyjb9v2m51jg6"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000003b-0000-0043-0000-007e00000067"))), smConvRoleName = (fromJust (parseRoleName "os0nos_u_gikwzs0096r21habcc2bzrn5hh7n3umnzsndasdwx0r3ktfrotw69lruse4otb7i6vii6ug0eihfah"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000001f-0000-0018-0000-003f00000027"))), smConvRoleName = (fromJust (parseRoleName "khbahb69i755qg5hoc5fkg35dckq3m6mykztl_r_ngnbzqdrz6p5xd2784lgmq46hznt870m50y6lko4ifwjor37wue4q6xloxzpjg0nznzk2jtvlqktysb"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000001d-0000-0013-0000-003500000024"))), smConvRoleName = (fromJust (parseRoleName "5twqxg07tng8uwkrvxv28hsk0vb44nqp67i7ea5n42m8y45tglhhr9nmp2u2d1t0s2f7bjth5yjdwu5tet7pi1nm8leajw8t6"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000052-0000-0040-0000-00610000006f"))), smConvRoleName = (fromJust (parseRoleName "n4wvz0k2u46j1ekzf5uikvo7c"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000044-0000-005a-0000-001d00000057"))), smConvRoleName = (fromJust (parseRoleName "m9oa5cac69niuoz2g1rc3nlnkb2l5kto21i41cq035t_yb1mk7nxex6sh4e4wroatmn6xg3b"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000002c-0000-0033-0000-005600000021"))), smConvRoleName = (fromJust (parseRoleName "nme26s62yd52d4whdss9ycdm9m8rco"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000006a-0000-0019-0000-003a00000077"))), smConvRoleName = (fromJust (parseRoleName "ddswvjfiuwbper1l9ve7tk5kc356iju8mlgab1uk7znq83d6k6pvyn5pgk4aikczha8f_e9chg0bpdek6pi"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000003d-0000-0039-0000-00530000001c"))), smConvRoleName = (fromJust (parseRoleName "h02b1jieckgxvud_nbfmd0y1dfur9qpjvvubatamvlfgeftlff20pl3n1imv"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000002-0000-0045-0000-00640000005f"))), smConvRoleName = (fromJust (parseRoleName "e2cwmzp2hoxttlhqh7l0qfl2ch9fu04k36k3i98e_cgvnmnkbvoxnhzdmk86lsp77qg2xc_w4jzb2xso2e06qs6jvsvtofbtqu75lj7fjt902"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000055-0000-0051-0000-00630000004b"))), smConvRoleName = (fromJust (parseRoleName "9t3xpuuh70ssis"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000028-0000-0011-0000-006700000053"))), smConvRoleName = (fromJust (parseRoleName "xkpxcljs0kxd57xa_za5pulbchj3"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000000d-0000-0060-0000-005e0000003c"))), smConvRoleName = (fromJust (parseRoleName "x54b4m0o7eevbuvgpcwyaut09kozf3nixq74h71ju48c33hpk12298rurfc9_cmj8t65yqcs38ny_3hl02g43jme_t426sw"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000001-0000-007c-0000-000a00000001"))), smConvRoleName = (fromJust (parseRoleName "jf04zw2l_puvd4w10r5b7eh6ywbbaxu5ukxj6pljseyr_nc0_4csmrr2ovk9vvv2050uprea8bnq620zk__hq45iy54ghfcrg8d2hdznsos6fym8g2pfnpgcwmy5v"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000003e-0000-006e-0000-002700000077"))), smConvRoleName = (fromJust (parseRoleName "1qkeh681u0tc83cv7h47sy46x8q2xl7wk8js9i90u6ar8vyr5hcbc8adf5wutx5l6etst3dn6dzxkz5xlcwlsdj08tiqzbfegli_drkh74uzucs_ajzccbi9q"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000006d-0000-002c-0000-000d00000049"))), smConvRoleName = (fromJust (parseRoleName "m3m_pgftnv90z8dt5mmtd1upu0k"))}]}
