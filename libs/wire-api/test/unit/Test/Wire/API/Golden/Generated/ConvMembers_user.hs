{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ConvMembers_user where

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
testObject_ConvMembers_1 :: ConvMembers
testObject_ConvMembers_1 = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), memService = Nothing, memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}), memOtrMutedRef = Just "", memOtrArchived = False, memOtrArchivedRef = Just "", memHidden = False, memHiddenRef = Nothing, memConvRoleName = (fromJust (parseRoleName "93bo5i"))}, cmOthers = [OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "huqqfil8c4dd92mpcj2awleqwddvi2yvyz"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))}), omConvRoleName = (fromJust (parseRoleName "fjxpiklqofg4229cxuz_oqiqkdf0nlgfqghiadz2xmuvtyvr00agmzi2hpdedo2ukktrt"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))}), omConvRoleName = (fromJust (parseRoleName "makxv7w70i5dc12coedmislb42cm7o4hky4cvtd14cbctxnfif3s53odgttvo123al_4bfoq5mx_4qombn2oh3mg808htqyjma7m4okhxbqfoxm2_9egn9_gu490hbc"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))}), omConvRoleName = (fromJust (parseRoleName "uog9tpnx3cxlknrjrcrrh0d3a4hq74zz33"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "py34wl4q8nr7ewl7nhqkvn04vl7iti79eongg2y_kqx65zgh_ihlrorepyegx9cfquzw75m03n8mk"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))}), omConvRoleName = (fromJust (parseRoleName "t515kpszxbdxilxsibav567uaxts3o80w6z2gongo89obfd6ixeu8m0j_9rvl34pleg_"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))}), omConvRoleName = (fromJust (parseRoleName "waz3zohg33lb_e537_osu5xfu2rakhihcc"))}]}
testObject_ConvMembers_2 :: ConvMembers
testObject_ConvMembers_2 = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), memService = Nothing, memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}), memOtrMutedRef = Just "\57823", memOtrArchived = False, memOtrArchivedRef = Just "", memHidden = True, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "isrexzaqklgvokzia_154cqicaet3x5zu9fxd6eex9wds04umn41tug01eztclmourrc4228xlpg1s1i31bvt8"))}, cmOthers = [OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "0uspxo3xuok7nnolidho5rqdh56apnwopruh45mhs_gyg68rv6ebems_3lrp8e8b72twatsbbvdotc9w15gk5bj5n36pjf62_r3f4vyr2jqg08m_j1wy2i_r43k78"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))}), omConvRoleName = (fromJust (parseRoleName "r8g2bydjmkvydcieaojjnyl7mhts2_q46h9ili2unpk0580aka3qhgm1f7midl"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))}), omConvRoleName = (fromJust (parseRoleName "dlfext83zernhotoaac6a98v59r814vmlsuecq"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))}), omConvRoleName = (fromJust (parseRoleName "yd4mk5cb88_w5of_f8ljfzwifvqucq4q_icbflm46fatoy2uanc7bkefv_glvrec50uy7_lgg4t8b"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))}), omConvRoleName = (fromJust (parseRoleName "4g68tgbcma1q52kqu4cbnwsxqucmyci_r4mpbq8qqi0l27nsdo7l0sfjxdcq0vb0bofv6l5zb39ldyv0y17h33t38fb"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "wr5nox8gzblg8700xw3h10as0lxho_bv3mu3iiovbafshxfkco1z67qnxw_jajjx_bdj0qqvjq_394jz88899kdfdqvzbbdtv4ntewz3o982e3ipu43mw1eu4pf"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))}), omConvRoleName = (fromJust (parseRoleName "8cgzulafqkynlur4mf8rosozyfiemfo5rp"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "cx8azf3x13ps8awil1i5_pk6mc_"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "yjkmq1z"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))}), omConvRoleName = (fromJust (parseRoleName "1q7ehvf2bexraw5ix3aq6g1xsixafwu54kz37r6e"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "mu27ntsi9abjxzk6dus0iz0pbvs3y_zpadayw"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))}), omConvRoleName = (fromJust (parseRoleName "g5j98h7b2jbg2tl3kij7rypckwlkc"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))}), omConvRoleName = (fromJust (parseRoleName "q6n3rmn3nmtvz2vjx0_r6xro0vz5xi29eti6zc2mm95z9v1lgndvl7_jjv14md9id0pjx5k3t50innixac0p_ih8h_4ewsnbx"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "kt8qk2fa6obk33evrtx0t_t63oopu06t1khjlini62n9mbi0g4w97j8ocbmsor331uxoid2e1_l4tkhi5xy4f_qpmgj_mx9v22nzilso3unbqio2xpos1pfsgyyes6"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))}), omConvRoleName = (fromJust (parseRoleName "ij23p252a8nixixc6_3xzs9ejjpi0pd1mwthwtswinu1tmo4rfcnyr38a5rj4r"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "fb9wsz35dh_pvz9uhjzrxjqd7_pxfndvjuh86gal8_af46fcv3t77fk06979xubn598y54u4mpre3i3uc7_lek7qm2zbum5ij4lbpn0d0w23eujdyo94_kxix2"))}]}
testObject_ConvMembers_3 :: ConvMembers
testObject_ConvMembers_3 = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))}), memOtrMuted = False, memOtrMutedStatus = Nothing, memOtrMutedRef = Nothing, memOtrArchived = True, memOtrArchivedRef = Just "\US", memHidden = False, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "7mecui8sp25a08luds_q8uqq9s"))}, cmOthers = [OtherMember {omId = (Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000200000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000002"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000002")))}), omConvRoleName = (fromJust (parseRoleName "f55"))}]}
testObject_ConvMembers_4 :: ConvMembers
testObject_ConvMembers_4 = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), memService = Nothing, memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), memOtrMutedRef = Just "", memOtrArchived = False, memOtrArchivedRef = Just "\EM", memHidden = True, memHiddenRef = Just "\NAK", memConvRoleName = (fromJust (parseRoleName "31l7rt8025xiglz_h7lx_tlie4180fzz67i8h1oeb8qrkypqkm39tnu32dthizyprx2lbioggga1adhh_0d6u5rsx024xbk7jyw2obzps3"))}, cmOthers = []}
testObject_ConvMembers_5 :: ConvMembers
testObject_ConvMembers_5 = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), memOtrMutedRef = Just "\DEL", memOtrArchived = False, memOtrArchivedRef = Just "\DC3", memHidden = True, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "igskvu_l7518ljxlv80hkatkmw3d01ekjw4h_a37yatl4kpoum2kng072l_rzh708fb7ynmnh5uymmhnsiojnrrwkb5c4ps29ku5mu47bx7t_u9f6eiu5jg6w21"))}, cmOthers = [OtherMember {omId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000000"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "awt7jv"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000002"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))}), omConvRoleName = (fromJust (parseRoleName "0ssmosh2479wwgv4qhbkftys8dxhgom5nfu_2kw1jchie8rgn__xviipuftgg1_06zs_w90jsdht8r3emmmhrig2b508kdehe22x9uijz_d"))}]}
