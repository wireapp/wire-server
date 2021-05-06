{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.NewConvManaged_user where

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
testObject_NewConvManaged_user_1 :: NewConvManaged
testObject_NewConvManaged_user_1 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))], newConvName = Nothing, newConvAccess = Set.fromList [LinkAccess,CodeAccess], newConvAccessRole = Just TeamAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 2323896433937624}), newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "yvhqq6w8p5_x3chxmsgk3taxj9"))})
testObject_NewConvManaged_user_2 :: NewConvManaged
testObject_NewConvManaged_user_2 = NewConvManaged (NewConv {newConvUsers = [], newConvName = Just "\1074796", newConvAccess = Set.fromList [InviteAccess,LinkAccess], newConvAccessRole = Just PrivateAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), cnvManaged = True}), newConvMessageTimer = Nothing, newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -2}), newConvUsersRole = (fromJust (parseRoleName "lnt_1cqj7bxw7jmtil5ifq5lm810rkof4_oe5g4lvb56wib0mvlf8jkag8thwuie"))})
testObject_NewConvManaged_user_3 :: NewConvManaged
testObject_NewConvManaged_user_3 = NewConvManaged (NewConv {newConvUsers = [], newConvName = Just "", newConvAccess = Set.fromList [], newConvAccessRole = Nothing, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 8549267345424472}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 2}), newConvUsersRole = (fromJust (parseRoleName "e3qwrhpszf2ysqfic8e6w0370f6aon4ofneoefu3k5g6f17m5j6s8nqp_im8eswunsmskxdv"))})
testObject_NewConvManaged_user_4 :: NewConvManaged
testObject_NewConvManaged_user_4 = NewConvManaged (NewConv {newConvUsers = [], newConvName = Just "L\1073073W", newConvAccess = Set.fromList [], newConvAccessRole = Nothing, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 6802155329416750}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 2}), newConvUsersRole = (fromJust (parseRoleName "hr2b62sovw17967fytraaviuktyzpqkuqx955npxa58wkcpr8vqnmpre0pu545qu_mxmuw4atn2sc1o525lzdvnijeogh9bfz9966ib26xakph_4mneu7mrs"))})
testObject_NewConvManaged_user_5 :: NewConvManaged
testObject_NewConvManaged_user_5 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000001")))], newConvName = Nothing, newConvAccess = Set.fromList [PrivateAccess], newConvAccessRole = Just NonActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000000"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 2375333128422231}), newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "k2xkj8uuek6hbuu2yk5f3z69tocnn889h8elgr0ja7ip3tz9qa5yxn70ayu8pak"))})
testObject_NewConvManaged_user_6 :: NewConvManaged
testObject_NewConvManaged_user_6 = NewConvManaged (NewConv {newConvUsers = [], newConvName = Just "", newConvAccess = Set.fromList [LinkAccess], newConvAccessRole = Just ActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 5066085888060426}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 2}), newConvUsersRole = (fromJust (parseRoleName "lurnc_drb_vbq4fvqsgpxpa0j2_fzeetsoyov88itpsol9jdnueqnos3serjisnhhsvswhjc"))})
testObject_NewConvManaged_user_7 :: NewConvManaged
testObject_NewConvManaged_user_7 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))], newConvName = Just "", newConvAccess = Set.fromList [], newConvAccessRole = Just PrivateAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), cnvManaged = True}), newConvMessageTimer = Nothing, newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 0}), newConvUsersRole = (fromJust (parseRoleName "2_0ugiei9wbsoz4q336m054fpr9g1u1vtyg8cn7ycdobkrq_4b1tm2x97fw6i68gi33vy_3zqu3cv9g8do1zuklwcrrkflxlmyildidufkqq80t5k2f49xu8e40uy"))})
testObject_NewConvManaged_user_8 :: NewConvManaged
testObject_NewConvManaged_user_8 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000000"))),(Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000000")))], newConvName = Just "\DC4", newConvAccess = Set.fromList [InviteAccess,CodeAccess], newConvAccessRole = Just ActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000002"))), cnvManaged = True}), newConvMessageTimer = Nothing, newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -3}), newConvUsersRole = (fromJust (parseRoleName "9dalnvmeqh8y8pbltb0nkb0ivh_57tg7udoezuell_0qfy2n2h654qlmjcn_cxqk4fgyucg2topqy4i_qh"))})
testObject_NewConvManaged_user_9 :: NewConvManaged
testObject_NewConvManaged_user_9 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))], newConvName = Just "", newConvAccess = Set.fromList [CodeAccess], newConvAccessRole = Just TeamAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 3290361062363302}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -2}), newConvUsersRole = (fromJust (parseRoleName "e91lwgsnsi7ip_2_pid2dt3ljj408"))})
testObject_NewConvManaged_user_10 :: NewConvManaged
testObject_NewConvManaged_user_10 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))], newConvName = Just "\998517", newConvAccess = Set.fromList [], newConvAccessRole = Nothing, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 6088017802873381}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -1}), newConvUsersRole = (fromJust (parseRoleName "rjvilnaw3z7in3l3_l7p71vb8ohdangmraiaf94eficfk7tv_qttzgs2lo7_mw1xqdab4vsm"))})
testObject_NewConvManaged_user_11 :: NewConvManaged
testObject_NewConvManaged_user_11 = NewConvManaged (NewConv {newConvUsers = [], newConvName = Just "", newConvAccess = Set.fromList [InviteAccess], newConvAccessRole = Just PrivateAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), cnvManaged = True}), newConvMessageTimer = Nothing, newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "bbq336v1lfol48i5ns86xcf1dignq9so8k6e_s0kngc2"))})
testObject_NewConvManaged_user_12 :: NewConvManaged
testObject_NewConvManaged_user_12 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))], newConvName = Nothing, newConvAccess = Set.fromList [], newConvAccessRole = Nothing, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 8625856571390754}), newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "hzberi_2al1neq00itopft92kj170f0lx0klf_3dn5mkhskixuon8oa6yolmr1ov38tzodscj4s547892v9_d8r7tqh84gmbtya18m5ycu60cfxyb09v9_9lcldwz_a"))})
testObject_NewConvManaged_user_13 :: NewConvManaged
testObject_NewConvManaged_user_13 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))], newConvName = Just "\1012198", newConvAccess = Set.fromList [], newConvAccessRole = Just PrivateAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 6818664840881153}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 1}), newConvUsersRole = (fromJust (parseRoleName "r6qmwica4am13_eg8xvx7qoq1c4z36t3dkfcd9jehrkmbtgvnpp6pxmhss_zum7jwf60n5n388wagv0_dp8aar6tng"))})
testObject_NewConvManaged_user_14 :: NewConvManaged
testObject_NewConvManaged_user_14 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))], newConvName = Just "TL\"", newConvAccess = Set.fromList [InviteAccess], newConvAccessRole = Just ActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 2411814737022142}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 3}), newConvUsersRole = (fromJust (parseRoleName "troheunc4cr39u9dektq162i_dl5ohbmoyhuxutdqd4x2ojhrjy0uosnxes565pcbjq8v82css74cdw0y8y289td10x8qp4bfyai7"))})
testObject_NewConvManaged_user_15 :: NewConvManaged
testObject_NewConvManaged_user_15 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))], newConvName = Just "", newConvAccess = Set.fromList [LinkAccess], newConvAccessRole = Nothing, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 4864784093412799}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -1}), newConvUsersRole = (fromJust (parseRoleName "9zb5jb"))})
testObject_NewConvManaged_user_16 :: NewConvManaged
testObject_NewConvManaged_user_16 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))], newConvName = Just "]zmg", newConvAccess = Set.fromList [PrivateAccess,LinkAccess,CodeAccess], newConvAccessRole = Just PrivateAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000000"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 2564381408289588}), newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "cmu08bv4agoxvlv4mih4rbfkdscz67e0pstb9tofmbjfdtdhw"))})
testObject_NewConvManaged_user_17 :: NewConvManaged
testObject_NewConvManaged_user_17 = NewConvManaged (NewConv {newConvUsers = [], newConvName = Nothing, newConvAccess = Set.fromList [InviteAccess,LinkAccess], newConvAccessRole = Just ActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000001"))), cnvManaged = True}), newConvMessageTimer = Nothing, newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 4}), newConvUsersRole = (fromJust (parseRoleName "lz12f664_8avktu72mwrlgwnwhh4o39997w4wj9a704cslgzetytf3t2k8vp_4dstf3k9tlz_fw6wyhd9v630py2a5oxxfqhc1y983bumjlfn32ty5rq8jw28lnirt78"))})
testObject_NewConvManaged_user_18 :: NewConvManaged
testObject_NewConvManaged_user_18 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000001")))], newConvName = Just "x\SYN\74099", newConvAccess = Set.fromList [PrivateAccess,InviteAccess,CodeAccess], newConvAccessRole = Nothing, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 6721126955707954}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -2}), newConvUsersRole = (fromJust (parseRoleName "gc1329oe0i583q3fw5yq9x62844pjcbo4wvkhzsdd8cbvoj0f34ekbnv1dacg1h_yw7i8yfpzx9fev8dj8y6jr5wv7dn3hqeumcazei8l9f5k0p9972hwey14n27n1"))})
testObject_NewConvManaged_user_19 :: NewConvManaged
testObject_NewConvManaged_user_19 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))], newConvName = Just "", newConvAccess = Set.fromList [], newConvAccessRole = Nothing, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 7233159854885110}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -1}), newConvUsersRole = (fromJust (parseRoleName "oumix1vr3x3qr1f53lht7izcifhx4yhpp6vn96_1jgxp6h4v7v7i02qud7ljbone"))})
testObject_NewConvManaged_user_20 :: NewConvManaged
testObject_NewConvManaged_user_20 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000100000003")))], newConvName = Just "\10411", newConvAccess = Set.fromList [PrivateAccess,InviteAccess,LinkAccess,CodeAccess], newConvAccessRole = Just ActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), cnvManaged = True}), newConvMessageTimer = Nothing, newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 1}), newConvUsersRole = (fromJust (parseRoleName "3e5idyb5vfsxq4zav_446sn78sahxnrxy9xjlviaihxeyqbmf435g6ukghxj1f03xi7s1hqql85rp__2hbibg4cxwft08m1jkkdzsgf3"))})
