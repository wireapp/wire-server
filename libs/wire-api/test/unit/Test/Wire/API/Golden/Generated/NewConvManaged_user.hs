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
testObject_NewConvManaged_user_1 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))),(Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000000")))], newConvName = Nothing, newConvAccess = Set.fromList [InviteAccess], newConvAccessRole = Nothing, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 6454906086662448}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 3}), newConvUsersRole = (fromJust (parseRoleName "u3dkt3xgy61cb1d6kkjw3db1a9410_o4oeeb1bu596dp0p5o55bc1zge"))})
testObject_NewConvManaged_user_2 :: NewConvManaged
testObject_NewConvManaged_user_2 = NewConvManaged (NewConv {newConvUsers = [], newConvName = Just "Y`", newConvAccess = Set.fromList [], newConvAccessRole = Nothing, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), cnvManaged = True}), newConvMessageTimer = Nothing, newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 2}), newConvUsersRole = (fromJust (parseRoleName "oq_bxax0cdkbfrgo1o9ukylijqzk2uvuq6c1yfd30k5wy54"))})
testObject_NewConvManaged_user_3 :: NewConvManaged
testObject_NewConvManaged_user_3 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))], newConvName = Just "\SYN\NUL1", newConvAccess = Set.fromList [PrivateAccess,InviteAccess], newConvAccessRole = Just NonActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 6497735039165266}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 0}), newConvUsersRole = (fromJust (parseRoleName "ey56iceqr1yd7ba"))})
testObject_NewConvManaged_user_4 :: NewConvManaged
testObject_NewConvManaged_user_4 = NewConvManaged (NewConv {newConvUsers = [], newConvName = Just "\158047", newConvAccess = Set.fromList [], newConvAccessRole = Just NonActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 5964003076393280}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 0}), newConvUsersRole = (fromJust (parseRoleName "mvc5tn76v8p07smdfxo2jbzy8uzgdua6_gko19docqf1sxn0r8ohzk1dzpwxl9novpsqfuv8svwt7un7sja0slp6375kqa91fxdcw1rnqw5m50o4ej"))})
testObject_NewConvManaged_user_5 :: NewConvManaged
testObject_NewConvManaged_user_5 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))], newConvName = Just "", newConvAccess = Set.fromList [InviteAccess,CodeAccess], newConvAccessRole = Just PrivateAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 2352747553451315}), newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "l1rfny3rmsu5rms3zfeqy3acv10wlyfhre36oawyfu11ov66v1_my"))})
testObject_NewConvManaged_user_6 :: NewConvManaged
testObject_NewConvManaged_user_6 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000000")))], newConvName = Just "\46973\SUB", newConvAccess = Set.fromList [CodeAccess], newConvAccessRole = Just ActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000000"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 3539805289186236}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 4}), newConvUsersRole = (fromJust (parseRoleName "oerdf265r5fd1qfhtonhrywczfwq98zelv2pkfjpx8_q2y5_32veo420wsl6j_94iokez2vacz"))})
testObject_NewConvManaged_user_7 :: NewConvManaged
testObject_NewConvManaged_user_7 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000001")))], newConvName = Just "", newConvAccess = Set.fromList [InviteAccess], newConvAccessRole = Just PrivateAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 5325987716507013}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 2}), newConvUsersRole = (fromJust (parseRoleName "lci84jp1qylugsr7oya8e75hr3ahx2xqeq5ytnxn4mi5ixlpls64fkf23c5yqwgngn6yimwnqwut1hi1v_wz_j815dhphw77d044fcx"))})
testObject_NewConvManaged_user_8 :: NewConvManaged
testObject_NewConvManaged_user_8 = NewConvManaged (NewConv {newConvUsers = [], newConvName = Nothing, newConvAccess = Set.fromList [InviteAccess,CodeAccess], newConvAccessRole = Just TeamAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 5949103528543497}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -1}), newConvUsersRole = (fromJust (parseRoleName "3eiyqjbxve3vn2yq_nhnmrle5p0bx0ix_drku2d5c2vsbedfm5om5w7i60cpy3oo56pcrt21th9o"))})
testObject_NewConvManaged_user_9 :: NewConvManaged
testObject_NewConvManaged_user_9 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))], newConvName = Just "S", newConvAccess = Set.fromList [PrivateAccess,InviteAccess,LinkAccess], newConvAccessRole = Just ActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 3590449206374938}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -2}), newConvUsersRole = (fromJust (parseRoleName "eojta1m9fz81qy83itg25hq1er4265nppunz6s8eoogvb_q83ilu90qks4dr2pzk83olgv0qd8xmfu07vrh2xo48teabflh96rx9dtnw8lvkny"))})
testObject_NewConvManaged_user_10 :: NewConvManaged
testObject_NewConvManaged_user_10 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))], newConvName = Just "\93804", newConvAccess = Set.fromList [InviteAccess,LinkAccess], newConvAccessRole = Just ActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000000"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 2689247541157094}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 0}), newConvUsersRole = (fromJust (parseRoleName "3x3ezn70k78egrju7a5ln0_0c97rb6i74lx1q6l1w2r2d70no7ib31"))})
testObject_NewConvManaged_user_11 :: NewConvManaged
testObject_NewConvManaged_user_11 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))], newConvName = Just "Iz\DC3", newConvAccess = Set.fromList [PrivateAccess], newConvAccessRole = Nothing, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 3590137126943037}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -2}), newConvUsersRole = (fromJust (parseRoleName "akyiqkszgqwt_xtflppg06sxvj8ng2mou3dge1bxbehtih80qo39qepmdr2nut65m"))})
testObject_NewConvManaged_user_12 :: NewConvManaged
testObject_NewConvManaged_user_12 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000002")))], newConvName = Just "\127802\a", newConvAccess = Set.fromList [PrivateAccess,CodeAccess], newConvAccessRole = Nothing, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), cnvManaged = True}), newConvMessageTimer = Nothing, newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "45_q06a4uiefh6dy518ul"))})
testObject_NewConvManaged_user_13 :: NewConvManaged
testObject_NewConvManaged_user_13 = NewConvManaged (NewConv {newConvUsers = [], newConvName = Just "", newConvAccess = Set.fromList [], newConvAccessRole = Just PrivateAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 8764041080049016}), newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "r866xsco7xchuawa_hn9wyjdzcpn4cjd2hi711h4nol79fdgj981x9p37sf2q"))})
testObject_NewConvManaged_user_14 :: NewConvManaged
testObject_NewConvManaged_user_14 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000000")))], newConvName = Just "\CAN\DC3", newConvAccess = Set.fromList [LinkAccess], newConvAccessRole = Just ActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), cnvManaged = True}), newConvMessageTimer = Nothing, newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 1}), newConvUsersRole = (fromJust (parseRoleName "koe3utgyuvcn6_"))})
testObject_NewConvManaged_user_15 :: NewConvManaged
testObject_NewConvManaged_user_15 = NewConvManaged (NewConv {newConvUsers = [], newConvName = Just "(D<", newConvAccess = Set.fromList [CodeAccess], newConvAccessRole = Just ActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), cnvManaged = True}), newConvMessageTimer = Nothing, newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 1}), newConvUsersRole = (fromJust (parseRoleName "ez2w8wckxyusfsprb_29i68b9n6aje4d8rm7g_cxz_odnkdcvlv8qln1k4dzxr55t017wp9c7bth1"))})
testObject_NewConvManaged_user_16 :: NewConvManaged
testObject_NewConvManaged_user_16 = NewConvManaged (NewConv {newConvUsers = [], newConvName = Just "'", newConvAccess = Set.fromList [InviteAccess], newConvAccessRole = Nothing, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 1303153896219673}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 0}), newConvUsersRole = (fromJust (parseRoleName "cszi506t_9u9mf_a49kute_1x66gn2a00whs1pxk03pud_655sdskt7"))})
testObject_NewConvManaged_user_17 :: NewConvManaged
testObject_NewConvManaged_user_17 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))], newConvName = Nothing, newConvAccess = Set.fromList [InviteAccess,LinkAccess,CodeAccess], newConvAccessRole = Nothing, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000002"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 7172995940485927}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 2}), newConvUsersRole = (fromJust (parseRoleName "oyy7_ol7zbaq2guovjhajpxrju5nrxdis17ta7prbjkg8m27mugedq88l0m7pwc3szdjki31wz_88h013hz5t4pog5vhpcfkte8_gdfdlt"))})
testObject_NewConvManaged_user_18 :: NewConvManaged
testObject_NewConvManaged_user_18 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000300000003")))], newConvName = Just "at", newConvAccess = Set.fromList [], newConvAccessRole = Nothing, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000002"))), cnvManaged = True}), newConvMessageTimer = Nothing, newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 1}), newConvUsersRole = (fromJust (parseRoleName "2ivdpc5ug0pt196gd23l41546szt1qpzr50knt8ac60xc7cj2748p50m9w8s2gv8jn9ete_aofzmph2am6t6kr8j8m"))})
testObject_NewConvManaged_user_19 :: NewConvManaged
testObject_NewConvManaged_user_19 = NewConvManaged (NewConv {newConvUsers = [], newConvName = Just "o?\NUL", newConvAccess = Set.fromList [CodeAccess], newConvAccessRole = Just TeamAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000000"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 3000435218834332}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 1}), newConvUsersRole = (fromJust (parseRoleName "wnbm57lxgkzobooh9vmw5e2wskjry9dc650uphxcvcvpl0lf9n8_2w6jf94mrvw_vf3iveu4q81ds63z9q4x2consxm027"))})
testObject_NewConvManaged_user_20 :: NewConvManaged
testObject_NewConvManaged_user_20 = NewConvManaged (NewConv {newConvUsers = [], newConvName = Just "F\1001349+", newConvAccess = Set.fromList [PrivateAccess,CodeAccess], newConvAccessRole = Just TeamAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 3838488971930618}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -1}), newConvUsersRole = (fromJust (parseRoleName "cs5qoc_9hceaziqtn1verga8q8fpm6qmo3ilmuhthipopgko"))})
