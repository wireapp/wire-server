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
testObject_NewConvManaged_user_1 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))], newConvName = Nothing, newConvAccess = Set.fromList [PrivateAccess,LinkAccess], newConvAccessRole = Just PrivateAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 608811123227356}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -1}), newConvUsersRole = (fromJust (parseRoleName "vchmnl4snsp84dcmghcczuic8w1cbctf0pbi2"))})
testObject_NewConvManaged_user_2 :: NewConvManaged
testObject_NewConvManaged_user_2 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))], newConvName = Just "LW", newConvAccess = Set.fromList [], newConvAccessRole = Just TeamAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), cnvManaged = True}), newConvMessageTimer = Nothing, newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 0}), newConvUsersRole = (fromJust (parseRoleName "stpv2xz0ygjk403a8hpc7v33o11fepv_s6tfzo_kq2auxe4b10v3jfq"))})
testObject_NewConvManaged_user_3 :: NewConvManaged
testObject_NewConvManaged_user_3 = NewConvManaged (NewConv {newConvUsers = [], newConvName = Just "", newConvAccess = Set.fromList [InviteAccess], newConvAccessRole = Nothing, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001"))), cnvManaged = True}), newConvMessageTimer = Nothing, newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -4}), newConvUsersRole = (fromJust (parseRoleName "kcqp7h1jnhd6eo7tbikmt7kyqzjg5iq2ubp5qoygbirxbfps70p74injupvexk6oj3vhvip3vz3"))})
testObject_NewConvManaged_user_4 :: NewConvManaged
testObject_NewConvManaged_user_4 = NewConvManaged (NewConv {newConvUsers = [], newConvName = Nothing, newConvAccess = Set.fromList [], newConvAccessRole = Nothing, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 5878711189676248}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -1}), newConvUsersRole = (fromJust (parseRoleName "2r2cmzai4n_ort04l1ugpkpimryk7hulmi1s3or4v2h6iyk4fxi7forclopx8ek88_ehe7kxklf2us_s2ya1zvbhcwpblt_z"))})
testObject_NewConvManaged_user_5 :: NewConvManaged
testObject_NewConvManaged_user_5 = NewConvManaged (NewConv {newConvUsers = [], newConvName = Just "\987331", newConvAccess = Set.fromList [CodeAccess], newConvAccessRole = Just ActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 4367206414080851}), newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "1s62j0abz5yeg_oz0a54olklasoco90a38f5qcty2cr9p8r6brr8s1"))})
testObject_NewConvManaged_user_6 :: NewConvManaged
testObject_NewConvManaged_user_6 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))], newConvName = Just "r^\44743", newConvAccess = Set.fromList [], newConvAccessRole = Just PrivateAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 3516576967500087}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 1}), newConvUsersRole = (fromJust (parseRoleName "s2j6c42vzm3tanab8_dakwc3_j1l21alujmfzl3k10t8hcdivysb1q_s86hhzn8se8v"))})
testObject_NewConvManaged_user_7 :: NewConvManaged
testObject_NewConvManaged_user_7 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))], newConvName = Just "B", newConvAccess = Set.fromList [], newConvAccessRole = Nothing, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 7338294671047748}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 2}), newConvUsersRole = (fromJust (parseRoleName "c4hnznvm_5g23sdkz8tgrk0e5yanhkj6v8ycd1wws8lbpprrmxsxmbou9snnodyyp69zkbyfmx4yd9q_96n38weznjr28zwp6lzudb3z19x0sssfg609_4cicvm"))})
testObject_NewConvManaged_user_8 :: NewConvManaged
testObject_NewConvManaged_user_8 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))], newConvName = Nothing, newConvAccess = Set.fromList [PrivateAccess], newConvAccessRole = Just PrivateAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 4923157204457194}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 0}), newConvUsersRole = (fromJust (parseRoleName "8vxzyuu9ol0"))})
testObject_NewConvManaged_user_9 :: NewConvManaged
testObject_NewConvManaged_user_9 = NewConvManaged (NewConv {newConvUsers = [], newConvName = Nothing, newConvAccess = Set.fromList [CodeAccess], newConvAccessRole = Nothing, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 8852054548029690}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 1}), newConvUsersRole = (fromJust (parseRoleName "l5dv1knbd22g8ahm_b52njujugfje9is4xvcu9y8u4hg64t5acvr1bdf"))})
testObject_NewConvManaged_user_10 :: NewConvManaged
testObject_NewConvManaged_user_10 = NewConvManaged (NewConv {newConvUsers = [], newConvName = Just "", newConvAccess = Set.fromList [PrivateAccess,CodeAccess], newConvAccessRole = Just PrivateAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 580339313874347}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 2}), newConvUsersRole = (fromJust (parseRoleName "ejdulzkowd02crz8_xuxmdqwnfkf30f46l1kkr8bnnlga0ndp2ku_6"))})
testObject_NewConvManaged_user_11 :: NewConvManaged
testObject_NewConvManaged_user_11 = NewConvManaged (NewConv {newConvUsers = [], newConvName = Just "\182346", newConvAccess = Set.fromList [InviteAccess,LinkAccess], newConvAccessRole = Just NonActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 3232969802851260}), newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "w5amfcl8jsyv1pqo44bz5kalh54lguqw_ayqwgsbht47ihi4c4fd3u_wui0wxf_fhk2rv41lswjgoyjy1"))})
testObject_NewConvManaged_user_12 :: NewConvManaged
testObject_NewConvManaged_user_12 = NewConvManaged (NewConv {newConvUsers = [], newConvName = Just "", newConvAccess = Set.fromList [PrivateAccess,InviteAccess], newConvAccessRole = Just PrivateAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000002"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 7612706953257475}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 3}), newConvUsersRole = (fromJust (parseRoleName "tjh7agg1gs5f37b4n_uui3ukux5a9wbx26etn72uqmr2birv2d6p742d8d6v30sqld2xot23ymumi0qbg5ajbjkzy5"))})
testObject_NewConvManaged_user_13 :: NewConvManaged
testObject_NewConvManaged_user_13 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000200000003")))], newConvName = Just "\n\ae", newConvAccess = Set.fromList [PrivateAccess,InviteAccess,LinkAccess], newConvAccessRole = Just TeamAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 5118473174854501}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 2}), newConvUsersRole = (fromJust (parseRoleName "lzd9qm244q7d9t1_g92ayk4cjvy1kx9yseahwgbrcplxkyq3yczliva93ijtjknqznjoc572xx85zxr33s63wzd8waivlbxlgxqfar0_31kph9ljm_t6_raiaka"))})
testObject_NewConvManaged_user_14 :: NewConvManaged
testObject_NewConvManaged_user_14 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000002")))], newConvName = Just "o/J", newConvAccess = Set.fromList [PrivateAccess,InviteAccess,LinkAccess], newConvAccessRole = Just PrivateAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 6962668701915521}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 0}), newConvUsersRole = (fromJust (parseRoleName "hc27q86fputgserq0okv3nsgdtn1dp9shuv6jclmi9gq840i_30l1xxebydw"))})
testObject_NewConvManaged_user_15 :: NewConvManaged
testObject_NewConvManaged_user_15 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))], newConvName = Just "cH", newConvAccess = Set.fromList [], newConvAccessRole = Just ActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000001"))), cnvManaged = True}), newConvMessageTimer = Nothing, newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -1}), newConvUsersRole = (fromJust (parseRoleName "az3ohaymol4oujyp6a5n8ito085n6mux31a"))})
testObject_NewConvManaged_user_16 :: NewConvManaged
testObject_NewConvManaged_user_16 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))], newConvName = Just "", newConvAccess = Set.fromList [InviteAccess,LinkAccess,CodeAccess], newConvAccessRole = Nothing, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000001"))), cnvManaged = True}), newConvMessageTimer = Nothing, newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 4}), newConvUsersRole = (fromJust (parseRoleName "srod66aqtt"))})
testObject_NewConvManaged_user_17 :: NewConvManaged
testObject_NewConvManaged_user_17 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))], newConvName = Just "\174342", newConvAccess = Set.fromList [PrivateAccess,InviteAccess], newConvAccessRole = Just NonActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 6383942798435361}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 0}), newConvUsersRole = (fromJust (parseRoleName "2dmyjwlh90ooj_gh8_z3_1jl4gsi6rl1ozy3pp9wiuckm1et9l83xtcaehr_1p503equzmmznurjb5do46hy_l724sjxrh38qjhgd7swlfzbkbbpg9w"))})
testObject_NewConvManaged_user_18 :: NewConvManaged
testObject_NewConvManaged_user_18 = NewConvManaged (NewConv {newConvUsers = [], newConvName = Just "\1050732", newConvAccess = Set.fromList [InviteAccess,LinkAccess], newConvAccessRole = Just ActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), cnvManaged = True}), newConvMessageTimer = Nothing, newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "01q41_08o"))})
testObject_NewConvManaged_user_19 :: NewConvManaged
testObject_NewConvManaged_user_19 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))], newConvName = Just "", newConvAccess = Set.fromList [], newConvAccessRole = Nothing, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 4403860383417757}), newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "ikho8abe_76s3qw0zsn235usqye"))})
testObject_NewConvManaged_user_20 :: NewConvManaged
testObject_NewConvManaged_user_20 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000002")))], newConvName = Nothing, newConvAccess = Set.fromList [], newConvAccessRole = Just PrivateAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 1289761473879522}), newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "03m8qqpek2w598k98gy7hhmjnxe41zdjrk3uuxiyzmpcmw46235n1gybucvvd2sr2onq315cwk7nzn5tyo_czx2lq1ud_t115jlc"))})
