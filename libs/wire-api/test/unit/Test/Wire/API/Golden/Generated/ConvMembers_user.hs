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
testObject_ConvMembers_user_1 :: ConvMembers
testObject_ConvMembers_user_1 = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))}), memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}), memOtrMutedRef = Just "", memOtrArchived = True, memOtrArchivedRef = Just "w", memHidden = True, memHiddenRef = Just "*", memConvRoleName = (fromJust (parseRoleName "_aao06p_8s5rqsucx4n6q1q8jw8msddrpcweq6t9_q5lzkk220lc5t00o"))}, cmOthers = [OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000000"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "czr24250mmjusbtqhnudwrjj3c25t1tq_y8b1te4oismr6syhw1of31datpgz6wb"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))}), omConvRoleName = (fromJust (parseRoleName "vbjud1y2zgutrn2j6zep00jepzto2iijn340otyn5j_fhz"))}]}
testObject_ConvMembers_user_2 :: ConvMembers
testObject_ConvMembers_user_2 = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}), memOtrMutedRef = Just "", memOtrArchived = False, memOtrArchivedRef = Just "", memHidden = True, memHiddenRef = Nothing, memConvRoleName = (fromJust (parseRoleName "rottfcd6m8kfbz0oi0ms4_y00pb_g0595kfcwt_iximpnkvzzv9n_e1jdq7q0_f28_l7"))}, cmOthers = []}
testObject_ConvMembers_user_3 :: ConvMembers
testObject_ConvMembers_user_3 = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), memService = Nothing, memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), memOtrMutedRef = Just "", memOtrArchived = True, memOtrArchivedRef = Just "\1036341", memHidden = False, memHiddenRef = Just "\48735", memConvRoleName = (fromJust (parseRoleName "4zu4z8chov9zy8siwc6bl7n1u1x0k"))}, cmOthers = [OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))}), omConvRoleName = (fromJust (parseRoleName "fy_q41sq2xfrzz_c0s21id126ck1l07911j0b5wua81vm60jj2i267pp1tktvch2za1ws10f9s79mwr_3x08mqbnxac7"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))}), omConvRoleName = (fromJust (parseRoleName "164qp3kkwe7cthnr6_pze3awc1mcv_tf9_n0zj9noz9goacitkorgg8q7g_7ltwzv6pa0c"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "vilhde5pmr5rxxu82vqxo6qbw74w63wa_ca0epye8c61n_sujtp0h84gx181"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "hcfvi"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))}), omConvRoleName = (fromJust (parseRoleName "e_hfjteyn11c1ilux_u7ghe3qnigabpqs11d82dl0bbqedul82e_gdk0duzt"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "wnjs4yy8kf1zjfbfjfn9"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))}), omConvRoleName = (fromJust (parseRoleName "c_u0xorzcpv4x8k2ncf2vu1y8cevrlyk0mb4ob1ekz8o3jq"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "9ev5uyzuiptyfz5i8fhyxi5p8eik9jz6u9f67a5y4anm9o7tvyn6qrgb7tfkegd_l0lh6mestodp7564r228hpsrhy6dx"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))}), omConvRoleName = (fromJust (parseRoleName "dl53ezo71q0vkhz8p17lzk9se68844x_dzvoo152cruh0we4ppv94omxin9_8kx"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "v98ziscend9rbz5v1n248tw2bw7tt_jv7d1m41kwane1ddnrg27jo59s18d764snunz2nmze63nsmxejpe7gicnu2_c"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "9z33aq2_eshdjz25xrvkeejou16074_7z1uejfgyuv6sr_7qkeda9nolcfnihxbwthfzup_8_0mpkxh9u9s4btnmc8v59tt0nizyurqk"))}]}
testObject_ConvMembers_user_4 :: ConvMembers
testObject_ConvMembers_user_4 = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))}), memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), memOtrMutedRef = Just "", memOtrArchived = False, memOtrArchivedRef = Just "", memHidden = True, memHiddenRef = Nothing, memConvRoleName = (fromJust (parseRoleName "pabyi4g23zg1vzcwip5rzvzov16szcgvakny6hmtp7j43u3dvuidvxybvsho10u04djp48yizzpshlur15tlhtn5a7pgo2h7xnu43c1o57n"))}, cmOthers = [OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))}), omConvRoleName = (fromJust (parseRoleName "85apsf2xby8o9bnhnkcrsg8nddgg1901fa6opxgdegwl8tgyqx2lillsuurwk2nyds976kwe8neampfgp1yljahg6v75fvoes2t05"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "pws4z5eaxdj3mv3d4b"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))}), omConvRoleName = (fromJust (parseRoleName "t1lojtn8k4ofyu2pj1sxgyzoml6kjo1cvd0wpvt88ykmsux"))}]}
testObject_ConvMembers_user_5 :: ConvMembers
testObject_ConvMembers_user_5 = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), memService = Nothing, memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), memOtrMutedRef = Nothing, memOtrArchived = True, memOtrArchivedRef = Nothing, memHidden = False, memHiddenRef = Just "2", memConvRoleName = (fromJust (parseRoleName "xypbityfqcuxrb27d1yodg527ahql0w2r_pgqj6faj2t5onjm7zuadqp4zoacc45__wpr24f2wy6phgpashiczq_3eoutf8dm1v2p"))}, cmOthers = [OtherMember {omId = (Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000400000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000000")))}), omConvRoleName = (fromJust (parseRoleName "z2yucsyph5ofk6hzr_pe87pom8j71vt_abcf9r7mxm3x4rcanx"))}]}
