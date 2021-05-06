{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Member_user where

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
testObject_Member_user_1 :: Member
testObject_Member_user_1 = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))}), memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}), memOtrMutedRef = Nothing, memOtrArchived = True, memOtrArchivedRef = Just "", memHidden = True, memHiddenRef = Just "=", memConvRoleName = (fromJust (parseRoleName "g481movzy5ct6y8kqeoc9wry0j074nbrdnlra53h1fc7tao9a8ypf0vtlo9qcn8osy46g78ix"))}
testObject_Member_user_2 :: Member
testObject_Member_user_2 = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))}), memOtrMuted = True, memOtrMutedStatus = Nothing, memOtrMutedRef = Nothing, memOtrArchived = False, memOtrArchivedRef = Just "Lt", memHidden = False, memHiddenRef = Nothing, memConvRoleName = (fromJust (parseRoleName "s19hz6w32shub_bj7vnfg3c4jfui_vu2do9zq0etdhsy3iq3sqbn3lrgmntjss2jr0exf0hsa0j0_ygn69ckp4w3dqn207xuiiunjbfc2af_g"))}
testObject_Member_user_3 :: Member
testObject_Member_user_3 = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))}), memOtrMuted = False, memOtrMutedStatus = Nothing, memOtrMutedRef = Just "+)", memOtrArchived = True, memOtrArchivedRef = Just "\1013733", memHidden = True, memHiddenRef = Just "zn6", memConvRoleName = (fromJust (parseRoleName "50s0_i_4p3mwc3u8ejsvzsv3209hniqiph5l1u0rll7i0ffsbor8arsr9jh4iitwc3r2eppmhk9qndmzahsd9dz7079h7pi7"))}
testObject_Member_user_4 :: Member
testObject_Member_user_4 = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000002"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))}), memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}), memOtrMutedRef = Just "", memOtrArchived = True, memOtrArchivedRef = Just "\189507", memHidden = False, memHiddenRef = Just "\161025", memConvRoleName = (fromJust (parseRoleName "_a"))}
testObject_Member_user_5 :: Member
testObject_Member_user_5 = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))}), memOtrMuted = True, memOtrMutedStatus = Nothing, memOtrMutedRef = Just "\SYN\1108884\EOT", memOtrArchived = False, memOtrArchivedRef = Just "v\1108384\38624", memHidden = False, memHiddenRef = Just "f\DC4\1056228", memConvRoleName = (fromJust (parseRoleName "127nao_ma5jup0e555nx1an_w2cg2k332poyts2n7xgjpr_wamo6bh4100mruftgu1gvf63716bc38h9cka2k6n69ar64ua"))}
testObject_Member_user_6 :: Member
testObject_Member_user_6 = Member {memId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 2}), memOtrMutedRef = Just ">", memOtrArchived = True, memOtrArchivedRef = Just "zk\b", memHidden = False, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "znpo1e00daj7457jhlq751hgn6q26mdkomtqv7vf3_j8yxiavunapdk3gnjg44q39rle5dr8ikfcfnonog1elrk3fg9r6qy_97qka6peghlfzikkgbt_o9a_6v20a"))}
testObject_Member_user_7 :: Member
testObject_Member_user_7 = Member {memId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000001"))), memService = Nothing, memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), memOtrMutedRef = Just "W", memOtrArchived = True, memOtrArchivedRef = Just "n", memHidden = False, memHiddenRef = Nothing, memConvRoleName = (fromJust (parseRoleName "5ki_g5m1mx5h4f1amdao2luqsmq9ql2bkfvggon5lh6gxcledibb5dw86_bvehgd4f_kyd01yit3_hhbgtcagc5j2mkw8cms4cth6bb0fjozjgq2c9"))}
testObject_Member_user_8 :: Member
testObject_Member_user_8 = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), memService = Nothing, memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -2}), memOtrMutedRef = Nothing, memOtrArchived = True, memOtrArchivedRef = Nothing, memHidden = False, memHiddenRef = Just "/", memConvRoleName = (fromJust (parseRoleName "qjpo_ohw1w8yx1xpx54gpxenydysgcam4zq0dcqpentsbuu54mzuj11hhnw8km605skr0vdce61mrhio21cq5uhc0e3qw82g09wempg55px16h44sfdlq4alqk10zlrv"))}
testObject_Member_user_9 :: Member
testObject_Member_user_9 = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000002"))), memService = Nothing, memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), memOtrMutedRef = Just "", memOtrArchived = True, memOtrArchivedRef = Just "", memHidden = False, memHiddenRef = Just "a\n", memConvRoleName = (fromJust (parseRoleName "n6"))}
testObject_Member_user_10 :: Member
testObject_Member_user_10 = Member {memId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000000"))), memService = Nothing, memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -2}), memOtrMutedRef = Just "Z", memOtrArchived = True, memOtrArchivedRef = Just "gy", memHidden = True, memHiddenRef = Nothing, memConvRoleName = (fromJust (parseRoleName "wj2o77q59af9e69x2kejm_h632lx4_pyyo7zwotbhqkrupfgngyltsal4z5n89lhlu5pbw953ahak30mha430keyup5jmjy8j3cclgf_zlhvahl9yek"))}
testObject_Member_user_11 :: Member
testObject_Member_user_11 = Member {memId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000001"))), memService = Nothing, memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), memOtrMutedRef = Just "", memOtrArchived = False, memOtrArchivedRef = Just "\1089601f", memHidden = False, memHiddenRef = Nothing, memConvRoleName = (fromJust (parseRoleName "9sjdhvxxt2wa12tplyed6i1suynktlw6yz4lozhozn_p522ntc7nermc9h3ryvpoho2nr30zbdlw1bb62a8_0ep9rkdgjl2blqf3tsvjvm54yepe37"))}
testObject_Member_user_12 :: Member
testObject_Member_user_12 = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000002"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))}), memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -2}), memOtrMutedRef = Just "\\", memOtrArchived = False, memOtrArchivedRef = Just "N\1080538\169052", memHidden = True, memHiddenRef = Just "\1043951", memConvRoleName = (fromJust (parseRoleName "b2qk00yhba9wz60k36j8_4ka3m1ahf83qx7shihzti_w87_h4ifn7oid9dio766ajtku1_6w9"))}
testObject_Member_user_13 :: Member
testObject_Member_user_13 = Member {memId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000002"))), memService = Nothing, memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 2}), memOtrMutedRef = Just "H", memOtrArchived = False, memOtrArchivedRef = Just "\ACK\20995\t", memHidden = False, memHiddenRef = Nothing, memConvRoleName = (fromJust (parseRoleName "apgcvlaz5ugd3jgan8txduc0wzs66je7takwi_s2rps7v97dmca4ak1plj63l4ze_xwc_m53ndh5aldbssx59vlr_gxx1rvf"))}
testObject_Member_user_14 :: Member
testObject_Member_user_14 = Member {memId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000002"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))}), memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -2}), memOtrMutedRef = Nothing, memOtrArchived = True, memOtrArchivedRef = Just "\ENQY\b", memHidden = False, memHiddenRef = Just "s", memConvRoleName = (fromJust (parseRoleName "hjuypp5br7d3ua_n2"))}
testObject_Member_user_15 :: Member
testObject_Member_user_15 = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000001"))), memService = Nothing, memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}), memOtrMutedRef = Just "\SIw", memOtrArchived = False, memOtrArchivedRef = Just "\110761", memHidden = True, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "4s_ojkybiggiiug0n5or93d8f2h4nljl2wusfmwe2_4q8wsup5fe1lhjjbonif2yakho6vqw_riv3bq5enov7ntn5ydjuvi20t5xsdswtw_61epqdy6vsov"))}
testObject_Member_user_16 :: Member
testObject_Member_user_16 = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000002"))), memService = Nothing, memOtrMuted = True, memOtrMutedStatus = Nothing, memOtrMutedRef = Just "\1071541", memOtrArchived = True, memOtrArchivedRef = Just "", memHidden = True, memHiddenRef = Nothing, memConvRoleName = (fromJust (parseRoleName "qpumyeyruef46irrz2fky0aqk"))}
testObject_Member_user_17 :: Member
testObject_Member_user_17 = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))}), memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), memOtrMutedRef = Just "\b", memOtrArchived = True, memOtrArchivedRef = Just "M\DC3\FS", memHidden = True, memHiddenRef = Just "z0\1068663", memConvRoleName = (fromJust (parseRoleName "4_p99mle5a33iqwsqoosv2rv51l7ttxi5psgvj3x_26p_ds"))}
testObject_Member_user_18 :: Member
testObject_Member_user_18 = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))}), memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}), memOtrMutedRef = Just "", memOtrArchived = True, memOtrArchivedRef = Just "\ACK", memHidden = True, memHiddenRef = Just "\171901q", memConvRoleName = (fromJust (parseRoleName "kpeldwi4vb27xcx7w2sp1roa9e2bl9x23je"))}
testObject_Member_user_19 :: Member
testObject_Member_user_19 = Member {memId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000001"))), memService = Nothing, memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), memOtrMutedRef = Just "", memOtrArchived = False, memOtrArchivedRef = Just "", memHidden = True, memHiddenRef = Just "5,\172857", memConvRoleName = (fromJust (parseRoleName "th3r5ylnvwj1jsz_as69p_dcunv05vai5_ro3rwzkl2r6qea5zmjapgi7jer84ww6yf_76p_2b817o5a"))}
testObject_Member_user_20 :: Member
testObject_Member_user_20 = Member {memId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000002"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))}), memOtrMuted = False, memOtrMutedStatus = Nothing, memOtrMutedRef = Just "\1095658", memOtrArchived = True, memOtrArchivedRef = Just "\DC1W", memHidden = True, memHiddenRef = Just "\SO", memConvRoleName = (fromJust (parseRoleName "g1ltx7724_yu5mj2579m71b_wwoakdbfphgb96gz15vq6nmpu3km7c6wl9jjbkgr2ca5ybozos40bi9ab2d6gvp12lbw9iq4"))}
