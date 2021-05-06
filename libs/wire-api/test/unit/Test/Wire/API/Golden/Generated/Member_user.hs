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
testObject_Member_user_1 = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000001"))), memService = Nothing, memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}), memOtrMutedRef = Just "", memOtrArchived = False, memOtrArchivedRef = Nothing, memHidden = True, memHiddenRef = Just "6q#", memConvRoleName = (fromJust (parseRoleName "8b09lp90a43ps2ha9hfe845_s3_2wxb6a47fyp1hqnw4ig71qq0jds2ri_ouw"))}
testObject_Member_user_2 :: Member
testObject_Member_user_2 = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))}), memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), memOtrMutedRef = Just "r", memOtrArchived = True, memOtrArchivedRef = Nothing, memHidden = False, memHiddenRef = Just "\r\179238f", memConvRoleName = (fromJust (parseRoleName "5m5_el0uba1p_7rlig4kd_xj0nk3e_7i5g4uxftr1siggkmazkmnc91kmu8oxg6si5zeraxgl1wvo25q4zdrb8h"))}
testObject_Member_user_3 :: Member
testObject_Member_user_3 = Member {memId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000000"))), memService = Nothing, memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -2}), memOtrMutedRef = Just "3h", memOtrArchived = False, memOtrArchivedRef = Nothing, memHidden = True, memHiddenRef = Just "\n", memConvRoleName = (fromJust (parseRoleName "nln2809gdtj61mckaeyqrwy04z_61727qando14izvtzoswinkrc60z0we8p3wpd6j2cbuc9v49ea7i187emvpb633jya_pchv6y53097owdzirwh9_cfnbh3havce"))}
testObject_Member_user_4 :: Member
testObject_Member_user_4 = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))}), memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), memOtrMutedRef = Just "\1085599", memOtrArchived = True, memOtrArchivedRef = Just "", memHidden = False, memHiddenRef = Nothing, memConvRoleName = (fromJust (parseRoleName "t4sj5c0fpu4lh018r5x2xs6n8vu4vfjqpek15lcwuq4"))}
testObject_Member_user_5 :: Member
testObject_Member_user_5 = Member {memId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000001"))), memService = Nothing, memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), memOtrMutedRef = Just "W)1", memOtrArchived = True, memOtrArchivedRef = Just "", memHidden = True, memHiddenRef = Just "\161898", memConvRoleName = (fromJust (parseRoleName "a1mha4imm1xb5g_zpjn9dlgs1p_9qhbonvjotvlxrfc2cv956tsl3jfngrz9ej"))}
testObject_Member_user_6 :: Member
testObject_Member_user_6 = Member {memId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000002"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))}), memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -2}), memOtrMutedRef = Just "", memOtrArchived = True, memOtrArchivedRef = Just "S*\1075051", memHidden = True, memHiddenRef = Just "Y\1061361\DC2", memConvRoleName = (fromJust (parseRoleName "iqspab_so6kt_ifn3lf"))}
testObject_Member_user_7 :: Member
testObject_Member_user_7 = Member {memId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000001"))), memService = Nothing, memOtrMuted = True, memOtrMutedStatus = Nothing, memOtrMutedRef = Just "^", memOtrArchived = False, memOtrArchivedRef = Nothing, memHidden = False, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "fkcp3zxa88q7kk70ij3_9agz"))}
testObject_Member_user_8 :: Member
testObject_Member_user_8 = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 2}), memOtrMutedRef = Just "\1085178\GS", memOtrArchived = False, memOtrArchivedRef = Just "=", memHidden = True, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "w93x4b273q1gqw0316chxo8idg7xhh_fqd43t_ureo"))}
testObject_Member_user_9 :: Member
testObject_Member_user_9 = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000002"))), memService = Nothing, memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), memOtrMutedRef = Just "", memOtrArchived = False, memOtrArchivedRef = Just "Q?", memHidden = False, memHiddenRef = Nothing, memConvRoleName = (fromJust (parseRoleName "drjhreng8jzcx0wkybugbdi3ni8xcdnbeqhfz3jc2hcposq672uglalt9ieso3u62zy80f"))}
testObject_Member_user_10 :: Member
testObject_Member_user_10 = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000002"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 2}), memOtrMutedRef = Just "%L", memOtrArchived = False, memOtrArchivedRef = Just "f", memHidden = True, memHiddenRef = Nothing, memConvRoleName = (fromJust (parseRoleName "qaq3aaqy3141wuj686_2ke2dx0xctyr4xaheo_cuokpypqlab5y4vw4db9vgps34jclh9vu01161ze_xyinsc19oaqw4"))}
testObject_Member_user_11 :: Member
testObject_Member_user_11 = Member {memId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))}), memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 2}), memOtrMutedRef = Just "\1095784\RS", memOtrArchived = True, memOtrArchivedRef = Just "\1006957\DC1", memHidden = True, memHiddenRef = Nothing, memConvRoleName = (fromJust (parseRoleName "ockei620x22ybujjhnfmgq6v5s74"))}
testObject_Member_user_12 :: Member
testObject_Member_user_12 = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000001"))), memService = Nothing, memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), memOtrMutedRef = Just "\n", memOtrArchived = True, memOtrArchivedRef = Nothing, memHidden = False, memHiddenRef = Nothing, memConvRoleName = (fromJust (parseRoleName "68kf9e45l4aofua2om4c9u5n8fd_nwgq"))}
testObject_Member_user_13 :: Member
testObject_Member_user_13 = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001"))), memService = Nothing, memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -2}), memOtrMutedRef = Just "K\1021721", memOtrArchived = True, memOtrArchivedRef = Nothing, memHidden = True, memHiddenRef = Nothing, memConvRoleName = (fromJust (parseRoleName "px3cdba7q439m5txm6i2wgpq5bfh5rahn43poj4qg"))}
testObject_Member_user_14 :: Member
testObject_Member_user_14 = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), memService = Nothing, memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 2}), memOtrMutedRef = Just "z", memOtrArchived = True, memOtrArchivedRef = Just "(v\132402", memHidden = False, memHiddenRef = Nothing, memConvRoleName = (fromJust (parseRoleName "1qgr7v5s0eapk15cwr_ulxqgesw48em3hfwz0vz_lk5k7a94k84p2j7u9ll6xetvq1hfzc9dd19yz"))}
testObject_Member_user_15 :: Member
testObject_Member_user_15 = Member {memId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000002"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))}), memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 2}), memOtrMutedRef = Just "\GS\CAN\SUB", memOtrArchived = True, memOtrArchivedRef = Just "\US", memHidden = False, memHiddenRef = Just "\r9#", memConvRoleName = (fromJust (parseRoleName "ync5e0jx3ne1f1zrfdg1ablrkjklht80fvh60cc9gk0rxfebp0k7amd3foq"))}
testObject_Member_user_16 :: Member
testObject_Member_user_16 = Member {memId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000001"))), memService = Nothing, memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), memOtrMutedRef = Just "\41241>\169596", memOtrArchived = False, memOtrArchivedRef = Just "]\1055746\&1", memHidden = True, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "dmtgi2h1tajqu3qfxtpzqbuibefvas_yw9xq0caki2mtoodbk2e597n8c"))}
testObject_Member_user_17 :: Member
testObject_Member_user_17 = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -2}), memOtrMutedRef = Just "\17847", memOtrArchived = False, memOtrArchivedRef = Just "v", memHidden = False, memHiddenRef = Nothing, memConvRoleName = (fromJust (parseRoleName "b276fg0sykntrlf4s9esj7x5jz"))}
testObject_Member_user_18 :: Member
testObject_Member_user_18 = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), memOtrMutedRef = Nothing, memOtrArchived = False, memOtrArchivedRef = Just "", memHidden = True, memHiddenRef = Just "\1097275~", memConvRoleName = (fromJust (parseRoleName "9f9o8z9s"))}
testObject_Member_user_19 :: Member
testObject_Member_user_19 = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000000"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))}), memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}), memOtrMutedRef = Just "< ", memOtrArchived = True, memOtrArchivedRef = Just "", memHidden = True, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "91oqvpccks50b2rj2jfk50yjmqx2em7xns4ofquoagp45_ffkqx9p3"))}
testObject_Member_user_20 :: Member
testObject_Member_user_20 = Member {memId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000000"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))}), memOtrMuted = False, memOtrMutedStatus = Nothing, memOtrMutedRef = Just "\179024':", memOtrArchived = True, memOtrArchivedRef = Just "\1091232", memHidden = True, memHiddenRef = Just "T", memConvRoleName = (fromJust (parseRoleName "lkm33n9t115g72nxbfkl9mnzh02bdidoqklpjzr1asnvq0sumgp8zi8_ad2f0e6m4hyu4qa1w4tv7zuv2b_rxpv2a3ctbi455680em_jj7rgp6vrwbsfl23ylxxe"))}
