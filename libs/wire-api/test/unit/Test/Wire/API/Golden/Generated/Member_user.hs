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
testObject_Member_user_1 = Member {memId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000002"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), memOtrMutedRef = Just ".", memOtrArchived = True, memOtrArchivedRef = Nothing, memHidden = False, memHiddenRef = Just " ", memConvRoleName = (fromJust (parseRoleName "dlqvpxd60rlmzlghvs6tr3pxtcp8qnoegcneu1kn1"))}
testObject_Member_user_2 :: Member
testObject_Member_user_2 = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000001"))), memService = Nothing, memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), memOtrMutedRef = Just "", memOtrArchived = False, memOtrArchivedRef = Just "", memHidden = True, memHiddenRef = Just "y\GS\SO", memConvRoleName = (fromJust (parseRoleName "ov37ldmpe22lhvg81b7s5iu5720sk27vic1y05z2ibn85kcyj6vqlpq2ffvuqgax6h6pbkv3avje"))}
testObject_Member_user_3 :: Member
testObject_Member_user_3 = Member {memId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000002"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -2}), memOtrMutedRef = Nothing, memOtrArchived = False, memOtrArchivedRef = Nothing, memHidden = True, memHiddenRef = Just "\US\SI2", memConvRoleName = (fromJust (parseRoleName "rq5nec5z_2_dzsgrcgdmdopq_9juom6_rdipz4yidciq1v598xyilik_ay9szhmti6u8nrlw6uiid8wpvcjle6b2xc"))}
testObject_Member_user_4 :: Member
testObject_Member_user_4 = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -2}), memOtrMutedRef = Just "]", memOtrArchived = True, memOtrArchivedRef = Just "", memHidden = False, memHiddenRef = Just "!*", memConvRoleName = (fromJust (parseRoleName "d7_eet1iztwhskcd_u5tsr7kv3gf9cmx0bjuf7pk0prn3a75mfy3lfp29vnpc8fmv5ko85ivr"))}
testObject_Member_user_5 :: Member
testObject_Member_user_5 = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -2}), memOtrMutedRef = Just "", memOtrArchived = True, memOtrArchivedRef = Just "G", memHidden = False, memHiddenRef = Nothing, memConvRoleName = (fromJust (parseRoleName "xv597ku2j9quifk46hjkwivkve_373ihuf_xdm9suvxrhibokljh3mrxdq4h_51duv9cijx57n9sz5fths8h6_db7gxslu52_d"))}
testObject_Member_user_6 :: Member
testObject_Member_user_6 = Member {memId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000000"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))}), memOtrMuted = True, memOtrMutedStatus = Nothing, memOtrMutedRef = Nothing, memOtrArchived = True, memOtrArchivedRef = Nothing, memHidden = False, memHiddenRef = Just "Kb\SUB", memConvRoleName = (fromJust (parseRoleName "kvec8q8w8kzq6p2wb"))}
testObject_Member_user_7 :: Member
testObject_Member_user_7 = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000002"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))}), memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -2}), memOtrMutedRef = Just "\ETB\1075969", memOtrArchived = True, memOtrArchivedRef = Just "q", memHidden = True, memHiddenRef = Just "\19762\ETX", memConvRoleName = (fromJust (parseRoleName "_zwu6pb7tolqt5ju29nvho37e85xfto4oq5ksctchukf0dowkymui5"))}
testObject_Member_user_8 :: Member
testObject_Member_user_8 = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), memOtrMutedRef = Nothing, memOtrArchived = False, memOtrArchivedRef = Just "X\SUB:", memHidden = False, memHiddenRef = Just "\1007755\170223", memConvRoleName = (fromJust (parseRoleName "3qi4n3mzq1f8pr_3akdhb0_tsrbik7jpc6l66jovma5re22eicwyw"))}
testObject_Member_user_9 :: Member
testObject_Member_user_9 = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000000"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))}), memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), memOtrMutedRef = Just "K>", memOtrArchived = False, memOtrArchivedRef = Nothing, memHidden = True, memHiddenRef = Nothing, memConvRoleName = (fromJust (parseRoleName "z_ppa1ez6hwtprxr6_lsnx2pfscfxxqcqs4px6k6z2v6d9xqnjbg3g76brtyaya3b1o5k2czm9m3nl5w6z9yw5h5minlc3j7t8ucjpwtz4"))}
testObject_Member_user_10 :: Member
testObject_Member_user_10 = Member {memId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000002"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))}), memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -2}), memOtrMutedRef = Just "", memOtrArchived = False, memOtrArchivedRef = Just "\134292", memHidden = False, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "vy7t4xonqy9420zre08g2r0otnav7lgi7t_0kq_ei96cph9lqgl8j8wmf"))}
testObject_Member_user_11 :: Member
testObject_Member_user_11 = Member {memId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), memOtrMutedRef = Nothing, memOtrArchived = True, memOtrArchivedRef = Just ";", memHidden = False, memHiddenRef = Just "\28164", memConvRoleName = (fromJust (parseRoleName "m65hacgt8_sw1fl5offwpq45biw9xx49u216weo76un"))}
testObject_Member_user_12 :: Member
testObject_Member_user_12 = Member {memId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))}), memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), memOtrMutedRef = Just "\144070P", memOtrArchived = True, memOtrArchivedRef = Just "\40062TN", memHidden = True, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "4pccxjrzcws6t4kn9jo2_2s7vhnloj2x02xeka1p4hor41n6keq9x7"))}
testObject_Member_user_13 :: Member
testObject_Member_user_13 = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000000"))), memService = Nothing, memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), memOtrMutedRef = Just "\DLE$F", memOtrArchived = False, memOtrArchivedRef = Just "\1003323\t", memHidden = True, memHiddenRef = Just ",", memConvRoleName = (fromJust (parseRoleName "qf2e0flr2t0hcyjrw637xw3at7mdnld36t"))}
testObject_Member_user_14 :: Member
testObject_Member_user_14 = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000002"))), memService = Nothing, memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -2}), memOtrMutedRef = Just "iC7", memOtrArchived = False, memOtrArchivedRef = Just "JH", memHidden = True, memHiddenRef = Just "7}", memConvRoleName = (fromJust (parseRoleName "sf__2qkjt49m96nlkk8y8c_grw2f8fl5qq35urpb7jn5eic"))}
testObject_Member_user_15 :: Member
testObject_Member_user_15 = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000002"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 2}), memOtrMutedRef = Just "\1007956", memOtrArchived = False, memOtrArchivedRef = Nothing, memHidden = True, memHiddenRef = Nothing, memConvRoleName = (fromJust (parseRoleName "iayo0ioc9suu0wmakoeubea5qoajtqs8ios2jvbvkz9cledcx4v3guu78qztvch21xyv2t5cd2ep0p3icsessk1__j9kct4nq7ikdw1doe3amr"))}
testObject_Member_user_16 :: Member
testObject_Member_user_16 = Member {memId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000001"))), memService = Nothing, memOtrMuted = False, memOtrMutedStatus = Nothing, memOtrMutedRef = Just "\DC1", memOtrArchived = True, memOtrArchivedRef = Just "\19572z", memHidden = True, memHiddenRef = Just "\1092615\DC3", memConvRoleName = (fromJust (parseRoleName "01ef7suqygfx8ihozhks12e6yjjb1zqmob6mfcn8qqy4ccpv3ioj8rps8tsec0m0y6pfaqu3zht5x6ggu1fpkxek9fr0j5o6dtohx3jnf4h"))}
testObject_Member_user_17 :: Member
testObject_Member_user_17 = Member {memId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 2}), memOtrMutedRef = Just "", memOtrArchived = False, memOtrArchivedRef = Just "\995033", memHidden = False, memHiddenRef = Just "6\ESC", memConvRoleName = (fromJust (parseRoleName "45sci63_5ew_ys16rgenzjpd31a286jzb41hyuspl3cqy8p2zwqilsdzfn1eqqjqr8fsz_uegvoa7mhfdtzx3fy09bdi8_sk2lfao2_aqvfcflmb87bth02"))}
testObject_Member_user_18 :: Member
testObject_Member_user_18 = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}), memOtrMutedRef = Just "`-", memOtrArchived = False, memOtrArchivedRef = Just "", memHidden = True, memHiddenRef = Nothing, memConvRoleName = (fromJust (parseRoleName "ohwghmq8rtrkc6obgq2t65et38to5jdthzphflwknubutb72dy5yk8lwd_dlmg0bx6lw0np7ybttlu"))}
testObject_Member_user_19 :: Member
testObject_Member_user_19 = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))}), memOtrMuted = False, memOtrMutedStatus = Nothing, memOtrMutedRef = Just "\ESC\126092g", memOtrArchived = True, memOtrArchivedRef = Nothing, memHidden = False, memHiddenRef = Just "\1071116", memConvRoleName = (fromJust (parseRoleName "beftahguw04v153tl2v0mfwjnyqo1g4er856o"))}
testObject_Member_user_20 :: Member
testObject_Member_user_20 = Member {memId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000000"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), memOtrMutedRef = Nothing, memOtrArchived = True, memOtrArchivedRef = Just "K", memHidden = False, memHiddenRef = Nothing, memConvRoleName = (fromJust (parseRoleName "ni30kbmg"))}
