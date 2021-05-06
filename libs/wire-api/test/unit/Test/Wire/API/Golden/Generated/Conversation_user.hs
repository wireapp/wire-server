{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Conversation_user where

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
testObject_Conversation_user_1 :: Conversation
testObject_Conversation_user_1 = Conversation {cnvId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000002"))), cnvType = ConnectConv, cnvCreator = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000002"))), cnvAccess = [], cnvAccessRole = ActivatedAccessRole, cnvName = Just "l\1106075L", cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), memService = Nothing, memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), memOtrMutedRef = Just "", memOtrArchived = False, memOtrArchivedRef = Just "", memHidden = True, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "9k0d296icgm9i15wl_lltwvwrsm633k_rqcaxq9a_dk93co8wvol8a3hot36l1cxdapd6d27eiicdckzfi6zes"))}, cmOthers = []}, cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), cnvMessageTimer = Nothing, cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 1})}
testObject_Conversation_user_2 :: Conversation
testObject_Conversation_user_2 = Conversation {cnvId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000000"))), cnvType = RegularConv, cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000002"))), cnvAccess = [InviteAccess], cnvAccessRole = PrivateAccessRole, cnvName = Just "", cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), memService = Nothing, memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), memOtrMutedRef = Just "", memOtrArchived = False, memOtrArchivedRef = Just "", memHidden = False, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "ou4g6r1kiu3v3lh29tdrssznji"))}, cmOthers = [OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))}), omConvRoleName = (fromJust (parseRoleName "244a88zufclp0lxvp53fm8vv14j5_d12074ixzpvuqpepnz3"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))}), omConvRoleName = (fromJust (parseRoleName "93zvzsusdud9kuqup6hlf2hot7ycl2zi1gb5x8loekdbcsu04xjpbhnfdceh2nyg8cv_"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))}), omConvRoleName = (fromJust (parseRoleName "smrizugod71gro0ave5raq08tf31y21_sslgg6t9byg4_w3"))}]}, cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000002"))), cnvMessageTimer = Nothing, cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 0})}
testObject_Conversation_user_3 :: Conversation
testObject_Conversation_user_3 = Conversation {cnvId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000000"))), cnvType = RegularConv, cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000001"))), cnvAccess = [InviteAccess,CodeAccess,InviteAccess,PrivateAccess], cnvAccessRole = ActivatedAccessRole, cnvName = Just "mZ\996183", cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), memOtrMutedRef = Just "", memOtrArchived = True, memOtrArchivedRef = Just "", memHidden = False, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "sze_0mzsof3b_4y_iwgbuek4v3k5dewcp7tiy20o2woor6fiy4tths20vh5i17c4wxin3z_uriugvw8sq8upinn4d9cgb"))}, cmOthers = []}, cnvTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000002"))), cnvMessageTimer = Just (Ms {ms = 7397611557308551}), cnvReceiptMode = Just (ReceiptMode {unReceiptMode = -2})}
testObject_Conversation_user_4 :: Conversation
testObject_Conversation_user_4 = Conversation {cnvId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001"))), cnvType = RegularConv, cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), cnvAccess = [LinkAccess], cnvAccessRole = PrivateAccessRole, cnvName = Nothing, cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))}), memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), memOtrMutedRef = Nothing, memOtrArchived = False, memOtrArchivedRef = Just "", memHidden = True, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "yfs6sdjzi7m900hy9lyk4vmuca69i_t3uxhovp6yfe4ozuqf033yek1zvi_1wtfgmnfe9honh_ms94ai4ky_1d985qju3"))}, cmOthers = []}, cnvTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000002"))), cnvMessageTimer = Nothing, cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 1})}
testObject_Conversation_user_5 :: Conversation
testObject_Conversation_user_5 = Conversation {cnvId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000002"))), cnvType = SelfConv, cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000002"))), cnvAccess = [PrivateAccess,InviteAccess], cnvAccessRole = ActivatedAccessRole, cnvName = Just "", cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))}), memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}), memOtrMutedRef = Just "", memOtrArchived = False, memOtrArchivedRef = Nothing, memHidden = True, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "6g99eks_qb_80"))}, cmOthers = []}, cnvTeam = Nothing, cnvMessageTimer = Just (Ms {ms = 738636990826857}), cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 2})}
