{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ConversationList_20Conversation_user where

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
testObject_ConversationList_20Conversation_user_1 :: ConversationList Conversation
testObject_ConversationList_20Conversation_user_1 = ConversationList {convList = [Conversation {cnvId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), cnvType = SelfConv, cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), cnvAccess = [], cnvAccessRole = ActivatedAccessRole, cnvName = Just "", cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), memService = Nothing, memOtrMuted = True, memOtrMutedStatus = Nothing, memOtrMutedRef = Just "", memOtrArchived = False, memOtrArchivedRef = Just "", memHidden = True, memHiddenRef = Nothing, memConvRoleName = (fromJust (parseRoleName "3f1nevbrul4pf8vhn59qt90njrh0c787j9897yd_j9xy901x9ee0xzpnkwibu9ybg4"))}, cmOthers = []}, cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), cnvMessageTimer = Just (Ms {ms = 1}), cnvReceiptMode = Just (ReceiptMode {unReceiptMode = -1})},Conversation {cnvId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), cnvType = SelfConv, cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), cnvAccess = [], cnvAccessRole = PrivateAccessRole, cnvName = Nothing, cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))}), memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), memOtrMutedRef = Just "", memOtrArchived = True, memOtrArchivedRef = Just "", memHidden = False, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "x9ukjwhafmathr2affogykqv775ouys03klr9wa1z5g1ir5osv9u3cc1nu2l4g9v4deo1opbnbaxryjydw6crzb6oospxytou"))}, cmOthers = []}, cnvTeam = Nothing, cnvMessageTimer = Just (Ms {ms = 0}), cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 1})},Conversation {cnvId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), cnvType = ConnectConv, cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), cnvAccess = [], cnvAccessRole = TeamAccessRole, cnvName = Just "", cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))}), memOtrMuted = False, memOtrMutedStatus = Nothing, memOtrMutedRef = Just "", memOtrArchived = True, memOtrArchivedRef = Just "", memHidden = True, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "p9l1bo4qq8"))}, cmOthers = []}, cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), cnvMessageTimer = Just (Ms {ms = 0}), cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 1})}], convHasMore = False}
testObject_ConversationList_20Conversation_user_2 :: ConversationList Conversation
testObject_ConversationList_20Conversation_user_2 = ConversationList {convList = [], convHasMore = True}
testObject_ConversationList_20Conversation_user_3 :: ConversationList Conversation
testObject_ConversationList_20Conversation_user_3 = ConversationList {convList = [Conversation {cnvId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), cnvType = RegularConv, cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), cnvAccess = [LinkAccess], cnvAccessRole = TeamAccessRole, cnvName = Just "7", cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), memOtrMutedRef = Just "", memOtrArchived = False, memOtrArchivedRef = Just "", memHidden = True, memHiddenRef = Nothing, memConvRoleName = (fromJust (parseRoleName "_84m_qo0wt6enuynyfrqp3zz4b8onvlj4r2ptylrqg3jwa83t3yl0wfydtf8scv8n2zww"))}, cmOthers = []}, cnvTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), cnvMessageTimer = Just (Ms {ms = 1}), cnvReceiptMode = Just (ReceiptMode {unReceiptMode = -1})}], convHasMore = True}
testObject_ConversationList_20Conversation_user_4 :: ConversationList Conversation
testObject_ConversationList_20Conversation_user_4 = ConversationList {convList = [Conversation {cnvId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), cnvType = RegularConv, cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), cnvAccess = [], cnvAccessRole = ActivatedAccessRole, cnvName = Just "", cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}), memOtrMutedRef = Just "", memOtrArchived = False, memOtrArchivedRef = Just "", memHidden = False, memHiddenRef = Nothing, memConvRoleName = (fromJust (parseRoleName "qvj819r4tue"))}, cmOthers = []}, cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), cnvMessageTimer = Just (Ms {ms = 1}), cnvReceiptMode = Just (ReceiptMode {unReceiptMode = -1})}], convHasMore = True}
testObject_ConversationList_20Conversation_user_5 :: ConversationList Conversation
testObject_ConversationList_20Conversation_user_5 = ConversationList {convList = [Conversation {cnvId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), cnvType = SelfConv, cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), cnvAccess = [], cnvAccessRole = ActivatedAccessRole, cnvName = Just "", cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}), memOtrMutedRef = Just "", memOtrArchived = True, memOtrArchivedRef = Just "", memHidden = True, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "n0u66040fh5yuo"))}, cmOthers = []}, cnvTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), cnvMessageTimer = Nothing, cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 0})},Conversation {cnvId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), cnvType = One2OneConv, cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), cnvAccess = [], cnvAccessRole = PrivateAccessRole, cnvName = Just "", cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), memService = Nothing, memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}), memOtrMutedRef = Just "", memOtrArchived = True, memOtrArchivedRef = Just "", memHidden = True, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "4d4f_hnhxorpfo2medym7sxdnoe5rqbtqamau_gyqd1nnv780cdmb0mbnkkhi7fcfzk73l0o_7jjx85zhv7qk_s64zmhp6rgic7nbgr4"))}, cmOthers = []}, cnvTeam = Nothing, cnvMessageTimer = Just (Ms {ms = 1}), cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 0})},Conversation {cnvId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), cnvType = RegularConv, cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), cnvAccess = [], cnvAccessRole = PrivateAccessRole, cnvName = Just "", cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))}), memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}), memOtrMutedRef = Just "", memOtrArchived = False, memOtrArchivedRef = Just "", memHidden = True, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "upkmb8ycnir_z_dqfezd30k7n1k27e6c6_zjs6ilhjumrmo_x14ipyrrndcx0ha9b02apei7lxs"))}, cmOthers = []}, cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), cnvMessageTimer = Nothing, cnvReceiptMode = Just (ReceiptMode {unReceiptMode = -1})},Conversation {cnvId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), cnvType = One2OneConv, cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), cnvAccess = [], cnvAccessRole = ActivatedAccessRole, cnvName = Nothing, cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), memService = Nothing, memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}), memOtrMutedRef = Just "", memOtrArchived = True, memOtrArchivedRef = Just "", memHidden = True, memHiddenRef = Nothing, memConvRoleName = (fromJust (parseRoleName "0h9o329yw1zazrma3o"))}, cmOthers = []}, cnvTeam = Nothing, cnvMessageTimer = Just (Ms {ms = 0}), cnvReceiptMode = Just (ReceiptMode {unReceiptMode = -1})}], convHasMore = False}
