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
testObject_Conversation_1 :: Conversation
testObject_Conversation_1 = Conversation {cnvId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000002"))), cnvType = SelfConv, cnvCreator = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000000"))), cnvAccess = [PrivateAccess], cnvAccessRole = TeamAccessRole, cnvName = Just "", cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))}), memOtrMuted = True, memOtrMutedStatus = Nothing, memOtrMutedRef = Just "", memOtrArchived = True, memOtrArchivedRef = Nothing, memHidden = False, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "7jqde35duaidiq4c8j__gap07mgmf1_ctexvfs0rvyit2tr1hvg2h3jorjkmalbipq1vo55xytlzad87"))}, cmOthers = []}, cnvTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000000"))), cnvMessageTimer = Just (Ms {ms = 3}), cnvReceiptMode = Just (ReceiptMode {unReceiptMode = -1})}
testObject_Conversation_2 :: Conversation
testObject_Conversation_2 = Conversation {cnvId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000001"))), cnvType = SelfConv, cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000000"))), cnvAccess = [], cnvAccessRole = NonActivatedAccessRole, cnvName = Just "az", cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))}), memOtrMuted = False, memOtrMutedStatus = Nothing, memOtrMutedRef = Just "", memOtrArchived = True, memOtrArchivedRef = Nothing, memHidden = False, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "q8wyyaya3f6codsg4ozpf6dg7fgvdyhok53e660qegain1ok8yy973a7e5t6d0ovwvih7fur15su79gqh6i46d9jevst08"))}, cmOthers = [OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))}), omConvRoleName = (fromJust (parseRoleName "bjb3eq1c_44dlt"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))}), omConvRoleName = (fromJust (parseRoleName "kv1xj_ba9zove2"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "oz8"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))}), omConvRoleName = (fromJust (parseRoleName "1v13868u79jfvzhhfajjtrm3mth5gvzrk3i_rh3ttaje8ad_0hi2mg17skr65eop341mjhf2c5lyq1mfsbdcx"))}]}, cnvTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000002"))), cnvMessageTimer = Just (Ms {ms = 4}), cnvReceiptMode = Nothing}
testObject_Conversation_3 :: Conversation
testObject_Conversation_3 = Conversation {cnvId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000001"))), cnvType = ConnectConv, cnvCreator = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000000"))), cnvAccess = [InviteAccess,CodeAccess,CodeAccess,CodeAccess,CodeAccess], cnvAccessRole = ActivatedAccessRole, cnvName = Nothing, cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}), memOtrMutedRef = Nothing, memOtrArchived = True, memOtrArchivedRef = Just "", memHidden = True, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "5jnlpa9ai_l40nxveg008jtk996pr2y6f1k9d65xkhlic6img5ymddyz11t8ldd0nc105rnobp2zkd1x52ffn"))}, cmOthers = []}, cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), cnvMessageTimer = Just (Ms {ms = 0}), cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 0})}
testObject_Conversation_4 :: Conversation
testObject_Conversation_4 = Conversation {cnvId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))), cnvType = RegularConv, cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001"))), cnvAccess = [], cnvAccessRole = ActivatedAccessRole, cnvName = Nothing, cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))}), memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), memOtrMutedRef = Nothing, memOtrArchived = True, memOtrArchivedRef = Just "", memHidden = False, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "ffta73feupryyae39p0clc0ju262"))}, cmOthers = [OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))}), omConvRoleName = (fromJust (parseRoleName "xojr7xgzookt6k_32qjfv6mtzem_9jyecrimc_ingu_hvrrrd932mran13t3rlynun81wa1f8mxgzd233c9v"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))}), omConvRoleName = (fromJust (parseRoleName "y96yod3u4wsxc1d2xd6oz6gactj06gpiffed_gn76umj0oyht870idg0jkb3_vmjqavyjm1x4j1l4a1u_l1vyk7l9rzk8h06ggbckpswbi"))}]}, cnvTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000002"))), cnvMessageTimer = Just (Ms {ms = 0}), cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 0})}
testObject_Conversation_5 :: Conversation
testObject_Conversation_5 = Conversation {cnvId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000000"))), cnvType = RegularConv, cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), cnvAccess = [CodeAccess], cnvAccessRole = ActivatedAccessRole, cnvName = Just "u", cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))}), memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), memOtrMutedRef = Just "", memOtrArchived = True, memOtrArchivedRef = Just "", memHidden = True, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "_q2redmre88w5ynqpnl63bzr09fifoy_4a6xakihibh9jho42c6dxxfg9q8sjazulu45nfy213j7au0e"))}, cmOthers = []}, cnvTeam = Nothing, cnvMessageTimer = Just (Ms {ms = 1}), cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 1})}
