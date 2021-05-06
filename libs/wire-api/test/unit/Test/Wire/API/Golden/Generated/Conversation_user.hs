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
testObject_Conversation_user_1 = Conversation {cnvId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000001"))), cnvType = SelfConv, cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000002"))), cnvAccess = [], cnvAccessRole = PrivateAccessRole, cnvName = Nothing, cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))}), memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), memOtrMutedRef = Just "", memOtrArchived = False, memOtrArchivedRef = Nothing, memHidden = False, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "awjcx"))}, cmOthers = [OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "wdlcgpzu20qenra"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))}), omConvRoleName = (fromJust (parseRoleName "6agqmd9m9f8ifcz183xwfo40nnriorhc5jtbml9ijc9dn0zhq52dhcjm_yridjwkmec22qt8b42cqc0o79ejo_kyw5lae3501vuj9yx1s"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))}), omConvRoleName = (fromJust (parseRoleName "0ksndh_a7w5fv0alen563mo28_ozrdtv6q54lk39p_dvmbnf5ejj9bau9f7vm32c247bp9"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))}), omConvRoleName = (fromJust (parseRoleName "v83tinf1mqs1tmmukkr8vwp7qv24o5fo4j2wkx_64ubqnw461jsm4v5d9_osg86iyp0sqi_x6g36t92eiadbf2ja9ajqbgu0m3lq2oufvepy3mfwiz_hwisjf5po2f"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))}), omConvRoleName = (fromJust (parseRoleName "3r3mdiblwiv_yjz2n0agc971vbvfvjkm7ihja_gh2jtxzxfabf5qltv"))}]}, cnvTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000001"))), cnvMessageTimer = Just (Ms {ms = 4}), cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 2})}
testObject_Conversation_user_2 :: Conversation
testObject_Conversation_user_2 = Conversation {cnvId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000001"))), cnvType = SelfConv, cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000002"))), cnvAccess = [InviteAccess,LinkAccess,PrivateAccess], cnvAccessRole = PrivateAccessRole, cnvName = Just "", cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))}), memOtrMuted = True, memOtrMutedStatus = Nothing, memOtrMutedRef = Just "", memOtrArchived = False, memOtrArchivedRef = Just "", memHidden = False, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "yj_tlt3oh6e8766k9pc5wykuetauadtm4nbtk"))}, cmOthers = []}, cnvTeam = Nothing, cnvMessageTimer = Nothing, cnvReceiptMode = Nothing}
testObject_Conversation_user_3 :: Conversation
testObject_Conversation_user_3 = Conversation {cnvId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000002"))), cnvType = SelfConv, cnvCreator = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000002"))), cnvAccess = [PrivateAccess,PrivateAccess], cnvAccessRole = PrivateAccessRole, cnvName = Nothing, cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))}), memOtrMuted = False, memOtrMutedStatus = Nothing, memOtrMutedRef = Just "", memOtrArchived = True, memOtrArchivedRef = Just "", memHidden = False, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "b5ovpttrlm5psg83x_pf9_r_3jypn6wa22_uh3r47zhg7_c7_uv8kl9c9f4ew2mu0bji9y11x1o6hzbjrwxq2rlac65vi1s8qllx7jholxarowgku8nq"))}, cmOthers = [OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))}), omConvRoleName = (fromJust (parseRoleName "jcj58g7ok3hrr_mpi2rjozl24yf82kb2saesyzowkq2ya3ghnajrc8f99biqd0dci1r30hpxx09x5pz9fxjb51sbnryh4d5ff_iwm85w004j0b"))}]}, cnvTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000001"))), cnvMessageTimer = Just (Ms {ms = 0}), cnvReceiptMode = Nothing}
testObject_Conversation_user_4 :: Conversation
testObject_Conversation_user_4 = Conversation {cnvId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), cnvType = One2OneConv, cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000001"))), cnvAccess = [], cnvAccessRole = TeamAccessRole, cnvName = Just "\152024\1103749", cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), memService = Nothing, memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), memOtrMutedRef = Just "", memOtrArchived = True, memOtrArchivedRef = Nothing, memHidden = False, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "2lya5nki1kq8"))}, cmOthers = [OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "ue7hh1sieiuwfwcn_jo3hfr6pit1vuki_czsf0fexq8_38pr6d21vly30wiuq1ci4nxvke6byquqk4682wfsxkb"))}]}, cnvTeam = Nothing, cnvMessageTimer = Nothing, cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 1})}
testObject_Conversation_user_5 :: Conversation
testObject_Conversation_user_5 = Conversation {cnvId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), cnvType = SelfConv, cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000002"))), cnvAccess = [CodeAccess,CodeAccess,LinkAccess], cnvAccessRole = TeamAccessRole, cnvName = Just "\1069884\100718\169171", cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), memOtrMutedRef = Just "", memOtrArchived = False, memOtrArchivedRef = Nothing, memHidden = False, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "3p8lkc0favfcipct0t5kdg0mz7x"))}, cmOthers = [OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))}), omConvRoleName = (fromJust (parseRoleName "dxv6tz59iohns1bzixap37neyt_bd13jui0lwxccxd80sjw1bsu5wn1du030g5qqf_q0ze82f05w2zm2phtywdx4qm1gd"))}]}, cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000002"))), cnvMessageTimer = Nothing, cnvReceiptMode = Just (ReceiptMode {unReceiptMode = -1})}
