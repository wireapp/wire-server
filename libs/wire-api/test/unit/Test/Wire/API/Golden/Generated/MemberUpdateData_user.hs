{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.MemberUpdateData_user where

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
testObject_MemberUpdateData_user_1 :: MemberUpdateData
testObject_MemberUpdateData_user_1 = MemberUpdateData {misTarget = Nothing, misOtrMuted = Just False, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), misOtrMutedRef = Just "\1012774", misOtrArchived = Nothing, misOtrArchivedRef = Just "\SOH", misHidden = Just True, misHiddenRef = Nothing, misConvRoleName = Just (fromJust (parseRoleName "5rbybu01gyp7blx07crfzeiwcf9njm_cpcj_2gxayxn6cm8p7w3sy_"))}
testObject_MemberUpdateData_user_2 :: MemberUpdateData
testObject_MemberUpdateData_user_2 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000001"))), misOtrMuted = Nothing, misOtrMutedStatus = Nothing, misOtrMutedRef = Just "", misOtrArchived = Nothing, misOtrArchivedRef = Just "", misHidden = Nothing, misHiddenRef = Just "\SIl", misConvRoleName = Just (fromJust (parseRoleName "0egy_3kg9samlw11i_nl8xfc9cbvhobg3674"))}
testObject_MemberUpdateData_user_3 :: MemberUpdateData
testObject_MemberUpdateData_user_3 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))), misOtrMuted = Just False, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}), misOtrMutedRef = Just "", misOtrArchived = Just False, misOtrArchivedRef = Just "\149675I", misHidden = Just False, misHiddenRef = Nothing, misConvRoleName = Just (fromJust (parseRoleName "w77zexs7novf7ziys8s42giqf5kjbwr3f888fbja0em_9vc9j9vf_f0ngnsz5eapu7scvg283aud6z2lis6mqnt50xswn384"))}
testObject_MemberUpdateData_user_4 :: MemberUpdateData
testObject_MemberUpdateData_user_4 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), misOtrMuted = Just False, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}), misOtrMutedRef = Just "", misOtrArchived = Nothing, misOtrArchivedRef = Just "%\94312", misHidden = Just False, misHiddenRef = Just "\f\US\SYN", misConvRoleName = Nothing}
testObject_MemberUpdateData_user_5 :: MemberUpdateData
testObject_MemberUpdateData_user_5 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001"))), misOtrMuted = Just True, misOtrMutedStatus = Nothing, misOtrMutedRef = Nothing, misOtrArchived = Nothing, misOtrArchivedRef = Just "\15711=", misHidden = Just True, misHiddenRef = Nothing, misConvRoleName = Just (fromJust (parseRoleName "brh0uoi8309uz2e09kmvv90zo3uk4c3jjzrqe74y2zuexaxh5i73m9g3fbzmayw38j_7hz5mt4f21w5gm4chnl0v22mft83_5wkc9br51yw89ma_7ic86r2b"))}
testObject_MemberUpdateData_user_6 :: MemberUpdateData
testObject_MemberUpdateData_user_6 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001"))), misOtrMuted = Just True, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}), misOtrMutedRef = Just "", misOtrArchived = Nothing, misOtrArchivedRef = Just "\33825^", misHidden = Just False, misHiddenRef = Nothing, misConvRoleName = Just (fromJust (parseRoleName "pydxjlg3t4m90vl0khrz0iuz7njtj_"))}
testObject_MemberUpdateData_user_7 :: MemberUpdateData
testObject_MemberUpdateData_user_7 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000001"))), misOtrMuted = Nothing, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}), misOtrMutedRef = Just "", misOtrArchived = Nothing, misOtrArchivedRef = Nothing, misHidden = Nothing, misHiddenRef = Nothing, misConvRoleName = Just (fromJust (parseRoleName "vp83i_c_9c4ch0g1mvy2h6p60kkidj6822ayiklu2avibct75_haitgel4ucolq5jrsr1ewr72erxvkstigsvoldwpw3kszq"))}
testObject_MemberUpdateData_user_8 :: MemberUpdateData
testObject_MemberUpdateData_user_8 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000000"))), misOtrMuted = Nothing, misOtrMutedStatus = Nothing, misOtrMutedRef = Nothing, misOtrArchived = Just False, misOtrArchivedRef = Just "", misHidden = Just True, misHiddenRef = Just "Gt!", misConvRoleName = Nothing}
testObject_MemberUpdateData_user_9 :: MemberUpdateData
testObject_MemberUpdateData_user_9 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000002"))), misOtrMuted = Just False, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), misOtrMutedRef = Just "\FS;E", misOtrArchived = Just True, misOtrArchivedRef = Just "T\73443\172979", misHidden = Just False, misHiddenRef = Nothing, misConvRoleName = Just (fromJust (parseRoleName "8ptacddlfzzwfym4ifpbkv6wrbpe2l2z8c4jpocniume1nkks8ka3pjc32eshuem501lx094qd_8ksmqija81"))}
testObject_MemberUpdateData_user_10 :: MemberUpdateData
testObject_MemberUpdateData_user_10 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000001"))), misOtrMuted = Nothing, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), misOtrMutedRef = Nothing, misOtrArchived = Just False, misOtrArchivedRef = Just "M", misHidden = Just False, misHiddenRef = Just "\vh", misConvRoleName = Just (fromJust (parseRoleName "nben_p9mitr_swi2wf0uubu1wv9ownlfce5eori58tt6ax18w_n"))}
testObject_MemberUpdateData_user_11 :: MemberUpdateData
testObject_MemberUpdateData_user_11 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000002"))), misOtrMuted = Just False, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), misOtrMutedRef = Just "\f2@", misOtrArchived = Just True, misOtrArchivedRef = Just "", misHidden = Just True, misHiddenRef = Just "\184510", misConvRoleName = Just (fromJust (parseRoleName "p4t6jhv9sftlfhf3oy765931v09tfr8b700p"))}
testObject_MemberUpdateData_user_12 :: MemberUpdateData
testObject_MemberUpdateData_user_12 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), misOtrMuted = Just True, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}), misOtrMutedRef = Just "\47664", misOtrArchived = Just True, misOtrArchivedRef = Nothing, misHidden = Nothing, misHiddenRef = Nothing, misConvRoleName = Just (fromJust (parseRoleName "6i2gaq9jiu0orrh5xfododn8fdof469qyi4wzd3zemh"))}
testObject_MemberUpdateData_user_13 :: MemberUpdateData
testObject_MemberUpdateData_user_13 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000002"))), misOtrMuted = Just False, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), misOtrMutedRef = Nothing, misOtrArchived = Nothing, misOtrArchivedRef = Nothing, misHidden = Just True, misHiddenRef = Nothing, misConvRoleName = Nothing}
testObject_MemberUpdateData_user_14 :: MemberUpdateData
testObject_MemberUpdateData_user_14 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000002"))), misOtrMuted = Just True, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}), misOtrMutedRef = Nothing, misOtrArchived = Just True, misOtrArchivedRef = Just "", misHidden = Just True, misHiddenRef = Just "\DC1-", misConvRoleName = Just (fromJust (parseRoleName "ix895uvysnc383fmhmlc03ayapz9y2mbhabdthj411"))}
testObject_MemberUpdateData_user_15 :: MemberUpdateData
testObject_MemberUpdateData_user_15 = MemberUpdateData {misTarget = Nothing, misOtrMuted = Just False, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), misOtrMutedRef = Just "Zt", misOtrArchived = Just True, misOtrArchivedRef = Just "\NAKb|", misHidden = Just True, misHiddenRef = Nothing, misConvRoleName = Just (fromJust (parseRoleName "l7yqqtcxxnidrgcoibc07a4zi1"))}
testObject_MemberUpdateData_user_16 :: MemberUpdateData
testObject_MemberUpdateData_user_16 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000001"))), misOtrMuted = Just True, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), misOtrMutedRef = Just "\1015400", misOtrArchived = Just True, misOtrArchivedRef = Just "", misHidden = Just True, misHiddenRef = Just "\1091741", misConvRoleName = Just (fromJust (parseRoleName "qb6k8rqiygrl"))}
testObject_MemberUpdateData_user_17 :: MemberUpdateData
testObject_MemberUpdateData_user_17 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000000"))), misOtrMuted = Just True, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), misOtrMutedRef = Just "\92302d", misOtrArchived = Just False, misOtrArchivedRef = Just "\1110923=", misHidden = Just False, misHiddenRef = Nothing, misConvRoleName = Just (fromJust (parseRoleName "a0v6qbfm87llbpqxclg6at1zkrzsmufz88gsus8ot4rv5_pszmsnnl5rwyj7xwql0n"))}
testObject_MemberUpdateData_user_18 :: MemberUpdateData
testObject_MemberUpdateData_user_18 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000000"))), misOtrMuted = Nothing, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}), misOtrMutedRef = Nothing, misOtrArchived = Nothing, misOtrArchivedRef = Nothing, misHidden = Just True, misHiddenRef = Just "\1090883=", misConvRoleName = Nothing}
testObject_MemberUpdateData_user_19 :: MemberUpdateData
testObject_MemberUpdateData_user_19 = MemberUpdateData {misTarget = Nothing, misOtrMuted = Just False, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 2}), misOtrMutedRef = Just "", misOtrArchived = Just True, misOtrArchivedRef = Just "", misHidden = Just False, misHiddenRef = Nothing, misConvRoleName = Just (fromJust (parseRoleName "trlhcalbnyeityk650ecsm56p42ej1yfdfe922lxsoa6afiq6pi90064gsrw5w6wk05lfxojs8fs"))}
testObject_MemberUpdateData_user_20 :: MemberUpdateData
testObject_MemberUpdateData_user_20 = MemberUpdateData {misTarget = Nothing, misOtrMuted = Just False, misOtrMutedStatus = Nothing, misOtrMutedRef = Nothing, misOtrArchived = Nothing, misOtrArchivedRef = Nothing, misHidden = Just True, misHiddenRef = Just "&\n", misConvRoleName = Just (fromJust (parseRoleName "xpjijo62jlf_u_b9_w8ij0r9viqz64e4gbt5k7mw"))}
