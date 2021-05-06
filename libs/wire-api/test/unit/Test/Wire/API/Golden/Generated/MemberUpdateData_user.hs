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
testObject_MemberUpdateData_user_1 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), misOtrMuted = Just True, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -2}), misOtrMutedRef = Just "r", misOtrArchived = Just True, misOtrArchivedRef = Just "$R", misHidden = Just False, misHiddenRef = Just "F\b", misConvRoleName = Just (fromJust (parseRoleName "gcxiwbab6o08vjspm9iwsi91cufkj_9akgs29p8t6f07k83hj52yg1fleke6e2qbmumvlqc3mrp3k2lztx6ut22"))}
testObject_MemberUpdateData_user_2 :: MemberUpdateData
testObject_MemberUpdateData_user_2 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000001"))), misOtrMuted = Just True, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}), misOtrMutedRef = Nothing, misOtrArchived = Just False, misOtrArchivedRef = Just "\SOH", misHidden = Just False, misHiddenRef = Just "\1046628.", misConvRoleName = Just (fromJust (parseRoleName "a06jqmz1t"))}
testObject_MemberUpdateData_user_3 :: MemberUpdateData
testObject_MemberUpdateData_user_3 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000002"))), misOtrMuted = Just True, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), misOtrMutedRef = Just "q", misOtrArchived = Nothing, misOtrArchivedRef = Just "\DC4d", misHidden = Just False, misHiddenRef = Nothing, misConvRoleName = Just (fromJust (parseRoleName "k7nbja3tm2m_zy4bu4w4eealwzx89__0_vft38rkhxf3hcdt9cco65qagrvc6trav5gtjflg5sa6itv38uvgfersp2_hnfuje4wu5"))}
testObject_MemberUpdateData_user_4 :: MemberUpdateData
testObject_MemberUpdateData_user_4 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000002"))), misOtrMuted = Just False, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), misOtrMutedRef = Just "\t", misOtrArchived = Just False, misOtrArchivedRef = Just ">\120656", misHidden = Just False, misHiddenRef = Just "Uj", misConvRoleName = Just (fromJust (parseRoleName "5ctfwqmn2w3j"))}
testObject_MemberUpdateData_user_5 :: MemberUpdateData
testObject_MemberUpdateData_user_5 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000002"))), misOtrMuted = Just False, misOtrMutedStatus = Nothing, misOtrMutedRef = Just "YhY", misOtrArchived = Just True, misOtrArchivedRef = Nothing, misHidden = Nothing, misHiddenRef = Just "", misConvRoleName = Just (fromJust (parseRoleName "xjlj60b"))}
testObject_MemberUpdateData_user_6 :: MemberUpdateData
testObject_MemberUpdateData_user_6 = MemberUpdateData {misTarget = Nothing, misOtrMuted = Just True, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 2}), misOtrMutedRef = Just "\1000378C/", misOtrArchived = Just True, misOtrArchivedRef = Just "#\SI\73669", misHidden = Just False, misHiddenRef = Just "\SI\1068752Y", misConvRoleName = Just (fromJust (parseRoleName "ut47l17t7pp3osyh5ydvtnrg_huhso7sddhvsuhi4uympqxs9u49no486y0kvj15znqquwqc_l2juzbc0kjeagn9nqb2f1zqx1wxo3wo4j_8jm"))}
testObject_MemberUpdateData_user_7 :: MemberUpdateData
testObject_MemberUpdateData_user_7 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000001"))), misOtrMuted = Just True, misOtrMutedStatus = Nothing, misOtrMutedRef = Just "", misOtrArchived = Just False, misOtrArchivedRef = Just "|g", misHidden = Just False, misHiddenRef = Just "$", misConvRoleName = Nothing}
testObject_MemberUpdateData_user_8 :: MemberUpdateData
testObject_MemberUpdateData_user_8 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000002"))), misOtrMuted = Just True, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 2}), misOtrMutedRef = Nothing, misOtrArchived = Just True, misOtrArchivedRef = Nothing, misHidden = Nothing, misHiddenRef = Just "^\999425\&1", misConvRoleName = Nothing}
testObject_MemberUpdateData_user_9 :: MemberUpdateData
testObject_MemberUpdateData_user_9 = MemberUpdateData {misTarget = Nothing, misOtrMuted = Just True, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), misOtrMutedRef = Just "%:", misOtrArchived = Just False, misOtrArchivedRef = Just "", misHidden = Just True, misHiddenRef = Just "", misConvRoleName = Just (fromJust (parseRoleName "0o_h1nwuw"))}
testObject_MemberUpdateData_user_10 :: MemberUpdateData
testObject_MemberUpdateData_user_10 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000001"))), misOtrMuted = Just True, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}), misOtrMutedRef = Just "0\1056492", misOtrArchived = Just True, misOtrArchivedRef = Just ">", misHidden = Nothing, misHiddenRef = Just "@\69688\139803", misConvRoleName = Nothing}
testObject_MemberUpdateData_user_11 :: MemberUpdateData
testObject_MemberUpdateData_user_11 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000002"))), misOtrMuted = Just True, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), misOtrMutedRef = Just "\917539\1066458", misOtrArchived = Just True, misOtrArchivedRef = Just "X{", misHidden = Just True, misHiddenRef = Nothing, misConvRoleName = Just (fromJust (parseRoleName "l52tyk96fvrm92i415pjvob1k5_99jxifqjaa9hpy6rk_r4erfl17l8eblxlb4epzx2bqy2jqakko"))}
testObject_MemberUpdateData_user_12 :: MemberUpdateData
testObject_MemberUpdateData_user_12 = MemberUpdateData {misTarget = Nothing, misOtrMuted = Just True, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -2}), misOtrMutedRef = Just "", misOtrArchived = Just True, misOtrArchivedRef = Just "\27596", misHidden = Just False, misHiddenRef = Just "2", misConvRoleName = Just (fromJust (parseRoleName "6o2anpqjnrp2zeugc4ah4pahym8t3gyvkxnsbo"))}
testObject_MemberUpdateData_user_13 :: MemberUpdateData
testObject_MemberUpdateData_user_13 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))), misOtrMuted = Just True, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), misOtrMutedRef = Just "\DC2\GS&", misOtrArchived = Just True, misOtrArchivedRef = Just "\FSiC", misHidden = Just True, misHiddenRef = Just "\1021580U\STX", misConvRoleName = Just (fromJust (parseRoleName "62iwey3s_jsrw9j8j6_jo2uz50o4_jwmvw8ut92dnxw6pntpqftrhq_rhzgdvey1rnciu2achy8snk51ndlof8q21ovctynpcx2v1ybb"))}
testObject_MemberUpdateData_user_14 :: MemberUpdateData
testObject_MemberUpdateData_user_14 = MemberUpdateData {misTarget = Nothing, misOtrMuted = Just True, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -2}), misOtrMutedRef = Just ";\94917", misOtrArchived = Nothing, misOtrArchivedRef = Just "Fj", misHidden = Just False, misHiddenRef = Just "\1081415Js", misConvRoleName = Just (fromJust (parseRoleName "_renigb6ysvaommhgydwwawmlyk02lwes475nfx6igr7ki_rwr4vawuk9grouxc8u9dz5gc453er24vbfj06r_te2d9dz3udz16p3kf92684bd7vzoo"))}
testObject_MemberUpdateData_user_15 :: MemberUpdateData
testObject_MemberUpdateData_user_15 = MemberUpdateData {misTarget = Nothing, misOtrMuted = Nothing, misOtrMutedStatus = Nothing, misOtrMutedRef = Just "G\174732\SI", misOtrArchived = Just False, misOtrArchivedRef = Just "\181988X", misHidden = Just False, misHiddenRef = Just "|", misConvRoleName = Nothing}
testObject_MemberUpdateData_user_16 :: MemberUpdateData
testObject_MemberUpdateData_user_16 = MemberUpdateData {misTarget = Nothing, misOtrMuted = Just True, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), misOtrMutedRef = Nothing, misOtrArchived = Just False, misOtrArchivedRef = Just "\GS\1070261", misHidden = Just False, misHiddenRef = Just "l\DEL", misConvRoleName = Just (fromJust (parseRoleName "fi8ofv5ktxe"))}
testObject_MemberUpdateData_user_17 :: MemberUpdateData
testObject_MemberUpdateData_user_17 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), misOtrMuted = Just True, misOtrMutedStatus = Nothing, misOtrMutedRef = Nothing, misOtrArchived = Just False, misOtrArchivedRef = Just "+\fv", misHidden = Nothing, misHiddenRef = Nothing, misConvRoleName = Just (fromJust (parseRoleName "i6pms8gyob7y7dd4gmlcspwnrsem0tw6hhgjbx_yq75owevc6pna836"))}
testObject_MemberUpdateData_user_18 :: MemberUpdateData
testObject_MemberUpdateData_user_18 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000000"))), misOtrMuted = Nothing, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}), misOtrMutedRef = Just "R#\DC3", misOtrArchived = Just True, misOtrArchivedRef = Just "\189604b6", misHidden = Just True, misHiddenRef = Just "\2748", misConvRoleName = Just (fromJust (parseRoleName "f7fp9wqlbqzcioqefy636w51tx_xmo11viz8amilx6cg675p3n623i_iqeqaqmw5vek2yhomauezwvlw5sm0ipwy"))}
testObject_MemberUpdateData_user_19 :: MemberUpdateData
testObject_MemberUpdateData_user_19 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000002"))), misOtrMuted = Nothing, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 2}), misOtrMutedRef = Just "\1052865", misOtrArchived = Just True, misOtrArchivedRef = Just "\181607 ", misHidden = Just False, misHiddenRef = Just "U\SIt", misConvRoleName = Just (fromJust (parseRoleName "biwgv5mec_4q6l81fdh3115tlg_ph2g2vkbrmiko"))}
testObject_MemberUpdateData_user_20 :: MemberUpdateData
testObject_MemberUpdateData_user_20 = MemberUpdateData {misTarget = Nothing, misOtrMuted = Just True, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}), misOtrMutedRef = Just "\161722h]", misOtrArchived = Just True, misOtrArchivedRef = Just "U", misHidden = Just True, misHiddenRef = Nothing, misConvRoleName = Just (fromJust (parseRoleName "9c0p9w1mjc3ejwm83hbekpl99cems4a236vtye5d_kvtf"))}
