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
testObject_MemberUpdateData_1 :: MemberUpdateData
testObject_MemberUpdateData_1 = MemberUpdateData {misTarget = Nothing, misOtrMuted = Nothing, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}), misOtrMutedRef = Just "kk", misOtrArchived = Just True, misOtrArchivedRef = Nothing, misHidden = Just True, misHiddenRef = Just ",\GSq", misConvRoleName = Just (fromJust (parseRoleName "6__yjj0j8ltr"))}
testObject_MemberUpdateData_2 :: MemberUpdateData
testObject_MemberUpdateData_2 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001"))), misOtrMuted = Just False, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), misOtrMutedRef = Just "", misOtrArchived = Just True, misOtrArchivedRef = Just "~>", misHidden = Nothing, misHiddenRef = Nothing, misConvRoleName = Just (fromJust (parseRoleName "6q0ab8"))}
testObject_MemberUpdateData_3 :: MemberUpdateData
testObject_MemberUpdateData_3 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000000"))), misOtrMuted = Just False, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), misOtrMutedRef = Just ".>\a", misOtrArchived = Nothing, misOtrArchivedRef = Just "", misHidden = Just False, misHiddenRef = Just "", misConvRoleName = Nothing}
testObject_MemberUpdateData_4 :: MemberUpdateData
testObject_MemberUpdateData_4 = MemberUpdateData {misTarget = Nothing, misOtrMuted = Just False, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -2}), misOtrMutedRef = Just "G", misOtrArchived = Just True, misOtrArchivedRef = Nothing, misHidden = Just False, misHiddenRef = Nothing, misConvRoleName = Just (fromJust (parseRoleName "wn_khitnw4xqqh_f2oc0gkhdw4_al5_6y35yf73zf3etb9cv2kmqpmiw5j2z0c3_c6i68v5k450wi_wklcpzdj"))}
testObject_MemberUpdateData_5 :: MemberUpdateData
testObject_MemberUpdateData_5 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000001"))), misOtrMuted = Just True, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), misOtrMutedRef = Nothing, misOtrArchived = Just True, misOtrArchivedRef = Just "", misHidden = Just False, misHiddenRef = Just "", misConvRoleName = Just (fromJust (parseRoleName "nv29l15dtaphbc_ddg0dj94463vgnh1ndvx9939fhfozkohavfhtmssnd8yl7orvpa20tagyi4uib4nqqxtg9zz54a6petunjr1ldbghnqe_hlfr6lk50i"))}
