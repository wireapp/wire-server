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
testObject_MemberUpdateData_user_1 = MemberUpdateData {misTarget = Nothing, misOtrMuted = Nothing, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), misOtrMutedRef = Nothing, misOtrArchived = Nothing, misOtrArchivedRef = Nothing, misHidden = Just False, misHiddenRef = Nothing, misConvRoleName = Just (fromJust (parseRoleName "2ydjly85g9mkgzn32q94aj5v0wclas7eje30c5__5g47yy5q996sdwisk4h0iatpag2p2j66zeeyfvg1mwcq3j7ufi1g"))}
testObject_MemberUpdateData_user_2 :: MemberUpdateData
testObject_MemberUpdateData_user_2 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), misOtrMuted = Just True, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), misOtrMutedRef = Just "", misOtrArchived = Just True, misOtrArchivedRef = Just "", misHidden = Just False, misHiddenRef = Just "", misConvRoleName = Nothing}
testObject_MemberUpdateData_user_3 :: MemberUpdateData
testObject_MemberUpdateData_user_3 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000002"))), misOtrMuted = Nothing, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -2}), misOtrMutedRef = Just "", misOtrArchived = Nothing, misOtrArchivedRef = Just "", misHidden = Just True, misHiddenRef = Just "", misConvRoleName = Nothing}
testObject_MemberUpdateData_user_4 :: MemberUpdateData
testObject_MemberUpdateData_user_4 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000002"))), misOtrMuted = Just True, misOtrMutedStatus = Nothing, misOtrMutedRef = Just "", misOtrArchived = Nothing, misOtrArchivedRef = Just "\1058198", misHidden = Just True, misHiddenRef = Just "O\1091369", misConvRoleName = Just (fromJust (parseRoleName "vt_iu7lrdyk3s6_w_bvc4guqcjsz1ae5kg_3b_nw0udp_fuxyyka5w58qdjqryn2ibkbhjdu"))}
testObject_MemberUpdateData_user_5 :: MemberUpdateData
testObject_MemberUpdateData_user_5 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))), misOtrMuted = Nothing, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), misOtrMutedRef = Just "`O", misOtrArchived = Just False, misOtrArchivedRef = Just "\55148\1063465", misHidden = Just True, misHiddenRef = Just "\STX\188462y", misConvRoleName = Just (fromJust (parseRoleName "9jwc5ypfpxgoqwajwz7xm3nyt3e9gnvciebha0be_4ln0qvjhy_4bec5l3dwxsygnf611s9fcsns"))}
