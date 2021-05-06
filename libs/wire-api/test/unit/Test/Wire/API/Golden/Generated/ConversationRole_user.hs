{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ConversationRole_user where

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
testObject_ConversationRole_user_1 :: ConversationRole
testObject_ConversationRole_user_1 = (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))
testObject_ConversationRole_user_2 :: ConversationRole
testObject_ConversationRole_user_2 = (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))
testObject_ConversationRole_user_3 :: ConversationRole
testObject_ConversationRole_user_3 = (fromJust (toConvRole (fromJust (parseRoleName "5pgkos7w65e15qg2xjjcs8rwuiqzu0zrsv_sk7rfcsuohqhb52p93hwlpztg7alvtjltnsmhca7_eoaj6a596p8rqi62zqbaihm7h00_e32po7ez0lqqj2i2")) (Just ((Actions (Set.fromList [AddConversationMember,ModifyConversationName,ModifyConversationMessageTimer,ModifyConversationReceiptMode,LeaveConversation,DeleteConversation]))))))
testObject_ConversationRole_user_4 :: ConversationRole
testObject_ConversationRole_user_4 = (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))
testObject_ConversationRole_user_5 :: ConversationRole
testObject_ConversationRole_user_5 = (fromJust (toConvRole (fromJust (parseRoleName "v204frzw8mc1uoczxaclgbwwk8vz34slfkrjvqyuutath4lwz_3ucmrxt5mz0q48w")) (Just ((Actions (Set.fromList [RemoveConversationMember]))))))
testObject_ConversationRole_user_6 :: ConversationRole
testObject_ConversationRole_user_6 = (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))
testObject_ConversationRole_user_7 :: ConversationRole
testObject_ConversationRole_user_7 = (fromJust (toConvRole (fromJust (parseRoleName "9whm34xkv0kz5pep6k3gm_kdacn_8pv45c9v3ctu96_lzhlnv7_vbrel1hpaqubqy_91r3mi")) (Just ((Actions (Set.fromList [AddConversationMember,ModifyConversationName,ModifyConversationMessageTimer,ModifyConversationAccess,ModifyOtherConversationMember,LeaveConversation]))))))
testObject_ConversationRole_user_8 :: ConversationRole
testObject_ConversationRole_user_8 = (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))
testObject_ConversationRole_user_9 :: ConversationRole
testObject_ConversationRole_user_9 = (fromJust (toConvRole (fromJust (parseRoleName "1mj5li37802a94183b_9ie_5z7oww1bweryt5ob7g0bj7tomb4rsw84h3017k813getn6_pb1jrswy91z6l7x63wa0cf3huv0g5")) (Just ((Actions (Set.fromList [RemoveConversationMember,ModifyConversationName,ModifyConversationMessageTimer,ModifyConversationReceiptMode,ModifyConversationAccess,ModifyOtherConversationMember,LeaveConversation,DeleteConversation]))))))
testObject_ConversationRole_user_10 :: ConversationRole
testObject_ConversationRole_user_10 = (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))
testObject_ConversationRole_user_11 :: ConversationRole
testObject_ConversationRole_user_11 = (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))
testObject_ConversationRole_user_12 :: ConversationRole
testObject_ConversationRole_user_12 = (fromJust (toConvRole (fromJust (parseRoleName "9tnq6215l")) (Just ((Actions (Set.fromList [ModifyConversationName,ModifyConversationReceiptMode]))))))
testObject_ConversationRole_user_13 :: ConversationRole
testObject_ConversationRole_user_13 = (fromJust (toConvRole (fromJust (parseRoleName "313y9fglav7fug3kycv3xswxg_mqkpeqwz3u6hnhs_j3beakk4_fro67653lis7tu43glc")) (Just ((Actions (Set.fromList [RemoveConversationMember,ModifyConversationName,ModifyConversationAccess,ModifyOtherConversationMember,LeaveConversation,DeleteConversation]))))))
testObject_ConversationRole_user_14 :: ConversationRole
testObject_ConversationRole_user_14 = (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))
testObject_ConversationRole_user_15 :: ConversationRole
testObject_ConversationRole_user_15 = (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))
testObject_ConversationRole_user_16 :: ConversationRole
testObject_ConversationRole_user_16 = (fromJust (toConvRole (fromJust (parseRoleName "pqp41knfqumxin")) (Just ((Actions (Set.fromList [AddConversationMember,ModifyConversationMessageTimer,ModifyConversationAccess,ModifyOtherConversationMember]))))))
testObject_ConversationRole_user_17 :: ConversationRole
testObject_ConversationRole_user_17 = (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))
testObject_ConversationRole_user_18 :: ConversationRole
testObject_ConversationRole_user_18 = (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))
testObject_ConversationRole_user_19 :: ConversationRole
testObject_ConversationRole_user_19 = (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))
testObject_ConversationRole_user_20 :: ConversationRole
testObject_ConversationRole_user_20 = (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))
