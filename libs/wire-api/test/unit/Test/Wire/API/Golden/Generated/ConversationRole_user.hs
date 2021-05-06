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
testObject_ConversationRole_user_1 = (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))
testObject_ConversationRole_user_2 :: ConversationRole
testObject_ConversationRole_user_2 = (fromJust (toConvRole (fromJust (parseRoleName "emhdajfaey4aai_fhdtjxpqr43w_7oj4r1kks4gs9mv4_fft5ya4")) (Just ((Actions (Set.fromList [RemoveConversationMember,ModifyConversationMessageTimer,ModifyConversationAccess,LeaveConversation]))))))
testObject_ConversationRole_user_3 :: ConversationRole
testObject_ConversationRole_user_3 = (fromJust (toConvRole (fromJust (parseRoleName "7xwvcumrdke7qd5dwms6d9a9p4w9u31m")) (Just ((Actions (Set.fromList [ModifyOtherConversationMember]))))))
testObject_ConversationRole_user_4 :: ConversationRole
testObject_ConversationRole_user_4 = (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))
testObject_ConversationRole_user_5 :: ConversationRole
testObject_ConversationRole_user_5 = (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))
testObject_ConversationRole_user_6 :: ConversationRole
testObject_ConversationRole_user_6 = (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))
testObject_ConversationRole_user_7 :: ConversationRole
testObject_ConversationRole_user_7 = (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))
testObject_ConversationRole_user_8 :: ConversationRole
testObject_ConversationRole_user_8 = (fromJust (toConvRole (fromJust (parseRoleName "slad32fr0wa36zef4yohnygl37gu15hz3_5o49mbqm7swi0h_p2m")) (Just ((Actions (Set.fromList []))))))
testObject_ConversationRole_user_9 :: ConversationRole
testObject_ConversationRole_user_9 = (fromJust (toConvRole (fromJust (parseRoleName "hlgh93xfuhufk7wzo2rrt2h98megoahgcu_cva1i09oz0dnuai11f4nv9svrwemkzul1gqvh37c64rk6bfjnrqc5hrdaazrbcphdmnpaqucj7xrm1vjr4r2158z")) (Just ((Actions (Set.fromList [AddConversationMember,ModifyConversationReceiptMode,ModifyConversationAccess,ModifyOtherConversationMember,DeleteConversation]))))))
testObject_ConversationRole_user_10 :: ConversationRole
testObject_ConversationRole_user_10 = (fromJust (toConvRole (fromJust (parseRoleName "o_cqaj3rqjydvwm8xnedb282wsgtid17z6e9045ytolb85duxjs8c_e0d92eu51su0")) (Just ((Actions (Set.fromList [ModifyConversationAccess,ModifyOtherConversationMember]))))))
testObject_ConversationRole_user_11 :: ConversationRole
testObject_ConversationRole_user_11 = (fromJust (toConvRole (fromJust (parseRoleName "4w5ddi26z4r_2_ewgtg_7o1sax6n7jteyxh4xa198aba6lzlptq83u7pkhy3iin13j074n")) (Just ((Actions (Set.fromList [AddConversationMember,RemoveConversationMember,ModifyConversationName,ModifyConversationMessageTimer,ModifyConversationReceiptMode,ModifyOtherConversationMember]))))))
testObject_ConversationRole_user_12 :: ConversationRole
testObject_ConversationRole_user_12 = (fromJust (toConvRole (fromJust (parseRoleName "f4saww533arwfcvkf0h28q71nrksxl3ifd4tkm5_eevgxy_s8t6q1utnc8_mm8h73kprr39l2tir3g0iswo")) (Just ((Actions (Set.fromList [AddConversationMember,ModifyConversationName,ModifyConversationMessageTimer,ModifyConversationReceiptMode,ModifyOtherConversationMember,LeaveConversation,DeleteConversation]))))))
testObject_ConversationRole_user_13 :: ConversationRole
testObject_ConversationRole_user_13 = (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))
testObject_ConversationRole_user_14 :: ConversationRole
testObject_ConversationRole_user_14 = (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))
testObject_ConversationRole_user_15 :: ConversationRole
testObject_ConversationRole_user_15 = (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))
testObject_ConversationRole_user_16 :: ConversationRole
testObject_ConversationRole_user_16 = (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))
testObject_ConversationRole_user_17 :: ConversationRole
testObject_ConversationRole_user_17 = (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))
testObject_ConversationRole_user_18 :: ConversationRole
testObject_ConversationRole_user_18 = (fromJust (toConvRole (fromJust (parseRoleName "799rjst9om767otai_gct3_vx0sjzt8anepxiln97gfgyv4n3rf3n4h8s1ui5qh1qp8wm5ij8mb3gvvuiqvj16rz65tg")) (Just ((Actions (Set.fromList [RemoveConversationMember,ModifyConversationName,ModifyConversationReceiptMode,ModifyOtherConversationMember]))))))
testObject_ConversationRole_user_19 :: ConversationRole
testObject_ConversationRole_user_19 = (fromJust (toConvRole (fromJust (parseRoleName "m5_nk38mcl2fb79wgf29wpth6u3rcspr708uliv0yhr4yff4rbmc6mlcubpi9auesgdqj7755nxwhj9n77dphp9ldhomoox41d7")) (Just ((Actions (Set.fromList [LeaveConversation,DeleteConversation]))))))
testObject_ConversationRole_user_20 :: ConversationRole
testObject_ConversationRole_user_20 = (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))
