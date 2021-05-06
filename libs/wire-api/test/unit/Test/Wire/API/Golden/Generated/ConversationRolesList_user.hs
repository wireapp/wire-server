{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ConversationRolesList_user where

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
testObject_ConversationRolesList_user_1 :: ConversationRolesList
testObject_ConversationRolesList_user_1 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "evdz5blimnlzmnt9dxri7md2llchm1xheorhw612fhdvkb")) (Just ((Actions (Set.fromList [ModifyOtherConversationMember])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))]}
testObject_ConversationRolesList_user_2 :: ConversationRolesList
testObject_ConversationRolesList_user_2 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "a0eromoyrbdovjd9cfjkl7tfuln5je6w6cc5asd20ue9ldhrts0ksphgcn8w0o1tj540rp8z63t3r9n2yyihwrwj4pbqjkygylvdrsk24horl82o4h5me7mxm")) (Just ((Actions (Set.fromList [ModifyConversationReceiptMode,LeaveConversation])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))]}
testObject_ConversationRolesList_user_3 :: ConversationRolesList
testObject_ConversationRolesList_user_3 = ConversationRolesList {convRolesList = []}
testObject_ConversationRolesList_user_4 :: ConversationRolesList
testObject_ConversationRolesList_user_4 = ConversationRolesList {convRolesList = []}
testObject_ConversationRolesList_user_5 :: ConversationRolesList
testObject_ConversationRolesList_user_5 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "kgnld2iykie0e5hgv8mvl2bec27eqdf5cy_wneei2ql6k1oixo95111uclqvq2fqmop45j9gdegyryv4o8zxm8ufceff25p7gnu_hjy874p6804myprg2r0e57mljml")) (Just ((Actions (Set.fromList [AddConversationMember])))))),(fromJust (toConvRole (fromJust (parseRoleName "2jhwfgdee0ygctx7u6uxd_tk6ye6zg0ba4")) (Just ((Actions (Set.fromList [ModifyConversationAccess])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "5pa9av1qqxjof")) (Just ((Actions (Set.fromList [LeaveConversation])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "41v5paut4mok7gg4w234m1i3ejdj9shzzl2htw4qa1w0avtgt8blr_iauaqg4nl9xu__ukw6_detidxui46xb5rzlckol7p1y5kn4vl20in9ych2qqz0lop2zhk29l2")) (Just ((Actions (Set.fromList [ModifyConversationName,DeleteConversation]))))))]}
