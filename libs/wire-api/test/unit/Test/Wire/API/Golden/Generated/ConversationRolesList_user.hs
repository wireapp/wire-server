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
testObject_ConversationRolesList_user_1 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "1_jzp7te67ix5ib3ps8h9ifq6yp76yq5y9")) (Just ((Actions (Set.fromList [ModifyConversationReceiptMode,ModifyOtherConversationMember])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "v92xidyxzd61nsyekz_622u_1sc_ndb1hqezncnaxizex")) (Just ((Actions (Set.fromList [DeleteConversation])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))]}
testObject_ConversationRolesList_user_2 :: ConversationRolesList
testObject_ConversationRolesList_user_2 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))]}
testObject_ConversationRolesList_user_3 :: ConversationRolesList
testObject_ConversationRolesList_user_3 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "o1a40i0wqzgvt8jy5x1rmuenx7akbm")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "8hp795r8258g7s61e72bwenrukje5zp5eku_x_3j5uheby701a5pxpxc6ysni19z5dgga27yn1yylmi0acbkj8hmu_2essd7xe0rtlre_ggf74z5ub")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "epyeipzkt6ga92dvtcq29mgak4jw25ow2736gro8sbofjpi1b_3xzmr050gz9_aj4raut08l5vb8gse7")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "c8gbjk739uwy75cd0lgw8ca5dtyai8kxqw0p8784lsmmb3whclu2hqe")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "jslj379kewae_wi2q3jk1yuzhx")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))]}
testObject_ConversationRolesList_user_4 :: ConversationRolesList
testObject_ConversationRolesList_user_4 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "_ll")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))]}
testObject_ConversationRolesList_user_5 :: ConversationRolesList
testObject_ConversationRolesList_user_5 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "dme74a2d328b8wab4gtim")) (Just ((Actions (Set.fromList [ModifyConversationMessageTimer,LeaveConversation]))))))]}
