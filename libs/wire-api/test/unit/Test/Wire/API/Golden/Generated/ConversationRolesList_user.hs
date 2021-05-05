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
testObject_ConversationRolesList_1 :: ConversationRolesList
testObject_ConversationRolesList_1 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "ckfuji6cqk5n4a")) (Just ((Actions (Set.fromList [ModifyOtherConversationMember])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))]}
testObject_ConversationRolesList_2 :: ConversationRolesList
testObject_ConversationRolesList_2 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "pbydp5")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "hie3_c71a9087ol3uwgsuzj389gisxe4na9w8o21yk3bdl8avjzg_3_npv59b6nhp6tggfsc6kd2n4fdle9ljwncea7z6e7_pf6vc2_cd8iq_76pvyyxddkyr4eui709")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))]}
testObject_ConversationRolesList_3 :: ConversationRolesList
testObject_ConversationRolesList_3 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "284nm2t3xrbxp5z36o69ycnlgnstga3g_1lb1ucr0_8plwyhvsws")) (Just ((Actions (Set.fromList [ModifyConversationAccess,LeaveConversation]))))))]}
testObject_ConversationRolesList_4 :: ConversationRolesList
testObject_ConversationRolesList_4 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "7zfgob1y9bxchmah_0r_px5u_tpn3yyp9jom88acsl0ocr951jw92bkeo0uu15ot5mag2ravvre4o893_8wosayhp6x8olfwpdym4oai05x71s2d49tfjus45z_cy2")) (Just ((Actions (Set.fromList [ModifyConversationReceiptMode,ModifyOtherConversationMember])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "pt7l5a1nd7qpg4hbpijmbjo6ppl4odmmc3qy3sz7jvb8l4qrqbl8sq6gdh5a10mhdayk9d3zsgdj5vobzpxmizh7m7w84lo8837zku")) (Just ((Actions (Set.fromList [ModifyConversationMessageTimer]))))))]}
testObject_ConversationRolesList_5 :: ConversationRolesList
testObject_ConversationRolesList_5 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "ad41xkj9hhcrk12612w55kn564f13xtd3_30wulg2sst7p711f4_fim04khzo7qyger06q4tvmpjd3z7kmr9qf9lvebomkvbe")) (Just ((Actions (Set.fromList [LeaveConversation])))))),(fromJust (toConvRole (fromJust (parseRoleName "f3zjbasyzui6dz_xftpz09hf6_jvhg1lki29r4t_04k1un09ined4c3vxwrut_dw9mmhtai2pi_bkd4zat46sofi8n8vcg03b836lycw1lriqlfqaq3ydcr87xc")) (Just ((Actions (Set.fromList [ModifyConversationName,DeleteConversation])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "7djnkax9vmen02cebc52y0wak7jhq72dai2nffqosexfmiuvuk6dnvgemgy69d30a2xeomgpw")) (Just ((Actions (Set.fromList [ModifyConversationReceiptMode,DeleteConversation]))))))]}
