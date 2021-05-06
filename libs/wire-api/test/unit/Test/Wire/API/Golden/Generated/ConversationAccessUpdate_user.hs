{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ConversationAccessUpdate_user where

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
testObject_ConversationAccessUpdate_user_1 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_1 = ConversationAccessUpdate {cupAccess = [CodeAccess,PrivateAccess,InviteAccess,InviteAccess,PrivateAccess,InviteAccess,CodeAccess,InviteAccess], cupAccessRole = ActivatedAccessRole}
testObject_ConversationAccessUpdate_user_2 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_2 = ConversationAccessUpdate {cupAccess = [], cupAccessRole = ActivatedAccessRole}
testObject_ConversationAccessUpdate_user_3 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_3 = ConversationAccessUpdate {cupAccess = [LinkAccess,CodeAccess,InviteAccess,InviteAccess,LinkAccess,LinkAccess], cupAccessRole = NonActivatedAccessRole}
testObject_ConversationAccessUpdate_user_4 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_4 = ConversationAccessUpdate {cupAccess = [InviteAccess,LinkAccess,CodeAccess,PrivateAccess,LinkAccess,InviteAccess,CodeAccess,InviteAccess,CodeAccess,CodeAccess,LinkAccess,LinkAccess,PrivateAccess,InviteAccess,InviteAccess,CodeAccess,LinkAccess], cupAccessRole = ActivatedAccessRole}
testObject_ConversationAccessUpdate_user_5 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_5 = ConversationAccessUpdate {cupAccess = [PrivateAccess,LinkAccess,CodeAccess,InviteAccess,CodeAccess], cupAccessRole = TeamAccessRole}
testObject_ConversationAccessUpdate_user_6 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_6 = ConversationAccessUpdate {cupAccess = [LinkAccess,CodeAccess], cupAccessRole = PrivateAccessRole}
testObject_ConversationAccessUpdate_user_7 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_7 = ConversationAccessUpdate {cupAccess = [], cupAccessRole = ActivatedAccessRole}
testObject_ConversationAccessUpdate_user_8 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_8 = ConversationAccessUpdate {cupAccess = [CodeAccess,LinkAccess,CodeAccess,LinkAccess,CodeAccess,CodeAccess,CodeAccess], cupAccessRole = PrivateAccessRole}
testObject_ConversationAccessUpdate_user_9 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_9 = ConversationAccessUpdate {cupAccess = [], cupAccessRole = TeamAccessRole}
testObject_ConversationAccessUpdate_user_10 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_10 = ConversationAccessUpdate {cupAccess = [], cupAccessRole = NonActivatedAccessRole}
testObject_ConversationAccessUpdate_user_11 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_11 = ConversationAccessUpdate {cupAccess = [LinkAccess,LinkAccess,LinkAccess,CodeAccess], cupAccessRole = TeamAccessRole}
testObject_ConversationAccessUpdate_user_12 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_12 = ConversationAccessUpdate {cupAccess = [PrivateAccess], cupAccessRole = ActivatedAccessRole}
testObject_ConversationAccessUpdate_user_13 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_13 = ConversationAccessUpdate {cupAccess = [LinkAccess,InviteAccess,LinkAccess], cupAccessRole = TeamAccessRole}
testObject_ConversationAccessUpdate_user_14 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_14 = ConversationAccessUpdate {cupAccess = [], cupAccessRole = NonActivatedAccessRole}
testObject_ConversationAccessUpdate_user_15 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_15 = ConversationAccessUpdate {cupAccess = [LinkAccess,PrivateAccess,InviteAccess,CodeAccess,CodeAccess,PrivateAccess,LinkAccess,CodeAccess], cupAccessRole = NonActivatedAccessRole}
testObject_ConversationAccessUpdate_user_16 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_16 = ConversationAccessUpdate {cupAccess = [LinkAccess,PrivateAccess], cupAccessRole = NonActivatedAccessRole}
testObject_ConversationAccessUpdate_user_17 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_17 = ConversationAccessUpdate {cupAccess = [], cupAccessRole = ActivatedAccessRole}
testObject_ConversationAccessUpdate_user_18 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_18 = ConversationAccessUpdate {cupAccess = [PrivateAccess,LinkAccess,CodeAccess,LinkAccess], cupAccessRole = TeamAccessRole}
testObject_ConversationAccessUpdate_user_19 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_19 = ConversationAccessUpdate {cupAccess = [InviteAccess,InviteAccess,InviteAccess,InviteAccess,CodeAccess,CodeAccess,LinkAccess,InviteAccess,CodeAccess], cupAccessRole = ActivatedAccessRole}
testObject_ConversationAccessUpdate_user_20 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_20 = ConversationAccessUpdate {cupAccess = [CodeAccess], cupAccessRole = NonActivatedAccessRole}
