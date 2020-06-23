-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Test.Wire.API.Roundtrip.Aeson (tests) where

import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON)
import Data.Aeson.Types (parseEither)
import Data.Id (ConvId)
import Imports
import qualified Test.Tasty as T
import Test.Tasty.ExpectedFailure (ignoreTest)
import Test.Tasty.QuickCheck (Arbitrary, counterexample, testProperty, (===))
import Type.Reflection (typeRep)
import qualified Wire.API.Asset as Asset
import qualified Wire.API.Asset.V3.Resumable as Asset.Resumable
import qualified Wire.API.Call.TURN as Call.TURN
import qualified Wire.API.Connection as Connection
import qualified Wire.API.Conversation as Conversation
import qualified Wire.API.Conversation.Bot as Conversation.Bot
import qualified Wire.API.Conversation.Code as Conversation.Code
import qualified Wire.API.Conversation.Member as Conversation.Member
import qualified Wire.API.Conversation.Role as Conversation.Role
import qualified Wire.API.Conversation.Typing as Conversation.Typing
import qualified Wire.API.CustomBackend as CustomBackend
import qualified Wire.API.Event.Conversation as Event.Conversation
import qualified Wire.API.Event.Team as Event.Team
import qualified Wire.API.Message as Message
import qualified Wire.API.Notification as Notification
import qualified Wire.API.Properties as Properties
import qualified Wire.API.Provider as Provider
import qualified Wire.API.Provider.Bot as Provider.Bot
import qualified Wire.API.Provider.External as Provider.External
import qualified Wire.API.Provider.Service as Provider.Service
import qualified Wire.API.Provider.Service.Tag as Provider.Service.Tag
import qualified Wire.API.Push.Token as Push.Token
import qualified Wire.API.Team as Team
import qualified Wire.API.Team.Conversation as Team.Conversation
import qualified Wire.API.Team.Feature as Team.Feature
import qualified Wire.API.Team.Invitation as Team.Invitation
import qualified Wire.API.Team.LegalHold as Team.LegalHold
import qualified Wire.API.Team.LegalHold.External as Team.LegalHold.External
import qualified Wire.API.Team.Member as Team.Member
import qualified Wire.API.Team.Permission as Team.Permission
import qualified Wire.API.Team.Role as Team.Role
import qualified Wire.API.Team.SearchVisibility as Team.SearchVisibility
import qualified Wire.API.User as User
import qualified Wire.API.User.Activation as User.Activation
import qualified Wire.API.User.Auth as User.Auth
import qualified Wire.API.User.Client as User.Client
import qualified Wire.API.User.Client.Prekey as User.Client.Prekey
import qualified Wire.API.User.Handle as User.Handle
import qualified Wire.API.User.Identity as User.Identity
import qualified Wire.API.User.Password as User.Password
import qualified Wire.API.User.Profile as User.Profile
import qualified Wire.API.User.RichInfo as User.RichInfo
import qualified Wire.API.User.Search as User.Search

-- FUTUREWORK(#1446): fix tests marked as failing
-- (either fixing Arbitrary or serialization instance)
tests :: T.TestTree
tests =
  T.localOption (T.Timeout (60 * 1000000) "60s") . T.testGroup "JSON roundtrip tests" $
    [ testRoundTrip @Asset.AssetToken,
      testRoundTrip @Asset.NewAssetToken,
      testRoundTrip @Asset.AssetRetention,
      testRoundTrip @Asset.AssetSettings,
      testRoundTrip @Asset.AssetKey,
      currentlyFailing (testRoundTrip @Asset.Asset), -- because ToJSON is rounding UTCTime
      testRoundTrip @Asset.Resumable.ResumableSettings,
      testRoundTrip @Asset.Resumable.TotalSize,
      testRoundTrip @Asset.Resumable.ChunkSize,
      testRoundTrip @Asset.Resumable.Offset,
      currentlyFailing (testRoundTrip @Asset.Resumable.ResumableAsset), -- because ToJSON is rounding UTCTime
      testRoundTrip @Call.TURN.TurnHost,
      testRoundTrip @Call.TURN.Scheme,
      testRoundTrip @Call.TURN.Transport,
      testRoundTrip @Call.TURN.TurnURI,
      testRoundTrip @Call.TURN.TurnUsername,
      testRoundTrip @Call.TURN.RTCIceServer,
      testRoundTrip @Call.TURN.RTCConfiguration,
      testRoundTrip @Connection.ConnectionRequest,
      testRoundTrip @Connection.Relation,
      testRoundTrip @Connection.Message,
      testRoundTrip @Connection.UserConnection,
      testRoundTrip @Connection.UserConnectionList,
      testRoundTrip @Connection.ConnectionUpdate,
      currentlyFailing (testRoundTrip @Conversation.Conversation), -- flaky, fails for large sizes because of rounding error in cnvMessageTimer
      currentlyFailing (testRoundTrip @Conversation.NewConvUnmanaged),
      currentlyFailing (testRoundTrip @Conversation.NewConvManaged),
      testRoundTrip @(Conversation.ConversationList ConvId),
      testRoundTrip @(Conversation.ConversationList Conversation.Conversation),
      testRoundTrip @Conversation.Access,
      testRoundTrip @Conversation.AccessRole,
      testRoundTrip @Conversation.ConvType,
      testRoundTrip @Conversation.ReceiptMode,
      testRoundTrip @Conversation.ConvTeamInfo,
      testRoundTrip @Conversation.Invite,
      testRoundTrip @Conversation.ConversationRename,
      testRoundTrip @Conversation.ConversationAccessUpdate,
      testRoundTrip @Conversation.ConversationReceiptModeUpdate,
      currentlyFailing (testRoundTrip @Conversation.ConversationMessageTimerUpdate),
      testRoundTrip @Conversation.Bot.AddBot,
      currentlyFailing (testRoundTrip @Conversation.Bot.AddBotResponse),
      currentlyFailing (testRoundTrip @Conversation.Bot.RemoveBotResponse),
      testRoundTrip @Conversation.Bot.UpdateBotPrekeys,
      testRoundTrip @Conversation.Code.ConversationCode,
      currentlyFailing (testRoundTrip @Conversation.Member.MemberUpdate),
      testRoundTrip @Conversation.Member.MutedStatus,
      testRoundTrip @Conversation.Member.Member,
      testRoundTrip @Conversation.Member.OtherMember,
      testRoundTrip @Conversation.Member.ConvMembers,
      currentlyFailing (testRoundTrip @Conversation.Member.OtherMemberUpdate),
      testRoundTrip @Conversation.Role.RoleName,
      testRoundTrip @Conversation.Role.Action,
      testRoundTrip @Conversation.Role.ConversationRole,
      testRoundTrip @Conversation.Role.ConversationRolesList,
      testRoundTrip @Conversation.Typing.TypingStatus,
      testRoundTrip @Conversation.Typing.TypingData,
      testRoundTrip @CustomBackend.CustomBackend,
      currentlyFailing (testRoundTrip @Event.Conversation.Event), -- because ToJSON is rounding UTCTime
      testRoundTrip @Event.Conversation.EventType,
      testRoundTrip @Event.Conversation.SimpleMember,
      testRoundTrip @Event.Conversation.SimpleMembers,
      testRoundTrip @Event.Conversation.Connect,
      testRoundTrip @Event.Conversation.MemberUpdateData,
      testRoundTrip @Event.Conversation.OtrMessage,
      currentlyFailing (testRoundTrip @Event.Team.Event), -- flaky, fails because of TeamUpdateData
      testRoundTrip @Event.Team.EventType,
      testRoundTrip @Message.Priority,
      testRoundTrip @Message.OtrRecipients,
      testRoundTrip @Message.NewOtrMessage,
      currentlyFailing (testRoundTrip @Message.ClientMismatch), -- because ToJSON is rounding UTCTime
      testRoundTrip @Notification.QueuedNotification,
      testRoundTrip @Notification.QueuedNotificationList,
      testRoundTrip @Properties.PropertyKey,
      testRoundTrip @Properties.PropertyValue,
      testRoundTrip @Provider.Provider,
      testRoundTrip @Provider.ProviderProfile,
      testRoundTrip @Provider.NewProvider,
      testRoundTrip @Provider.NewProviderResponse,
      testRoundTrip @Provider.UpdateProvider,
      testRoundTrip @Provider.ProviderActivationResponse,
      testRoundTrip @Provider.ProviderLogin,
      testRoundTrip @Provider.DeleteProvider,
      testRoundTrip @Provider.PasswordReset,
      testRoundTrip @Provider.CompletePasswordReset,
      testRoundTrip @Provider.PasswordChange,
      testRoundTrip @Provider.EmailUpdate,
      testRoundTrip @Provider.Bot.BotConvView,
      testRoundTrip @Provider.Bot.BotUserView,
      testRoundTrip @Provider.External.NewBotRequest,
      testRoundTrip @Provider.External.NewBotResponse,
      testRoundTrip @Provider.Service.ServiceRef,
      testRoundTrip @Provider.Service.ServiceKeyPEM,
      testRoundTrip @Provider.Service.ServiceKeyType,
      testRoundTrip @Provider.Service.ServiceKey,
      testRoundTrip @Provider.Service.ServiceToken,
      testRoundTrip @Provider.Service.Service,
      testRoundTrip @Provider.Service.ServiceProfile,
      testRoundTrip @Provider.Service.ServiceProfilePage,
      testRoundTrip @Provider.Service.NewService,
      testRoundTrip @Provider.Service.NewServiceResponse,
      testRoundTrip @Provider.Service.UpdateService,
      testRoundTrip @Provider.Service.UpdateServiceConn,
      testRoundTrip @Provider.Service.DeleteService,
      testRoundTrip @Provider.Service.UpdateServiceWhitelist,
      testRoundTrip @Provider.Service.Tag.ServiceTag,
      testRoundTrip @Provider.Service.Tag.ServiceTagList,
      testRoundTrip @Push.Token.Transport,
      testRoundTrip @Push.Token.Token,
      testRoundTrip @Push.Token.AppName,
      testRoundTrip @Push.Token.PushToken,
      testRoundTrip @Push.Token.PushTokenList,
      testRoundTrip @Team.BindingNewTeam,
      testRoundTrip @Team.TeamBinding,
      testRoundTrip @Team.Team,
      testRoundTrip @Team.TeamList,
      currentlyFailing (testRoundTrip @Team.TeamUpdateData), -- "no update data specified" if all fields are 'Nothing'
      testRoundTrip @Team.TeamDeleteData,
      testRoundTrip @Team.Conversation.TeamConversation,
      testRoundTrip @Team.Conversation.TeamConversationList,
      testRoundTrip @Team.Feature.TeamFeatureStatus,
      testRoundTrip @Team.Feature.TeamFeatureStatusValue,
      testRoundTrip @Team.Invitation.InvitationRequest,
      testRoundTrip @Team.Invitation.Invitation,
      testRoundTrip @Team.Invitation.InvitationList,
      testRoundTrip @Team.LegalHold.NewLegalHoldService,
      testRoundTrip @Team.LegalHold.ViewLegalHoldServiceInfo,
      testRoundTrip @Team.LegalHold.NewLegalHoldService,
      testRoundTrip @Team.LegalHold.ViewLegalHoldService,
      testRoundTrip @Team.LegalHold.UserLegalHoldStatusResponse,
      testRoundTrip @Team.LegalHold.RemoveLegalHoldSettingsRequest,
      testRoundTrip @Team.LegalHold.DisableLegalHoldForUserRequest,
      testRoundTrip @Team.LegalHold.ApproveLegalHoldForUserRequest,
      testRoundTrip @Team.LegalHold.External.RequestNewLegalHoldClient,
      testRoundTrip @Team.LegalHold.External.NewLegalHoldClient,
      testRoundTrip @Team.LegalHold.External.LegalHoldServiceConfirm,
      testRoundTrip @Team.LegalHold.External.LegalHoldServiceRemove,
      testRoundTrip @Team.Member.TeamMember,
      testRoundTrip @Team.Member.ListType,
      testRoundTrip @Team.Member.TeamMemberList,
      testRoundTrip @Team.Member.NewTeamMember,
      testRoundTrip @Team.Member.TeamMemberDeleteData,
      testRoundTrip @Team.Permission.Permissions,
      testRoundTrip @Team.Role.Role,
      testRoundTrip @Team.SearchVisibility.TeamSearchVisibility,
      testRoundTrip @Team.SearchVisibility.TeamSearchVisibilityView,
      testRoundTrip @User.NewUser,
      testRoundTrip @User.NewUserPublic,
      testRoundTrip @User.UserIdList,
      testRoundTrip @User.UserProfile,
      testRoundTrip @User.User,
      testRoundTrip @User.SelfProfile,
      testRoundTrip @User.InvitationCode,
      testRoundTrip @User.BindingNewTeamUser,
      -- FUTUREWORK: this should probably be tested individually,
      -- but NewUserOrigin doesn't have JSON instances, just plain functions.
      -- testRoundTrip @User.NewUserOrigin,
      testRoundTrip @User.UserUpdate,
      testRoundTrip @User.PasswordChange,
      testRoundTrip @User.LocaleUpdate,
      testRoundTrip @User.EmailUpdate,
      testRoundTrip @User.PhoneUpdate,
      testRoundTrip @User.HandleUpdate,
      testRoundTrip @User.DeleteUser,
      testRoundTrip @User.VerifyDeleteUser,
      testRoundTrip @User.DeletionCodeTimeout,
      testRoundTrip @User.Activation.ActivationKey,
      -- FUTUREWORK: this should probably be tested individually,
      -- but ActivationTarget currently doesn't have JSON instances itself.
      -- testRoundTrip @User.Activation.ActivationTarget,
      testRoundTrip @User.Activation.ActivationCode,
      testRoundTrip @User.Activation.Activate,
      testRoundTrip @User.Activation.ActivationResponse,
      testRoundTrip @User.Activation.SendActivationCode,
      testRoundTrip @User.Auth.LoginId,
      testRoundTrip @User.Auth.LoginCode,
      testRoundTrip @User.Auth.PendingLoginCode,
      testRoundTrip @User.Auth.SendLoginCode,
      testRoundTrip @User.Auth.LoginCodeTimeout,
      testRoundTrip @User.Auth.CookieLabel,
      testRoundTrip @User.Auth.Login,
      testRoundTrip @User.Auth.CookieId,
      testRoundTrip @User.Auth.CookieType,
      testRoundTrip @(User.Auth.Cookie ()),
      testRoundTrip @User.Auth.CookieList,
      testRoundTrip @User.Auth.RemoveCookies,
      testRoundTrip @User.Auth.TokenType,
      testRoundTrip @User.Auth.AccessToken,
      testRoundTrip @(User.Client.UserClientMap Int),
      testRoundTrip @User.Client.UserClients,
      testRoundTrip @User.Client.ClientType,
      testRoundTrip @User.Client.ClientClass,
      testRoundTrip @User.Client.PubClient,
      testRoundTrip @User.Client.Client,
      testRoundTrip @User.Client.NewClient,
      testRoundTrip @User.Client.UpdateClient,
      testRoundTrip @User.Client.RmClient,
      testRoundTrip @User.Client.Prekey.LastPrekey,
      testRoundTrip @User.Client.Prekey.PrekeyId,
      testRoundTrip @User.Client.Prekey.Prekey,
      testRoundTrip @User.Client.Prekey.ClientPrekey,
      testRoundTrip @User.Client.Prekey.PrekeyBundle,
      testRoundTrip @User.Handle.UserHandleInfo,
      testRoundTrip @User.Handle.CheckHandles,
      testRoundTrip @User.Identity.Email,
      testRoundTrip @User.Identity.Phone,
      testRoundTrip @User.Identity.UserSSOId,
      testRoundTrip @User.Identity.UserIdentity,
      testRoundTrip @User.Password.NewPasswordReset,
      testRoundTrip @User.Password.PasswordResetKey,
      -- FUTUREWORK: this should probably be tested individually,
      -- but PasswordResetIdentity currently doesn't have JSON instances itself.
      -- testRoundTrip @User.Password.PasswordResetIdentity,
      testRoundTrip @User.Password.PasswordResetCode,
      testRoundTrip @User.Password.CompletePasswordReset,
      testRoundTrip @User.Profile.Pict,
      testRoundTrip @User.Profile.Name,
      testRoundTrip @User.Profile.ColourId,
      testRoundTrip @User.Profile.AssetSize,
      testRoundTrip @User.Profile.Asset,
      testRoundTrip @User.Profile.Locale,
      testRoundTrip @User.Profile.ManagedBy,
      testRoundTrip @User.RichInfo.RichField,
      testRoundTrip @User.RichInfo.RichInfoAssocList,
      testRoundTrip @User.RichInfo.RichInfoMapAndList,
      testRoundTrip @User.RichInfo.RichInfo,
      testRoundTrip @(User.Search.SearchResult User.Search.Contact),
      testRoundTrip @User.Search.Contact
    ]
  where
    currentlyFailing = ignoreTest

testRoundTrip ::
  forall a.
  (Arbitrary a, Typeable a, ToJSON a, FromJSON a, Eq a, Show a) =>
  T.TestTree
testRoundTrip = testProperty msg trip
  where
    msg = show (typeRep @a)
    trip (v :: a) =
      counterexample (show $ toJSON v) $
        Right v === (parseEither parseJSON . toJSON) v
