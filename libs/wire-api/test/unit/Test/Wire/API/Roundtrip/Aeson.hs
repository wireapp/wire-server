-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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
import Data.OpenApi (ToSchema, validatePrettyToJSON)
import Imports
import Test.Tasty qualified as T
import Test.Tasty.QuickCheck (Arbitrary, counterexample, testProperty, (.&&.), (===))
import Type.Reflection (typeRep)
import Wire.API.Asset qualified as Asset
import Wire.API.Call.Config qualified as Call.Config
import Wire.API.Connection qualified as Connection
import Wire.API.Conversation qualified as Conversation
import Wire.API.Conversation.Action qualified as Conversation.Action
import Wire.API.Conversation.Bot qualified as Conversation.Bot
import Wire.API.Conversation.Code qualified as Conversation.Code
import Wire.API.Conversation.Member qualified as Conversation.Member
import Wire.API.Conversation.Role qualified as Conversation.Role
import Wire.API.Conversation.Typing qualified as Conversation.Typing
import Wire.API.CustomBackend qualified as CustomBackend
import Wire.API.Event.Conversation qualified as Event.Conversation
import Wire.API.Event.Team qualified as Event.Team
import Wire.API.FederationStatus qualified as FederationStatus
import Wire.API.Locale qualified as Locale
import Wire.API.Message qualified as Message
import Wire.API.OAuth qualified as OAuth
import Wire.API.Properties qualified as Properties
import Wire.API.Provider qualified as Provider
import Wire.API.Provider.Bot qualified as Provider.Bot
import Wire.API.Provider.External qualified as Provider.External
import Wire.API.Provider.Service qualified as Provider.Service
import Wire.API.Provider.Service.Tag qualified as Provider.Service.Tag
import Wire.API.Push.Token qualified as Push.Token
import Wire.API.Routes.FederationDomainConfig qualified as FederationDomainConfig
import Wire.API.Routes.Internal.Brig.EJPD qualified as EJPD
import Wire.API.Routes.Internal.Galley.TeamsIntra qualified as TeamsIntra
import Wire.API.Routes.Version qualified as Routes.Version
import Wire.API.SystemSettings qualified as SystemSettings
import Wire.API.Team qualified as Team
import Wire.API.Team.Conversation qualified as Team.Conversation
import Wire.API.Team.Feature qualified as Team.Feature
import Wire.API.Team.Invitation qualified as Team.Invitation
import Wire.API.Team.LegalHold qualified as Team.LegalHold
import Wire.API.Team.LegalHold.External qualified as Team.LegalHold.External
import Wire.API.Team.Member qualified as Team.Member
import Wire.API.Team.Permission qualified as Team.Permission
import Wire.API.Team.Role qualified as Team.Role
import Wire.API.Team.SearchVisibility qualified as Team.SearchVisibility
import Wire.API.User qualified as User
import Wire.API.User.Activation qualified as User.Activation
import Wire.API.User.Auth qualified as User.Auth
import Wire.API.User.Client qualified as User.Client
import Wire.API.User.Client.Prekey qualified as User.Client.Prekey
import Wire.API.User.Handle qualified as User.Handle
import Wire.API.User.Identity qualified as User.Identity
import Wire.API.User.Password qualified as User.Password
import Wire.API.User.Profile qualified as User.Profile
import Wire.API.User.RichInfo qualified as User.RichInfo
import Wire.API.User.Scim qualified as Scim
import Wire.API.User.Search qualified as User.Search
import Wire.API.Wrapped qualified as Wrapped

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
      testRoundTrip @Asset.Asset,
      testRoundTrip @Call.Config.TurnHost,
      testRoundTrip @Call.Config.Scheme,
      testRoundTrip @Call.Config.Transport,
      testRoundTrip @Call.Config.TurnURI,
      testRoundTrip @Call.Config.TurnUsername,
      testRoundTrip @Call.Config.RTCIceServer,
      testRoundTrip @Call.Config.RTCConfiguration,
      testRoundTrip @Call.Config.SFTServer,
      testRoundTrip @Connection.ConnectionRequest,
      testRoundTrip @Connection.Relation,
      testRoundTrip @Connection.UserConnection,
      testRoundTrip @Connection.UserConnectionList,
      testRoundTrip @Connection.ConnectionUpdate,
      testRoundTrip @Conversation.Conversation,
      testRoundTrip @(Conversation.ConversationList ConvId),
      testRoundTrip @(Conversation.ConversationList Conversation.Conversation),
      testRoundTrip @Conversation.Access,
      testRoundTrip @Conversation.AccessRoleLegacy,
      testRoundTrip @Conversation.AccessRole,
      testRoundTrip @Conversation.ConvType,
      testRoundTrip @Conversation.ReceiptMode,
      testRoundTrip @Conversation.ConvTeamInfo,
      testRoundTrip @Conversation.ConversationCoverView,
      testRoundTrip @Conversation.Invite,
      testRoundTrip @Conversation.ConversationRename,
      testRoundTrip @Conversation.ConversationAccessData,
      testRoundTrip @Conversation.ConversationReceiptModeUpdate,
      testRoundTrip @Conversation.ConversationMessageTimerUpdate,
      testRoundTrip @Conversation.ConversationMetadata,
      testRoundTrip @Conversation.Bot.AddBot,
      testRoundTrip @Conversation.Bot.AddBotResponse,
      testRoundTrip @Conversation.Bot.RemoveBotResponse,
      testRoundTrip @Conversation.Bot.UpdateBotPrekeys,
      testRoundTrip @Conversation.Code.ConversationCode,
      testRoundTrip @Conversation.Code.ConversationCodeInfo,
      testRoundTrip @Conversation.Code.JoinConversationByCode,
      testRoundTrip @Conversation.Code.CreateConversationCodeRequest,
      testRoundTrip @Conversation.Member.MemberUpdate,
      testRoundTrip @Conversation.Member.MutedStatus,
      testRoundTrip @Conversation.Member.Member,
      testRoundTrip @Conversation.Member.OtherMember,
      testRoundTrip @Conversation.Member.ConvMembers,
      testRoundTrip @Conversation.Member.OtherMemberUpdate,
      testRoundTrip @Conversation.Role.RoleName,
      testRoundTrip @Conversation.Role.Action,
      testRoundTrip @Conversation.Role.ConversationRole,
      testRoundTrip @Conversation.Role.ConversationRolesList,
      testRoundTrip @Conversation.Typing.TypingStatus,
      testRoundTrip @CustomBackend.CustomBackend,
      testRoundTrip @EJPD.EJPDContact,
      testRoundTrip @Event.Conversation.Event,
      testRoundTrip @Event.Conversation.EventType,
      testRoundTrip @Event.Conversation.SimpleMember,
      testRoundTrip @Event.Conversation.SimpleMembers,
      testRoundTrip @Event.Conversation.Connect,
      testRoundTrip @Event.Conversation.MemberUpdateData,
      testRoundTrip @Event.Conversation.OtrMessage,
      testRoundTrip @Event.Team.Event,
      testRoundTrip @Event.Team.EventType,
      testRoundTrip @FederationDomainConfig.FederationDomainConfigs,
      testRoundTrip @FederationDomainConfig.FederationStrategy,
      testRoundTrip @FederationStatus.FederationStatus,
      testRoundTrip @FederationStatus.RemoteDomains,
      testRoundTrip @Locale.Locale,
      testRoundTrip @Message.Priority,
      testRoundTrip @Message.OtrRecipients,
      testRoundTrip @Message.NewOtrMessage,
      testRoundTrip @Message.ClientMismatch,
      testRoundTrip @OAuth.RedirectUrl,
      testRoundTrip @OAuth.OAuthApplicationName,
      testRoundTrip @OAuth.OAuthClientConfig,
      testRoundTrip @OAuth.OAuthClient,
      testRoundTrip @OAuth.CreateOAuthAuthorizationCodeRequest,
      testRoundTrip @OAuth.OAuthAccessTokenRequest,
      testRoundTrip @OAuth.OAuthApplication,
      testRoundTrip @Properties.PropertyKey,
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
      testRoundTrip @Scim.CreateScimToken,
      testRoundTrip @SystemSettings.SystemSettings,
      testRoundTrip @SystemSettings.SystemSettingsPublic,
      testRoundTrip @SystemSettings.SystemSettingsInternal,
      testRoundTrip @Team.NewTeam,
      testRoundTrip @Team.TeamBinding,
      testRoundTrip @Team.Team,
      testRoundTrip @Team.TeamList,
      testRoundTrip @Team.TeamUpdateData,
      testRoundTrip @Team.TeamDeleteData,
      testRoundTrip @Team.Conversation.TeamConversation,
      testRoundTrip @Team.Conversation.TeamConversationList,
      testRoundTrip @(Team.Feature.LockableFeature Team.Feature.LegalholdConfig),
      testRoundTrip @(Team.Feature.LockableFeaturePatch Team.Feature.LegalholdConfig),
      testRoundTrip @(Team.Feature.LockableFeaturePatch Team.Feature.SelfDeletingMessagesConfig),
      testRoundTrip @(Team.Feature.Feature Team.Feature.LegalholdConfig),
      testRoundTrip @Team.Feature.AllTeamFeatures,
      testRoundTrip @Team.Feature.FeatureStatus,
      testRoundTrip @Team.Feature.LockStatus,
      testRoundTrip @Team.Invitation.InvitationRequest,
      testRoundTrip @Team.Invitation.Invitation,
      testRoundTrip @Team.Invitation.InvitationList,
      testRoundTrip @Team.LegalHold.ViewLegalHoldServiceInfo,
      testRoundTrip @Team.LegalHold.NewLegalHoldService,
      testRoundTrip @Team.LegalHold.ViewLegalHoldService,
      testRoundTrip @Team.LegalHold.UserLegalHoldStatusResponse,
      testRoundTrip @Team.LegalHold.RemoveLegalHoldSettingsRequest,
      testRoundTrip @Team.LegalHold.DisableLegalHoldForUserRequest,
      testRoundTrip @Team.LegalHold.ApproveLegalHoldForUserRequest,
      testRoundTrip @Team.LegalHold.External.RequestNewLegalHoldClientV0,
      testRoundTrip @Team.LegalHold.External.RequestNewLegalHoldClient,
      testRoundTrip @Team.LegalHold.External.NewLegalHoldClient,
      testRoundTrip @Team.LegalHold.External.LegalHoldServiceConfirm,
      testRoundTrip @Team.LegalHold.External.LegalHoldServiceRemove,
      testRoundTrip @Team.LegalHold.LegalholdProtectee,
      testRoundTrip @Team.Member.TeamMember,
      testRoundTrip @Team.Member.ListType,
      testRoundTrip @Team.Member.NewListType,
      testRoundTrip @Team.Member.TeamMemberList,
      testRoundTrip @Team.Member.NewTeamMember,
      testRoundTrip @Team.Member.TeamMemberDeleteData,
      testRoundTrip @Team.Permission.Permissions,
      testRoundTrip @Team.Role.Role,
      testRoundTrip @Team.SearchVisibility.TeamSearchVisibility,
      testRoundTrip @Team.SearchVisibility.TeamSearchVisibilityView,
      testRoundTrip @User.NameUpdate,
      testRoundTrip @User.NewUser,
      testRoundTrip @User.NewUserPublic,
      testRoundTrip @User.UserIdList,
      testRoundTrip @(User.LimitedQualifiedUserIdList 20),
      testRoundTrip @User.UserProfile,
      testRoundTrip @User.User,
      testRoundTrip @User.UserSet,
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
      testRoundTrip @User.VerificationAction,
      testRoundTrip @User.SendVerificationCode,
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
      testRoundTrip @User.Client.UserClientsFull,
      testRoundTrip @User.Client.ClientType,
      testRoundTrip @User.Client.ClientClass,
      testRoundTrip @User.Client.PubClient,
      testRoundTrip @User.Client.Client,
      testRoundTrip @User.Client.NewClient,
      testRoundTrip @User.Client.UpdateClient,
      testRoundTripWithSwagger @User.Client.ClientCapability,
      testRoundTripWithSwagger @User.Client.ClientCapabilityList,
      testRoundTrip @User.Client.RmClient,
      testRoundTrip @User.Client.Prekey.LastPrekey,
      testRoundTrip @User.Client.Prekey.PrekeyId,
      testRoundTrip @User.Client.Prekey.Prekey,
      testRoundTrip @User.Client.Prekey.ClientPrekey,
      testRoundTrip @User.Client.Prekey.PrekeyBundle,
      testRoundTrip @User.Handle.UserHandleInfo,
      testRoundTrip @User.Handle.CheckHandles,
      testRoundTrip @User.Identity.EmailAddress,
      testRoundTrip @User.Identity.Phone,
      testRoundTrip @User.Identity.UserSSOId,
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
      testRoundTrip @User.Profile.ManagedBy,
      testRoundTrip @User.RichInfo.RichField,
      testRoundTrip @User.RichInfo.RichInfoAssocList,
      testRoundTrip @User.RichInfo.RichInfo,
      testRoundTrip @(User.Search.SearchResult User.Search.TeamContact),
      testRoundTrip @User.Search.PagingState,
      testRoundTrip @User.Search.TeamContact,
      testRoundTrip @(Wrapped.Wrapped "some_int" Int),
      testRoundTrip @Conversation.Action.SomeConversationAction,
      testRoundTrip @Routes.Version.Version,
      testRoundTrip @Routes.Version.VersionNumber,
      testRoundTrip @TeamsIntra.GuardLegalholdPolicyConflicts,
      testRoundTrip @TeamsIntra.TeamStatus,
      testRoundTrip @TeamsIntra.TeamStatusUpdate,
      testRoundTrip @TeamsIntra.TeamData,
      testRoundTrip @TeamsIntra.TeamName
    ]

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

testRoundTripWithSwagger ::
  forall a.
  (Arbitrary a, ToJSON a, FromJSON a, ToSchema a, Eq a, Show a) =>
  T.TestTree
testRoundTripWithSwagger = testProperty msg (trip .&&. scm)
  where
    msg = show (typeRep @a)

    trip (v :: a) =
      counterexample (show $ toJSON v) $
        Right v === (parseEither parseJSON . toJSON) v

    scm (v :: a) =
      counterexample
        ( fromMaybe "Schema validation failed, but there were no errors. This looks like a bug in swagger2!" $
            validatePrettyToJSON v
        )
        $ isNothing (validatePrettyToJSON v)
