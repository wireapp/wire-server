{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.ConversationSubsystem
  ( module Wire.ConversationSubsystem,
    Util.BotsAndMembers (..),
    Util.ConsentGiven (..),
    Util.canDeleteMember,
    Util.consentGiven,
    Util.isMember,
    Util.userLHEnabled,
    Features.toTeamStatus,
    MLSRemoval.RemoveUserIncludeMain (..),
    LegalholdConflicts.guardLegalholdPolicyConflicts,
  )
where

import Data.Code qualified as Code
import Data.CommaSeparatedList (CommaSeparatedList)
import Data.Domain
import Data.Id
import Data.LegalHold (UserLegalHoldStatus)
import Data.Misc (IpAddr, PlainTextPassword6)
import Data.Proxy (Proxy (..))
import Data.Qualified
import Data.Range
import Data.Singletons (Demote, Sing, SingKind)
import Galley.Types.Clients (Clients)
import Imports
import Polysemy
import Wire.API.Bot (AddBot, RemoveBot)
import Wire.API.Conversation hiding (Member)
import Wire.API.Conversation qualified as Public
import Wire.API.Conversation.Action
import Wire.API.Conversation.CellsState (CellsState)
import Wire.API.Conversation.Code (ConversationCodeInfo, CreateConversationCodeRequest, JoinConversationByCode)
import Wire.API.Conversation.Pagination (ConversationPage)
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role (ConversationRolesList)
import Wire.API.Conversation.Typing
import Wire.API.Event.Conversation
import Wire.API.Federation.API.Common
import Wire.API.Federation.API.Galley
import Wire.API.MLS.CommitBundle
import Wire.API.MLS.GroupInfo (GroupInfoData)
import Wire.API.MLS.Keys (MLSKeys, MLSKeysByPurpose, MLSPublicKey, MLSPublicKeyFormat, SomeKey)
import Wire.API.MLS.Message
import Wire.API.MLS.OutOfSync (EnableOutOfSyncCheck)
import Wire.API.MLS.Serialisation
import Wire.API.MLS.SubConversation (ConvOrSubConvId, PublicSubConversation, SubConvId)
import Wire.API.Message (ClientMismatch, IgnoreMissing, MessageSendingStatus, NewOtrMessage, QualifiedNewOtrMessage, ReportMissing)
import Wire.API.Pagination (PageSize, SortOrder)
import Wire.API.Provider.Bot qualified as Public (BotConvView)
import Wire.API.Routes.Internal.Galley.ConversationsIntra (UpsertOne2OneConversationRequest)
import Wire.API.Routes.Public (ZHostValue)
import Wire.API.Routes.Public.Galley.Conversation
import Wire.API.Routes.Public.Galley.MLS (MLSReset)
import Wire.API.Routes.Public.Galley.Messaging (MessageNotSent, PostOtrResponse)
import Wire.API.Routes.Public.Util (UpdateResult)
import Wire.API.Routes.Version
import Wire.API.ServantProto (RawProto (..))
import Wire.API.Team.Feature (AllTeamFeatures, GuestLinksConfig, LockableFeature)
import Wire.API.Team.LegalHold (UserLegalHoldStatusResponse)
import Wire.API.Team.Member (IsPerm, TeamMemberList)
import Wire.API.User (VerificationAction)
import Wire.ConversationStore.MLS.Types (ListGlobalSelfConvs)
import Wire.ConversationSubsystem.Features qualified as Features
import Wire.ConversationSubsystem.LegalholdConflicts qualified as LegalholdConflicts
import Wire.ConversationSubsystem.MLS.IncomingMessage (IncomingBundle, IncomingMessage)
import Wire.ConversationSubsystem.MLS.Removal qualified as MLSRemoval
import Wire.ConversationSubsystem.Util qualified as Util
import Wire.FeaturesConfigSubsystem.Types (GetFeatureConfig)
import Wire.NotificationSubsystem (LocalConversationUpdate)
import Wire.StoredConversation (BotMember, LocalMember, StoredConversation)

data PermissionCheckArgs teamAssociation where
  PermissionCheckArgs :: forall k (p :: k) teamAssociation. (SingKind k, IsPerm teamAssociation (Demote k)) => Sing p -> Maybe teamAssociation -> PermissionCheckArgs teamAssociation

data ConversationSubsystem m a where
  NotifyConversationAction ::
    Sing tag ->
    EventFrom ->
    Bool ->
    Maybe ConnId ->
    Local StoredConversation ->
    Set UserId ->
    Set (Remote UserId) ->
    Set BotMember ->
    ConversationAction (tag :: ConversationActionTag) ->
    ExtraConversationData ->
    ConversationSubsystem r LocalConversationUpdate
  InternalCreateGroupConversation ::
    Local UserId ->
    Maybe ConnId ->
    NewConv ->
    ConversationSubsystem m StoredConversation
  CreateGroupConversationUpToV3 ::
    Local UserId ->
    Maybe ConnId ->
    NewConv ->
    ConversationSubsystem m (ConversationResponse Public.OwnConversation)
  CreateGroupOwnConversation ::
    Local UserId ->
    Maybe ConnId ->
    NewConv ->
    ConversationSubsystem m CreateGroupConversationResponseV9
  CreateGroupConversation ::
    Local UserId ->
    Maybe ConnId ->
    NewConv ->
    ConversationSubsystem m CreateGroupConversation
  CreateProteusSelfConversation ::
    Local UserId ->
    ConversationSubsystem m (ConversationResponse Public.OwnConversation)
  CreateOne2OneConversation ::
    Local UserId ->
    ConnId ->
    NewOne2OneConv ->
    ConversationSubsystem m (ConversationResponse Public.OwnConversation)
  CreateConnectConversation ::
    Local UserId ->
    Maybe ConnId ->
    Connect ->
    ConversationSubsystem m (ConversationResponse Public.OwnConversation)
  GetConversations ::
    [ConvId] ->
    ConversationSubsystem m [StoredConversation]
  GetConversationIds ::
    Local UserId ->
    Range 1 1000 Int32 ->
    Maybe ConversationPagingState ->
    ConversationSubsystem r ConvIdsPage
  InternalGetClientIds :: [UserId] -> ConversationSubsystem m Clients
  InternalGetLocalMember ::
    ConvId ->
    UserId ->
    ConversationSubsystem m (Maybe LocalMember)
  InternalGetMember ::
    Qualified ConvId ->
    UserId ->
    ConversationSubsystem m (Maybe Public.Member)
  GetConversationMeta ::
    ConvId ->
    ConversationSubsystem m ConversationMetadata
  GetMLSOne2OneConversationInternal ::
    Local UserId ->
    Qualified UserId ->
    ConversationSubsystem m Public.OwnConversation
  IsMLSOne2OneEstablished ::
    Local UserId ->
    Qualified UserId ->
    ConversationSubsystem m Bool
  GetLocalConversationInternal ::
    ConvId ->
    ConversationSubsystem m Conversation
  RmClient ::
    UserId ->
    ClientId ->
    ConversationSubsystem m ()
  GetClients ::
    UserId ->
    ConversationSubsystem m [ClientId]
  AddBot ::
    Local UserId ->
    ConnId ->
    AddBot ->
    ConversationSubsystem m Event
  RmBot ::
    Local UserId ->
    Maybe ConnId ->
    RemoveBot ->
    ConversationSubsystem m (UpdateResult Event)
  GetFeatureInternal ::
    (GetFeatureConfig cfg) =>
    TeamId ->
    ConversationSubsystem m (LockableFeature cfg)
  UpdateCellsState ::
    ConvId ->
    CellsState ->
    ConversationSubsystem m ()
  RemoveUser ::
    Local StoredConversation ->
    MLSRemoval.RemoveUserIncludeMain ->
    Qualified UserId ->
    ConversationSubsystem m ()
  PostMLSCommitBundle ::
    Local x ->
    Qualified UserId ->
    ClientId ->
    ConvType ->
    Qualified ConvOrSubConvId ->
    Maybe ConnId ->
    EnableOutOfSyncCheck ->
    IncomingBundle ->
    ConversationSubsystem m [LocalConversationUpdate]
  PostMLSCommitBundleFromLocalUser ::
    Version ->
    Local UserId ->
    ClientId ->
    ConnId ->
    RawMLS CommitBundle ->
    ConversationSubsystem m MLSMessageSendingStatus
  PostMLSMessage ::
    Local x ->
    Qualified UserId ->
    ClientId ->
    ConvType ->
    Qualified ConvOrSubConvId ->
    Maybe ConnId ->
    EnableOutOfSyncCheck ->
    IncomingMessage ->
    ConversationSubsystem m [LocalConversationUpdate]
  PostMLSMessageFromLocalUser ::
    Version ->
    Local UserId ->
    ClientId ->
    ConnId ->
    RawMLS Message ->
    ConversationSubsystem m MLSMessageSendingStatus
  IsMLSEnabled :: ConversationSubsystem m Bool
  IterateConversations ::
    Local UserId ->
    Range 1 500 Int32 ->
    ([StoredConversation] -> m a) ->
    ConversationSubsystem m ()
  RemoveMemberFromLocalConv ::
    Local ConvId ->
    Local UserId ->
    Maybe ConnId ->
    Qualified UserId ->
    ConversationSubsystem m (Maybe Event)
  FederationOnConversationCreated ::
    Domain ->
    ConversationCreated ConvId ->
    ConversationSubsystem m EmptyResponse
  FederationGetConversationsV1 ::
    Domain ->
    GetConversationsRequest ->
    ConversationSubsystem m GetConversationsResponse
  FederationGetConversations ::
    Domain ->
    GetConversationsRequest ->
    ConversationSubsystem m GetConversationsResponseV2
  FederationLeaveConversation ::
    Domain ->
    LeaveConversationRequest ->
    ConversationSubsystem m LeaveConversationResponse
  FederationSendMessage ::
    Domain ->
    ProteusMessageSendRequest ->
    ConversationSubsystem m MessageSendResponse
  FederationUpdateConversation ::
    Domain ->
    ConversationUpdateRequest ->
    ConversationSubsystem m ConversationUpdateResponse
  FederationMlsSendWelcome ::
    Domain ->
    MLSWelcomeRequest ->
    ConversationSubsystem m MLSWelcomeResponse
  FederationSendMLSMessage ::
    Domain ->
    MLSMessageSendRequest ->
    ConversationSubsystem m MLSMessageResponse
  FederationSendMLSCommitBundle ::
    Domain ->
    MLSMessageSendRequest ->
    ConversationSubsystem m MLSMessageResponse
  FederationQueryGroupInfo ::
    Domain ->
    GetGroupInfoRequest ->
    ConversationSubsystem m GetGroupInfoResponse
  FederationUpdateTypingIndicator ::
    Domain ->
    TypingDataUpdateRequest ->
    ConversationSubsystem m TypingDataUpdateResponse
  FederationOnTypingIndicatorUpdated ::
    Domain ->
    TypingDataUpdated ->
    ConversationSubsystem m EmptyResponse
  FederationGetSubConversationForRemoteUser ::
    Domain ->
    GetSubConversationsRequest ->
    ConversationSubsystem m GetSubConversationsResponse
  FederationDeleteSubConversationForRemoteUser ::
    Domain ->
    DeleteSubConversationFedRequest ->
    ConversationSubsystem m DeleteSubConversationResponse
  FederationLeaveSubConversation ::
    Domain ->
    LeaveSubConversationRequest ->
    ConversationSubsystem m LeaveSubConversationResponse
  FederationGetOne2OneConversationV1 ::
    Domain ->
    GetOne2OneConversationRequest ->
    ConversationSubsystem m GetOne2OneConversationResponse
  FederationGetOne2OneConversation ::
    Domain ->
    GetOne2OneConversationRequest ->
    ConversationSubsystem m GetOne2OneConversationResponseV2
  FederationOnClientRemoved ::
    Domain ->
    ClientRemovedRequest ->
    ConversationSubsystem m EmptyResponse
  FederationOnMessageSent ::
    Domain ->
    RemoteMessage ConvId ->
    ConversationSubsystem m EmptyResponse
  FederationOnMLSMessageSent ::
    Domain ->
    RemoteMLSMessage ->
    ConversationSubsystem m EmptyResponse
  FederationOnConversationUpdatedV0 ::
    Domain ->
    ConversationUpdateV0 ->
    ConversationSubsystem m EmptyResponse
  FederationOnConversationUpdated ::
    Domain ->
    ConversationUpdate ->
    ConversationSubsystem m EmptyResponse
  FederationOnUserDeleted ::
    Domain ->
    UserDeletedConversationsNotification ->
    ConversationSubsystem m EmptyResponse
  PostOtrMessageUnqualified ::
    Local UserId ->
    ConnId ->
    ConvId ->
    Maybe IgnoreMissing ->
    Maybe ReportMissing ->
    NewOtrMessage ->
    ConversationSubsystem m (PostOtrResponse ClientMismatch)
  PostOtrBroadcastUnqualified ::
    Local UserId ->
    ConnId ->
    Maybe IgnoreMissing ->
    Maybe ReportMissing ->
    NewOtrMessage ->
    ConversationSubsystem m (PostOtrResponse ClientMismatch)
  PostProteusMessage ::
    Local UserId ->
    ConnId ->
    Qualified ConvId ->
    RawProto QualifiedNewOtrMessage ->
    ConversationSubsystem m (PostOtrResponse MessageSendingStatus)
  PostProteusBroadcast ::
    Local UserId ->
    ConnId ->
    QualifiedNewOtrMessage ->
    ConversationSubsystem m (PostOtrResponse MessageSendingStatus)
  DeleteLocalConversation ::
    Local UserId ->
    ConnId ->
    Local ConvId ->
    ConversationSubsystem m (UpdateResult Event)
  GetMLSPublicKeys ::
    Maybe MLSPublicKeyFormat ->
    ConversationSubsystem m (MLSKeysByPurpose (MLSKeys SomeKey))
  FeatureEnabledForTeam ::
    forall cfg m.
    (GetFeatureConfig cfg) =>
    Proxy cfg ->
    TeamId ->
    ConversationSubsystem m Bool
  GetAllTeamFeaturesForUser ::
    UserId ->
    ConversationSubsystem m AllTeamFeatures
  GetSingleFeatureForUser ::
    forall cfg m.
    (GetFeatureConfig cfg) =>
    UserId ->
    ConversationSubsystem m (LockableFeature cfg)
  ResetMLSConversation ::
    Local UserId ->
    MLSReset ->
    ConversationSubsystem m ()
  GetSubConversation ::
    Local UserId ->
    Qualified ConvId ->
    SubConvId ->
    ConversationSubsystem m PublicSubConversation
  GetUserStatus ::
    Local UserId ->
    TeamId ->
    UserId ->
    ConversationSubsystem m UserLegalHoldStatusResponse
  GuardSecondFactorDisabled ::
    UserId ->
    ConvId ->
    ConversationSubsystem m ()
  GetBotConversation ::
    BotId ->
    ConvId ->
    ConversationSubsystem m Public.BotConvView
  -- Query functions
  GetUnqualifiedOwnConversation ::
    Local UserId ->
    ConvId ->
    ConversationSubsystem m Public.OwnConversation
  GetOwnConversation ::
    Local UserId ->
    Qualified ConvId ->
    ConversationSubsystem m Public.OwnConversation
  GetPaginatedConversations ::
    Local UserId ->
    Maybe (Range 1 32 (CommaSeparatedList ConvId)) ->
    Maybe ConvId ->
    Maybe (Range 1 500 Int32) ->
    ConversationSubsystem m (Public.ConversationList Public.OwnConversation)
  GetConversation ::
    Local UserId ->
    Qualified ConvId ->
    ConversationSubsystem m Public.Conversation
  GetConversationRoles ::
    Local UserId ->
    ConvId ->
    ConversationSubsystem m ConversationRolesList
  SearchChannels ::
    Local UserId ->
    TeamId ->
    Maybe Text ->
    Maybe SortOrder ->
    Maybe PageSize ->
    Maybe Text ->
    Maybe ConvId ->
    Bool ->
    ConversationSubsystem m ConversationPage
  GetGroupInfo ::
    Local UserId ->
    Qualified ConvId ->
    ConversationSubsystem m GroupInfoData
  ConversationIdsPageFromUnqualified ::
    Local UserId ->
    Maybe ConvId ->
    Maybe (Range 1 1000 Int32) ->
    ConversationSubsystem m (ConversationList ConvId)
  ConversationIdsPageFromV2 ::
    ListGlobalSelfConvs ->
    Local UserId ->
    Public.GetPaginatedConversationIds ->
    ConversationSubsystem m Public.ConvIdsPage
  ConversationIdsPageFrom ::
    Local UserId ->
    Public.GetPaginatedConversationIds ->
    ConversationSubsystem m Public.ConvIdsPage
  ListConversations ::
    Local UserId ->
    Public.ListConversations ->
    ConversationSubsystem m ConversationsResponse
  GetConversationByReusableCode ::
    Local UserId ->
    Code.Key ->
    Code.Value ->
    ConversationSubsystem m ConversationCoverView
  GetMLSSelfConversationWithError ::
    Local UserId ->
    ConversationSubsystem m Public.OwnConversation
  GetMLSOne2OneConversationV5 ::
    Local UserId ->
    Qualified UserId ->
    ConversationSubsystem m Public.OwnConversation
  GetMLSOne2OneConversationV6 ::
    Local UserId ->
    Qualified UserId ->
    ConversationSubsystem m (MLSOne2OneConversation MLSPublicKey)
  GetMLSOne2OneConversation ::
    Local UserId ->
    Qualified UserId ->
    Maybe MLSPublicKeyFormat ->
    ConversationSubsystem m (MLSOne2OneConversation SomeKey)
  GetLocalSelf ::
    Local UserId ->
    ConvId ->
    ConversationSubsystem m (Maybe Public.Member)
  GetSelfMember ::
    Local UserId ->
    Qualified ConvId ->
    ConversationSubsystem m (Maybe Public.Member)
  GetConversationGuestLinksStatus ::
    UserId ->
    ConvId ->
    ConversationSubsystem m (LockableFeature GuestLinksConfig)
  GetCode ::
    Maybe Text ->
    Local UserId ->
    ConvId ->
    ConversationSubsystem m ConversationCodeInfo
  -- Update functions
  AddMembersUnqualified ::
    Local UserId ->
    ConnId ->
    ConvId ->
    Invite ->
    ConversationSubsystem m (UpdateResult Event)
  AddMembersUnqualifiedV2 ::
    Local UserId ->
    ConnId ->
    ConvId ->
    InviteQualified ->
    ConversationSubsystem m (UpdateResult Event)
  AddMembers ::
    Local UserId ->
    ConnId ->
    Qualified ConvId ->
    InviteQualified ->
    ConversationSubsystem m (UpdateResult Event)
  ReplaceMembers ::
    Local UserId ->
    ConnId ->
    Qualified ConvId ->
    InviteQualified ->
    ConversationSubsystem m ()
  JoinConversationById ::
    Local UserId ->
    ConnId ->
    ConvId ->
    ConversationSubsystem m (UpdateResult Event)
  JoinConversationByReusableCode ::
    Local UserId ->
    ConnId ->
    JoinConversationByCode ->
    ConversationSubsystem m (UpdateResult Event)
  CheckReusableCode ::
    IpAddr ->
    ConversationCode ->
    ConversationSubsystem m ()
  AddCodeUnqualified ::
    Maybe CreateConversationCodeRequest ->
    UserId ->
    Maybe ZHostValue ->
    Maybe ConnId ->
    ConvId ->
    ConversationSubsystem m AddCodeResult
  AddCodeUnqualifiedWithReqBody ::
    UserId ->
    Maybe Text ->
    Maybe ConnId ->
    ConvId ->
    CreateConversationCodeRequest ->
    ConversationSubsystem m AddCodeResult
  RmCodeUnqualified ::
    Local UserId ->
    ConnId ->
    ConvId ->
    ConversationSubsystem m Event
  MemberTypingUnqualified ::
    Local UserId ->
    ConnId ->
    ConvId ->
    TypingStatus ->
    ConversationSubsystem m ()
  MemberTyping ::
    Local UserId ->
    ConnId ->
    Qualified ConvId ->
    TypingStatus ->
    ConversationSubsystem m ()
  RemoveMemberUnqualified ::
    Local UserId ->
    ConnId ->
    ConvId ->
    UserId ->
    ConversationSubsystem m (Maybe Event)
  RemoveMemberQualified ::
    Local UserId ->
    ConnId ->
    Qualified ConvId ->
    Qualified UserId ->
    ConversationSubsystem m (Maybe Event)
  UpdateOtherMemberUnqualified ::
    Local UserId ->
    ConnId ->
    ConvId ->
    UserId ->
    OtherMemberUpdate ->
    ConversationSubsystem m ()
  UpdateOtherMember ::
    Local UserId ->
    ConnId ->
    Qualified ConvId ->
    Qualified UserId ->
    OtherMemberUpdate ->
    ConversationSubsystem m ()
  UpdateUnqualifiedConversationName ::
    Local UserId ->
    ConnId ->
    ConvId ->
    ConversationRename ->
    ConversationSubsystem m (UpdateResult Event)
  UpdateConversationName ::
    Local UserId ->
    ConnId ->
    Qualified ConvId ->
    ConversationRename ->
    ConversationSubsystem m (UpdateResult Event)
  UpdateConversationMessageTimerUnqualified ::
    Local UserId ->
    ConnId ->
    ConvId ->
    ConversationMessageTimerUpdate ->
    ConversationSubsystem m (UpdateResult Event)
  UpdateConversationMessageTimer ::
    Local UserId ->
    ConnId ->
    Qualified ConvId ->
    ConversationMessageTimerUpdate ->
    ConversationSubsystem m (UpdateResult Event)
  UpdateConversationReceiptModeUnqualified ::
    Local UserId ->
    ConnId ->
    ConvId ->
    ConversationReceiptModeUpdate ->
    ConversationSubsystem m (UpdateResult Event)
  UpdateConversationReceiptMode ::
    Local UserId ->
    ConnId ->
    Qualified ConvId ->
    ConversationReceiptModeUpdate ->
    ConversationSubsystem m (UpdateResult Event)
  UpdateConversationAccessUnqualified ::
    Local UserId ->
    ConnId ->
    ConvId ->
    ConversationAccessData ->
    ConversationSubsystem m (UpdateResult Event)
  UpdateConversationAccess ::
    Local UserId ->
    ConnId ->
    Qualified ConvId ->
    ConversationAccessData ->
    ConversationSubsystem m (UpdateResult Event)
  UpdateConversationHistory ::
    Local UserId ->
    ConnId ->
    Qualified ConvId ->
    ConversationHistoryUpdate ->
    ConversationSubsystem m (UpdateResult Event)
  UpdateUnqualifiedSelfMember ::
    Local UserId ->
    ConnId ->
    ConvId ->
    MemberUpdate ->
    ConversationSubsystem m ()
  UpdateSelfMember ::
    Local UserId ->
    ConnId ->
    Qualified ConvId ->
    MemberUpdate ->
    ConversationSubsystem m ()
  UpdateConversationProtocolWithLocalUser ::
    Local UserId ->
    ConnId ->
    Qualified ConvId ->
    ProtocolUpdate ->
    ConversationSubsystem m (UpdateResult Event)
  UpdateChannelAddPermission ::
    Local UserId ->
    ConnId ->
    Qualified ConvId ->
    AddPermissionUpdate ->
    ConversationSubsystem m (UpdateResult Event)
  PostBotMessageUnqualified ::
    BotId ->
    ConvId ->
    Maybe IgnoreMissing ->
    Maybe ReportMissing ->
    NewOtrMessage ->
    ConversationSubsystem m (Either (MessageNotSent ClientMismatch) ClientMismatch)
  -- Sub-conversation functions
  DeleteSubConversation ::
    Local UserId ->
    Qualified ConvId ->
    SubConvId ->
    MLSReset ->
    ConversationSubsystem m ()
  GetSubConversationGroupInfo ::
    Local UserId ->
    Qualified ConvId ->
    SubConvId ->
    ConversationSubsystem m GroupInfoData
  LeaveSubConversation ::
    Local UserId ->
    ClientId ->
    Qualified ConvId ->
    SubConvId ->
    ConversationSubsystem m ()
  SendConversationActionNotifications ::
    forall tag m.
    Sing tag ->
    Qualified UserId ->
    Bool ->
    Maybe ConnId ->
    Local StoredConversation ->
    Util.BotsAndMembers ->
    ConversationAction (tag :: ConversationActionTag) ->
    ExtraConversationData ->
    ConversationSubsystem m LocalConversationUpdate
  PermissionCheck ::
    (IsPerm teamAssociation perm) =>
    perm -> Maybe teamAssociation -> ConversationSubsystem m teamAssociation
  PermissionCheckSAbs ::
    PermissionCheckArgs teamAssociation ->
    ConversationSubsystem m teamAssociation
  EnsureReAuthorised ::
    UserId ->
    Maybe PlainTextPassword6 ->
    Maybe Code.Value ->
    Maybe VerificationAction ->
    ConversationSubsystem m ()
  QualifyLocal ::
    a ->
    ConversationSubsystem m (Local a)
  AssertOnTeam ::
    UserId ->
    TeamId ->
    ConversationSubsystem m ()
  CheckConsent ::
    Map UserId TeamId ->
    UserId ->
    ConversationSubsystem m Util.ConsentGiven
  GetLHStatusForUsers ::
    [UserId] ->
    ConversationSubsystem m [(UserId, UserLegalHoldStatus)]
  EnsureConnectedToLocals ::
    UserId ->
    [UserId] ->
    ConversationSubsystem m ()
  GetTeamMembersForFanout ::
    TeamId ->
    ConversationSubsystem m TeamMemberList
  AssertTeamExists ::
    TeamId ->
    ConversationSubsystem m ()
  InternalUpsertOne2OneConversation ::
    UpsertOne2OneConversationRequest ->
    ConversationSubsystem m ()
  AcceptConv ::
    QualifiedWithTag QLocal UserId ->
    Maybe ConnId ->
    ConvId ->
    ConversationSubsystem m OwnConversation
  BlockConv ::
    QualifiedWithTag QLocal UserId ->
    Qualified ConvId ->
    ConversationSubsystem m ()
  UnblockConv ::
    QualifiedWithTag QLocal UserId ->
    Maybe ConnId ->
    Qualified ConvId ->
    ConversationSubsystem m ()

makeSem ''ConversationSubsystem

permissionCheckS :: forall k (p :: k) teamAssociation r. (Member ConversationSubsystem r, SingKind k, IsPerm teamAssociation (Demote k)) => Sing p -> Maybe teamAssociation -> Sem r teamAssociation
permissionCheckS p mTeam = send (PermissionCheckSAbs (PermissionCheckArgs p mTeam))
