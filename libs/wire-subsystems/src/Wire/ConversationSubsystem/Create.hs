{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}

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

module Wire.ConversationSubsystem.Create where

import Data.Id
import Data.Qualified
import Data.Set qualified as Set
import Galley.Types.Error
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog qualified as P
import Wire.API.Conversation hiding (Member)
import Wire.API.Conversation qualified as Public
import Wire.API.Conversation.Config
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Event.Conversation
import Wire.API.Federation.Client (FederatorClient)
import Wire.API.Federation.Error
import Wire.API.FederationStatus (RemoteDomains (..))
import Wire.API.Routes.Public.Galley.Conversation
import Wire.API.Routes.Public.Util (ResponseForExistedCreated (..))
import Wire.API.User
import Wire.BackendNotificationQueueAccess (BackendNotificationQueueAccess)
import Wire.BrigAPIAccess
import Wire.ConversationStore (ConversationStore)
import Wire.ConversationSubsystem.CreateInternal
import Wire.ConversationSubsystem.Mapping
import Wire.FeaturesConfigSubsystem
import Wire.FederationAPIAccess (FederationAPIAccess)
import Wire.FederationSubsystem (FederationSubsystem, checkFederationStatus, enforceFederationProtocol)
import Wire.LegalHoldStore (LegalHoldStore)
import Wire.NotificationSubsystem as NS
import Wire.Sem.Now (Now)
import Wire.Sem.Random (Random)
import Wire.TeamCollaboratorsSubsystem
import Wire.TeamStore (TeamStore)
import Wire.TeamSubsystem (TeamSubsystem)

----------------------------------------------------------------------------
-- API Handlers

createGroupConversationUpToV3 ::
  ( Member BrigAPIAccess r,
    Member ConversationStore r,
    Member (ErrorS 'ConvAccessDenied) r,
    Member (Error InvalidInput) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS OperationDenied) r,
    Member (ErrorS 'NotConnected) r,
    Member (ErrorS 'MLSNotEnabled) r,
    Member (ErrorS 'MLSNonEmptyMemberList) r,
    Member (ErrorS 'MissingLegalholdConsent) r,
    Member (ErrorS 'ChannelsNotEnabled) r,
    Member (ErrorS 'NotAnMlsConversation) r,
    Member (ErrorS HistoryNotSupported) r,
    Member (Input ConversationSubsystemConfig) r,
    Member LegalHoldStore r,
    Member TeamStore r,
    Member FeaturesConfigSubsystem r,
    Member TeamCollaboratorsSubsystem r,
    Member Random r,
    Member TeamSubsystem r,
    Member Now r,
    Member NotificationSubsystem r,
    Member (Error FederationError) r,
    Member BackendNotificationQueueAccess r,
    Member (FederationAPIAccess FederatorClient) r,
    Member (Error UnreachableBackendsLegacy) r,
    Member (Error InternalError) r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  Maybe ConnId ->
  NewConv ->
  Sem r (ConversationResponse Public.OwnConversation)
createGroupConversationUpToV3 lusr conn newConv = mapError UnreachableBackendsLegacy $ do
  dbConv <- createGroupConversationGeneric lusr conn newConv
  Created <$> conversationViewV9 lusr dbConv

createGroupOwnConversation ::
  ( Member BrigAPIAccess r,
    Member ConversationStore r,
    Member (ErrorS 'ConvAccessDenied) r,
    Member (Error InvalidInput) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS OperationDenied) r,
    Member (ErrorS 'NotConnected) r,
    Member (ErrorS 'MLSNotEnabled) r,
    Member (ErrorS 'MLSNonEmptyMemberList) r,
    Member (ErrorS 'MissingLegalholdConsent) r,
    Member (ErrorS 'ChannelsNotEnabled) r,
    Member (ErrorS 'NotAnMlsConversation) r,
    Member (ErrorS HistoryNotSupported) r,
    Member (Input ConversationSubsystemConfig) r,
    Member LegalHoldStore r,
    Member TeamStore r,
    Member FeaturesConfigSubsystem r,
    Member TeamCollaboratorsSubsystem r,
    Member Random r,
    Member TeamSubsystem r,
    Member Now r,
    Member NotificationSubsystem r,
    Member (Error FederationError) r,
    Member (Error UnreachableBackends) r,
    Member BackendNotificationQueueAccess r,
    Member (FederationAPIAccess FederatorClient) r,
    Member (Error InternalError) r,
    Member FederationSubsystem r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  Maybe ConnId ->
  NewConv ->
  Sem r CreateGroupConversationResponseV9
createGroupOwnConversation lusr conn newConv = do
  let remoteDomains = void <$> snd (partitionQualified lusr $ newConv.newConvQualifiedUsers)
  enforceFederationProtocol (baseProtocolToProtocol newConv.newConvProtocol) remoteDomains
  checkFederationStatus (RemoteDomains $ Set.fromList remoteDomains)
  dbConv <- createGroupConversationGeneric lusr conn newConv
  GroupConversationCreatedV9 <$> (CreateGroupOwnConversation <$> conversationViewV9 lusr dbConv <*> pure mempty)

createGroupConversation ::
  ( Member BrigAPIAccess r,
    Member ConversationStore r,
    Member (ErrorS 'ConvAccessDenied) r,
    Member (Error InvalidInput) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS OperationDenied) r,
    Member (ErrorS 'NotConnected) r,
    Member (ErrorS 'MLSNotEnabled) r,
    Member (ErrorS 'MLSNonEmptyMemberList) r,
    Member (ErrorS 'MissingLegalholdConsent) r,
    Member (ErrorS 'ChannelsNotEnabled) r,
    Member (ErrorS 'NotAnMlsConversation) r,
    Member (ErrorS HistoryNotSupported) r,
    Member (Input ConversationSubsystemConfig) r,
    Member LegalHoldStore r,
    Member TeamStore r,
    Member FeaturesConfigSubsystem r,
    Member TeamCollaboratorsSubsystem r,
    Member Random r,
    Member TeamSubsystem r,
    Member Now r,
    Member NotificationSubsystem r,
    Member (Error FederationError) r,
    Member (Error UnreachableBackends) r,
    Member BackendNotificationQueueAccess r,
    Member (FederationAPIAccess FederatorClient) r,
    Member FederationSubsystem r
  ) =>
  Local UserId ->
  Maybe ConnId ->
  NewConv ->
  Sem r CreateGroupConversation
createGroupConversation lusr conn newConv = do
  let remoteDomains = void <$> snd (partitionQualified lusr $ newConv.newConvQualifiedUsers)
  enforceFederationProtocol (baseProtocolToProtocol newConv.newConvProtocol) remoteDomains
  checkFederationStatus (RemoteDomains $ Set.fromList remoteDomains)
  dbConv <- createGroupConversationGeneric lusr conn newConv
  pure $
    CreateGroupConversation
      { conversation = conversationView (qualifyAs lusr ()) (Just lusr) dbConv,
        failedToAdd = mempty
      }

createProteusSelfConversation ::
  ( Member ConversationStore r,
    Member (Error InternalError) r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  Sem r (ConversationResponse Public.OwnConversation)
createProteusSelfConversation lusr = do
  (c, created) <- createProteusSelfConversationLogic lusr
  if created
    then Created <$> conversationViewV9 lusr c
    else Existed <$> conversationViewV9 lusr c

createOne2OneConversation ::
  ( Member BrigAPIAccess r,
    Member ConversationStore r,
    Member (Error FederationError) r,
    Member (Error UnreachableBackends) r,
    Member (Error InvalidInput) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS OperationDenied) r,
    Member (ErrorS 'NonBindingTeam) r,
    Member (ErrorS 'NoBindingTeamMembers) r,
    Member (ErrorS 'TeamNotFound) r,
    Member (ErrorS 'InvalidOperation) r,
    Member (ErrorS 'NotConnected) r,
    Member TeamStore r,
    Member TeamCollaboratorsSubsystem r,
    Member TeamSubsystem r,
    Member Now r,
    Member NotificationSubsystem r,
    Member BackendNotificationQueueAccess r,
    Member (FederationAPIAccess FederatorClient) r,
    Member (Error InternalError) r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  ConnId ->
  NewOne2OneConv ->
  Sem r (ConversationResponse Public.OwnConversation)
createOne2OneConversation lusr zcon j = do
  (c, created) <- createOne2OneConversationLogic lusr zcon j
  if created
    then Created <$> conversationViewV9 lusr c
    else Existed <$> conversationViewV9 lusr c

----------------------------------------------------------------------------
-- Helpers

createConnectConversation ::
  ( Member ConversationStore r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member (Error InvalidInput) r,
    Member (Error UnreachableBackends) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'InvalidOperation) r,
    Member NotificationSubsystem r,
    Member BackendNotificationQueueAccess r,
    Member Now r,
    Member (FederationAPIAccess FederatorClient) r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  Maybe ConnId ->
  Connect ->
  Sem r (ConversationResponse Public.OwnConversation)
createConnectConversation lusr conn j = do
  (c, created) <- createConnectConversationLogic lusr conn j
  if created
    then Created <$> conversationViewV9 lusr c
    else Existed <$> conversationViewV9 lusr c
