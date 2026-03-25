{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.ConversationSubsystem.Interpreter
  ( interpretConversationSubsystem,
  )
where

import Galley.Types.Error (InternalError, InvalidInput (..))
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Wire.API.Conversation.Config
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Federation.Client (FederatorClient)
import Wire.API.Federation.Error
import Wire.BackendNotificationQueueAccess (BackendNotificationQueueAccess)
import Wire.BrigAPIAccess
import Wire.ConversationStore (ConversationStore)
import Wire.ConversationStore qualified as ConvStore
import Wire.ConversationSubsystem
import Wire.ConversationSubsystem.CreateInternal qualified as CreateInternal
import Wire.ConversationSubsystem.Fetch qualified as Fetch
import Wire.ConversationSubsystem.Internal qualified as Internal
import Wire.ConversationSubsystem.Notify qualified as Notify
import Wire.ExternalAccess (ExternalAccess)
import Wire.FeaturesConfigSubsystem
import Wire.FederationAPIAccess (FederationAPIAccess)
import Wire.LegalHoldStore (LegalHoldStore)
import Wire.NotificationSubsystem as NS
import Wire.Sem.Now (Now)
import Wire.Sem.Random (Random)
import Wire.TeamCollaboratorsSubsystem
import Wire.TeamStore (TeamStore)
import Wire.TeamSubsystem (TeamSubsystem)
import Wire.UserClientIndexStore (UserClientIndexStore)

interpretConversationSubsystem ::
  ( Member (Error FederationError) r,
    Member (Error UnreachableBackends) r,
    Member (Error InternalError) r,
    Member (Error InvalidInput) r,
    Member (ErrorS 'ConvAccessDenied) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS OperationDenied) r,
    Member (ErrorS 'NotConnected) r,
    Member (ErrorS 'MLSNotEnabled) r,
    Member (ErrorS 'MLSNonEmptyMemberList) r,
    Member (ErrorS 'MissingLegalholdConsent) r,
    Member (ErrorS 'NonBindingTeam) r,
    Member (ErrorS 'NoBindingTeamMembers) r,
    Member (ErrorS 'TeamNotFound) r,
    Member (ErrorS 'InvalidOperation) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'ChannelsNotEnabled) r,
    Member (ErrorS 'NotAnMlsConversation) r,
    Member (ErrorS HistoryNotSupported) r,
    Member BackendNotificationQueueAccess r,
    Member NotificationSubsystem r,
    Member ExternalAccess r,
    Member Now r,
    Member ConversationStore r,
    Member (FederationAPIAccess FederatorClient) r,
    Member BrigAPIAccess r,
    Member FeaturesConfigSubsystem r,
    Member TeamCollaboratorsSubsystem r,
    Member Random r,
    Member TeamSubsystem r,
    Member (Input ConversationSubsystemConfig) r,
    Member LegalHoldStore r,
    Member TeamStore r,
    Member UserClientIndexStore r
  ) =>
  Sem (ConversationSubsystem : r) a ->
  Sem r a
interpretConversationSubsystem = interpret $ \case
  NotifyConversationAction tag quid notifyOrigDomain con lconv targetsLocal targetsRemote targetsBots action extraData ->
    Notify.notifyConversationActionImpl tag quid notifyOrigDomain con lconv targetsLocal targetsRemote targetsBots action extraData
  CreateGroupConversation lusr conn newConv ->
    CreateInternal.createGroupConversationGeneric lusr conn newConv
  CreateOne2OneConversation lusr conn newOne2One ->
    CreateInternal.createOne2OneConversationLogic lusr conn newOne2One
  CreateProteusSelfConversation lusr ->
    CreateInternal.createProteusSelfConversationLogic lusr
  CreateConnectConversation lusr conn j ->
    CreateInternal.createConnectConversationLogic lusr conn j
  GetConversations convIds ->
    ConvStore.getConversations convIds
  GetConversationIds lusr maxIds pagingState ->
    Fetch.getConversationIdsImpl lusr maxIds pagingState
  InternalGetClientIds uids ->
    Internal.internalGetClientIdsImpl uids
  InternalGetLocalMember cid uid ->
    ConvStore.getLocalMember cid uid
