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

module Wire.ConversationSubsystem.Action.Kick where

import Data.Default
import Data.Id
import Data.Qualified
import Data.Singletons
import Imports hiding ((\\))
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog
import Wire.API.Conversation hiding (Conversation, Member)
import Wire.API.Conversation.Action
import Wire.API.Conversation.Config (ConversationSubsystemConfig)
import Wire.API.Event.LeaveReason
import Wire.API.Federation.Error
import Wire.BackendNotificationQueueAccess
import Wire.ConversationStore (ConversationStore)
import Wire.ConversationSubsystem.Action.Leave
import Wire.ConversationSubsystem.Action.Notify
import Wire.ConversationSubsystem.Util
import Wire.ExternalAccess
import Wire.NotificationSubsystem
import Wire.ProposalStore (ProposalStore)
import Wire.Sem.Now (Now)
import Wire.Sem.Random (Random)
import Wire.StoredConversation

-- | Kick a user from a conversation and send notifications.
--
-- This function removes the given victim from the conversation by making them
-- leave, but then sends notifications as if the user was removed by someone
-- else.
kickMember ::
  ( Member BackendNotificationQueueAccess r,
    Member (Error FederationError) r,
    Member ExternalAccess r,
    Member NotificationSubsystem r,
    Member ProposalStore r,
    Member Now r,
    Member (Input ConversationSubsystemConfig) r,
    Member ConversationStore r,
    Member TinyLog r,
    Member Random r
  ) =>
  Qualified UserId ->
  Local StoredConversation ->
  BotsAndMembers ->
  Qualified UserId ->
  Sem r ()
kickMember qusr = kickMemberWith qusr Nothing EdReasonRemoved

-- | Like 'kickMember', but with an explicit originating connection and leave
-- reason.
--
-- Removal paths that are not a member removing another member need to report a
-- different reason: team member deletion and team collaborator removal use
-- 'EdReasonDeleted'.
kickMemberWith ::
  ( Member BackendNotificationQueueAccess r,
    Member (Error FederationError) r,
    Member ExternalAccess r,
    Member NotificationSubsystem r,
    Member ProposalStore r,
    Member Now r,
    Member (Input ConversationSubsystemConfig) r,
    Member ConversationStore r,
    Member TinyLog r,
    Member Random r
  ) =>
  Qualified UserId ->
  Maybe ConnId ->
  EdMemberLeftReason ->
  Local StoredConversation ->
  BotsAndMembers ->
  Qualified UserId ->
  Sem r ()
kickMemberWith qusr conn reason lconv targets victim = void . runError @NoChanges $ do
  leaveConversation victim lconv
  sendConversationActionNotifications
    (sing @'ConversationRemoveMembersTag)
    qusr
    True
    conn
    lconv
    targets
    (ConversationRemoveMembers (pure victim) reason)
    def
