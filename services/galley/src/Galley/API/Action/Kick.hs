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

module Galley.API.Action.Kick where

import Data.Default
import Data.Id
import Data.Qualified
import Data.Singletons
import Galley.API.Action.Leave
import Galley.API.Action.Notify
import Galley.API.Util
import Galley.Effects
import Galley.Env (Env)
import Imports hiding ((\\))
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog
import Wire.API.Conversation hiding (Conversation, Member)
import Wire.API.Conversation.Action
import Wire.API.Event.LeaveReason
import Wire.API.Federation.Error
import Wire.ConversationSubsystem
import Wire.NotificationSubsystem
import Wire.Sem.Now (Now)
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
    Member ConversationSubsystem r,
    Member NotificationSubsystem r,
    Member ProposalStore r,
    Member Now r,
    Member (Input Env) r,
    Member ConversationStore r,
    Member TinyLog r,
    Member Random r
  ) =>
  Qualified UserId ->
  Local StoredConversation ->
  BotsAndMembers ->
  Qualified UserId ->
  Sem r ()
kickMember qusr lconv targets victim = void . runError @NoChanges $ do
  leaveConversation victim lconv
  sendConversationActionNotifications
    (sing @'ConversationRemoveMembersTag)
    qusr
    True
    Nothing
    lconv
    targets
    (ConversationRemoveMembers (pure victim) EdReasonRemoved)
    def
