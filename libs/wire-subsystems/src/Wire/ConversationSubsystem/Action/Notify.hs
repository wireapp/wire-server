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

module Wire.ConversationSubsystem.Action.Notify where

import Data.Id
import Data.Qualified
import Data.Singletons
import Imports hiding ((\\))
import Polysemy
import Polysemy.Error
import Wire.API.Conversation hiding (Conversation, Member)
import Wire.API.Conversation.Action
import Wire.API.Event.Conversation
import Wire.API.Federation.Error
import Wire.BackendNotificationQueueAccess (BackendNotificationQueueAccess)
import Wire.ConversationSubsystem.Notify (notifyConversationActionImpl)
import Wire.ConversationSubsystem.Util
import Wire.ExternalAccess (ExternalAccess)
import Wire.NotificationSubsystem
import Wire.Sem.Now (Now)
import Wire.StoredConversation

sendConversationActionNotifications ::
  forall tag r.
  ( Member BackendNotificationQueueAccess r,
    Member ExternalAccess r,
    Member (Error FederationError) r,
    Member Now r,
    Member NotificationSubsystem r
  ) =>
  Sing tag ->
  Qualified UserId ->
  Bool ->
  Maybe ConnId ->
  Local StoredConversation ->
  BotsAndMembers ->
  ConversationAction (tag :: ConversationActionTag) ->
  ExtraConversationData ->
  Sem r LocalConversationUpdate
sendConversationActionNotifications tag quid notifyOrigDomain con lconv targets action extraData = do
  notifyConversationActionImpl
    tag
    (EventFromUser quid)
    notifyOrigDomain
    con
    lconv
    (bmLocals targets)
    (bmRemotes targets)
    (bmBots targets)
    action
    extraData
