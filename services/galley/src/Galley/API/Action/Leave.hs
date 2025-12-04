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

module Galley.API.Action.Leave (leaveConversation) where

import Control.Lens
import Data.Id
import Data.Qualified
import Galley.API.MLS.Removal
import Galley.API.Util
import Galley.Effects
import Imports hiding ((\\))
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog
import Wire.API.Federation.Error
import Wire.ConversationSubsystem.Interpreter (ConversationSubsystemConfig)
import Wire.NotificationSubsystem
import Wire.Sem.Now (Now)
import Wire.StoredConversation
import Wire.UserList

leaveConversation ::
  ( Member TinyLog r,
    Member (Error FederationError) r,
    Member BackendNotificationQueueAccess r,
    Member ExternalAccess r,
    Member ConversationStore r,
    Member NotificationSubsystem r,
    Member ProposalStore r,
    Member Random r,
    Member (Input ConversationSubsystemConfig) r,
    Member Now r
  ) =>
  Qualified UserId ->
  Local StoredConversation ->
  Sem r ()
leaveConversation origUser lconv = do
  let victims = [origUser]
  lconv' <- traverse (convDeleteMembers (toUserList lconv victims)) lconv
  -- send remove proposals in the MLS case
  traverse_ (removeUser lconv' RemoveUserIncludeMain) victims
