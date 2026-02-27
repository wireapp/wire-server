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

module Galley.Effects.DeleteConversationSubsystem
  ( interpretDeleteConversationSubsystem,
  )
where

import Data.Qualified
import Galley.API.Update qualified as API
import Imports
import Polysemy
import Wire.API.Conversation.Config (ConversationSubsystemConfig)
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Federation.Client (FederatorClient)
import Wire.API.Federation.Error
import Wire.BackendNotificationQueueAccess (BackendNotificationQueueAccess)
import Wire.BrigAPIAccess (BrigAPIAccess)
import Wire.CodeStore (CodeStore)
import Wire.ConversationStore (ConversationStore, MLSCommitLockStore)
import Wire.ConversationSubsystem (ConversationSubsystem)
import Wire.DeleteConversationSubsystem
import Wire.FederationAPIAccess (FederationAPIAccess)
import Wire.FederationSubsystem (FederationSubsystem)
import Wire.ProposalStore (ProposalStore)
import Wire.TeamCollaboratorsSubsystem (TeamCollaboratorsSubsystem)
import Wire.TeamStore (TeamStore)
import Wire.TeamSubsystem (TeamSubsystem)
import Polysemy.Error
import Polysemy.Input

interpretDeleteConversationSubsystem ::
  ( Member BrigAPIAccess r,
    Member BackendNotificationQueueAccess r,
    Member CodeStore r,
    Member ConversationStore r,
    Member (Error FederationError) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS ('ActionDenied 'DeleteConversation)) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'InvalidOperation) r,
    Member (FederationAPIAccess FederatorClient) r,
    Member ConversationSubsystem r,
    Member ProposalStore r,
    Member TeamStore r,
    Member TeamCollaboratorsSubsystem r,
    Member MLSCommitLockStore r,
    Member FederationSubsystem r,
    Member TeamSubsystem r,
    Member (Input ConversationSubsystemConfig) r
  ) =>
  Sem (DeleteConversationSubsystem : r) a ->
  Sem r a
interpretDeleteConversationSubsystem = interpret $ \case
  DeleteConversation lusr con _tid cnv -> do
    let lcnv = qualifyAs lusr cnv
    void $ API.deleteLocalConversation lusr con lcnv
