-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.Conversation.Action
  ( ConversationAction (..),
    conversationActionToEvent,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Id
import Data.List.NonEmpty (NonEmpty)
import Data.Qualified
import Data.Time.Clock
import Imports
import Wire.API.Arbitrary (Arbitrary, GenericUniform (..))
import Wire.API.Conversation
import Wire.API.Conversation.Role
import Wire.API.Event.Conversation
import Wire.API.Util.Aeson (CustomEncoded (..))

-- | An update to a conversation, including addition and removal of members.
-- Used to send notifications to users and to remote backends.
data ConversationAction
  = ConversationActionAddMembers (NonEmpty (Qualified UserId, RoleName))
  | ConversationActionRemoveMembers (NonEmpty (Qualified UserId))
  | ConversationActionRename ConversationRename
  | ConversationActionMessageTimerUpdate ConversationMessageTimerUpdate
  | ConversationActionMemberUpdate MemberUpdateData
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ConversationAction)
  deriving (ToJSON, FromJSON) via (CustomEncoded ConversationAction)

conversationActionToEvent ::
  UTCTime ->
  Qualified UserId ->
  Qualified ConvId ->
  ConversationAction ->
  Event
conversationActionToEvent now quid qcnv (ConversationActionAddMembers newMembers) =
  Event MemberJoin qcnv quid now $
    EdMembersJoin $ SimpleMembers (map (uncurry SimpleMember) . toList $ newMembers)
conversationActionToEvent now quid qcnv (ConversationActionRemoveMembers removedMembers) =
  Event MemberLeave qcnv quid now $
    EdMembersLeave . QualifiedUserIdList . toList $ removedMembers
conversationActionToEvent now quid qcnv (ConversationActionRename rename) =
  Event ConvRename qcnv quid now (EdConvRename rename)
conversationActionToEvent now quid qcnv (ConversationActionMessageTimerUpdate update) =
  Event ConvMessageTimerUpdate qcnv quid now (EdConvMessageTimerUpdate update)
conversationActionToEvent now quid qcnv (ConversationActionMemberUpdate update) =
  Event MemberStateUpdate qcnv quid now (EdMemberUpdate update)
