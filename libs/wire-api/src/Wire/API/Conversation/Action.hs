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
    conversationActionTag,
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

-- | A sum type consisting of all possible conversation actions.
data ConversationAction
  = ConversationActionAddMembers (NonEmpty (Qualified UserId)) RoleName
  | ConversationActionRemoveMembers (NonEmpty (Qualified UserId))
  | ConversationActionRename ConversationRename
  | ConversationActionMessageTimerUpdate ConversationMessageTimerUpdate
  | ConversationActionReceiptModeUpdate ConversationReceiptModeUpdate
  | ConversationActionMemberUpdate (Qualified UserId) OtherMemberUpdate
  | ConversationActionAccessUpdate ConversationAccessData
  | ConversationActionDelete
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ConversationAction)
  deriving (ToJSON, FromJSON) via (CustomEncoded ConversationAction)

conversationActionToEvent ::
  UTCTime ->
  Qualified UserId ->
  Qualified ConvId ->
  ConversationAction ->
  Event
conversationActionToEvent now quid qcnv (ConversationActionAddMembers newMembers role) =
  Event MemberJoin qcnv quid now $
    EdMembersJoin $ SimpleMembers (map (`SimpleMember` role) (toList newMembers))
conversationActionToEvent now quid qcnv (ConversationActionRemoveMembers removedMembers) =
  Event MemberLeave qcnv quid now $
    EdMembersLeave (QualifiedUserIdList (toList removedMembers))
conversationActionToEvent now quid qcnv (ConversationActionRename rename) =
  Event ConvRename qcnv quid now (EdConvRename rename)
conversationActionToEvent now quid qcnv (ConversationActionMessageTimerUpdate update) =
  Event ConvMessageTimerUpdate qcnv quid now (EdConvMessageTimerUpdate update)
conversationActionToEvent now quid qcnv (ConversationActionReceiptModeUpdate update) =
  Event ConvReceiptModeUpdate qcnv quid now (EdConvReceiptModeUpdate update)
conversationActionToEvent now quid qcnv (ConversationActionMemberUpdate target (OtherMemberUpdate role)) =
  let update = MemberUpdateData target Nothing Nothing Nothing Nothing Nothing Nothing role
   in Event MemberStateUpdate qcnv quid now (EdMemberUpdate update)
conversationActionToEvent now quid qcnv (ConversationActionAccessUpdate update) =
  Event ConvAccessUpdate qcnv quid now (EdConvAccessUpdate update)
conversationActionToEvent now quid qcnv ConversationActionDelete =
  Event ConvDelete qcnv quid now EdConvDelete

conversationActionTag :: Qualified UserId -> ConversationAction -> Action
conversationActionTag _ (ConversationActionAddMembers _ _) = AddConversationMember
conversationActionTag qusr (ConversationActionRemoveMembers victims)
  | pure qusr == victims = LeaveConversation
  | otherwise = RemoveConversationMember
conversationActionTag _ (ConversationActionRename _) = ModifyConversationName
conversationActionTag _ (ConversationActionMessageTimerUpdate _) = ModifyConversationMessageTimer
conversationActionTag _ (ConversationActionReceiptModeUpdate _) = ModifyConversationReceiptMode
conversationActionTag _ (ConversationActionMemberUpdate _ _) = ModifyOtherConversationMember
conversationActionTag _ (ConversationActionAccessUpdate _) = ModifyConversationAccess
conversationActionTag _ ConversationActionDelete = DeleteConversation
