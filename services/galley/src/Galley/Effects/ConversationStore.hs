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

module Galley.Effects.ConversationStore
  ( -- * ConversationStore Effect
    ConversationStore (..),

    -- * Create conversation
    createConversationId,
    createConversation,

    -- * Read conversation
    getConversation,
    getConversationByGroupId,
    getConversations,
    getConversationMetadata,
    isConversationAlive,
    getRemoteConversationStatus,
    selectConversations,
    getConversationIdByGroupId,

    -- * Update conversation
    setConversationType,
    setConversationName,
    setConversationAccess,
    setConversationReceiptMode,
    setConversationMessageTimer,
    acceptConnectConversation,
    setGroupId,

    -- * Delete conversation
    deleteConversation,
  )
where

import Data.Id
import Data.Misc
import Data.Qualified
import Data.Range
import Galley.Data.Conversation
import Galley.Types.Conversations.Members
import Imports
import Polysemy
import Wire.API.Conversation hiding (Conversation, Member)

data ConversationStore m a where
  CreateConversationId :: ConversationStore m ConvId
  CreateConversation :: Local ConvId -> NewConversation -> ConversationStore m Conversation
  DeleteConversation :: ConvId -> ConversationStore m ()
  GetConversation :: ConvId -> ConversationStore m (Maybe Conversation)
  GetConversationByGroupId :: GroupId -> ConversationStore m (Maybe (Qualified ConvId))
  GetConversations :: [ConvId] -> ConversationStore m [Conversation]
  GetConversationMetadata :: ConvId -> ConversationStore m (Maybe ConversationMetadata)
  IsConversationAlive :: ConvId -> ConversationStore m Bool
  GetRemoteConversationStatus ::
    UserId ->
    [Remote ConvId] ->
    ConversationStore m (Map (Remote ConvId) MemberStatus)
  SelectConversations :: UserId -> [ConvId] -> ConversationStore m [ConvId]
  SetConversationType :: ConvId -> ConvType -> ConversationStore m ()
  SetConversationName :: ConvId -> Range 1 256 Text -> ConversationStore m ()
  SetConversationAccess :: ConvId -> ConversationAccessData -> ConversationStore m ()
  SetConversationReceiptMode :: ConvId -> ReceiptMode -> ConversationStore m ()
  SetConversationMessageTimer :: ConvId -> Maybe Milliseconds -> ConversationStore m ()
  GetConversationIdByGroupId :: GroupId -> ConversationStore m (Maybe (Qualified ConvId))
  SetGroupId :: GroupId -> Qualified ConvId -> ConversationStore m ()

makeSem ''ConversationStore

acceptConnectConversation :: Member ConversationStore r => ConvId -> Sem r ()
acceptConnectConversation cid = setConversationType cid One2OneConv
