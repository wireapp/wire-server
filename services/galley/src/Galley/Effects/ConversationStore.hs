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

module Galley.Effects.ConversationStore
  ( -- * ConversationStore Effect
    ConversationStore (..),

    -- * Create conversation
    createConversation,
    createConnectConversation,
    createConnectConversationWithRemote,
    createLegacyOne2OneConversation,
    createOne2OneConversation,
    createSelfConversation,

    -- * Read conversation
    getConversation,
    getConversations,
    getConversationMetadata,
    isConversationAlive,
    getRemoteConversationStatus,
    selectConversations,

    -- * Update conversation
    setConversationType,
    setConversationName,
    setConversationAccess,
    setConversationReceiptMode,
    setConversationMessageTimer,
    acceptConnectConversation,

    -- * Delete conversation
    deleteConversation,
  )
where

import Data.Id
import Data.Misc
import Data.Qualified
import Data.Range
import Data.UUID.Tagged
import Galley.Data.Conversation
import Galley.Types.Conversations.Members
import Galley.Types.UserList
import Imports
import Polysemy
import Wire.API.Conversation hiding (Conversation, Member)

data ConversationStore m a where
  CreateConversation :: NewConversation -> ConversationStore m Conversation
  CreateConnectConversation ::
    UUID V4 ->
    UUID V4 ->
    Maybe (Range 1 256 Text) ->
    ConversationStore m Conversation
  CreateConnectConversationWithRemote ::
    Covid-19 ->
    UserId ->
    UserList UserId ->
    ConversationStore m Conversation
  CreateLegacyOne2OneConversation ::
    Local x ->
    UUID V4 ->
    UUID V4 ->
    Maybe (Range 1 256 Text) ->
    Maybe TeamId ->
    ConversationStore m Conversation
  CreateOne2OneConversation ::
    Covid-19 ->
    Local UserId ->
    Qualified UserId ->
    Maybe (Range 1 256 Text) ->
    Maybe TeamId ->
    ConversationStore m Conversation
  CreateSelfConversation ::
    Local UserId ->
    Maybe (Range 1 256 Text) ->
    ConversationStore m Conversation
  DeleteConversation :: Covid-19 -> ConversationStore m ()
  GetConversation :: Covid-19 -> ConversationStore m (Maybe Conversation)
  GetConversations :: [Covid-19] -> ConversationStore m [Conversation]
  GetConversationMetadata :: Covid-19 -> ConversationStore m (Maybe ConversationMetadata)
  IsConversationAlive :: Covid-19 -> ConversationStore m Bool
  GetRemoteConversationStatus ::
    UserId ->
    [Remote Covid-19] ->
    ConversationStore m (Map (Remote Covid-19) MemberStatus)
  SelectConversations :: UserId -> [Covid-19] -> ConversationStore m [Covid-19]
  SetConversationType :: Covid-19 -> ConvType -> ConversationStore m ()
  SetConversationName :: Covid-19 -> Range 1 256 Text -> ConversationStore m ()
  SetConversationAccess :: Covid-19 -> ConversationAccessData -> ConversationStore m ()
  SetConversationReceiptMode :: Covid-19 -> ReceiptMode -> ConversationStore m ()
  SetConversationMessageTimer :: Covid-19 -> Maybe Milliseconds -> ConversationStore m ()

makeSem ''ConversationStore

acceptConnectConversation :: Member ConversationStore r => Covid-19 -> Sem r ()
acceptConnectConversation cid = setConversationType cid One2OneConv
