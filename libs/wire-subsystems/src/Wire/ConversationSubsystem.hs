{-# LANGUAGE TemplateHaskell #-}

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

module Wire.ConversationSubsystem where

import Data.Id
import Data.Qualified
import Data.Singletons (Sing)
import Galley.Types.Clients (Clients)
import Imports
import Polysemy
import Wire.API.Conversation (ExtraConversationData, NewConv, NewOne2OneConv)
import Wire.API.Conversation.Action
import Wire.API.Event.Conversation
import Wire.NotificationSubsystem (LocalConversationUpdate)
import Wire.StoredConversation

data ConversationSubsystem m a where
  NotifyConversationAction ::
    Sing tag ->
    EventFrom ->
    Bool ->
    Maybe ConnId ->
    Local StoredConversation ->
    Set UserId ->
    Set (Remote UserId) ->
    Set BotMember ->
    ConversationAction (tag :: ConversationActionTag) ->
    ExtraConversationData ->
    ConversationSubsystem r LocalConversationUpdate
  CreateGroupConversation ::
    Local UserId ->
    Maybe ConnId ->
    NewConv ->
    ConversationSubsystem m StoredConversation
  CreateOne2OneConversation ::
    Local UserId ->
    ConnId ->
    NewOne2OneConv ->
    ConversationSubsystem m (StoredConversation, Bool)
  CreateProteusSelfConversation ::
    Local UserId ->
    ConversationSubsystem m (StoredConversation, Bool)
  CreateConnectConversation ::
    Local UserId ->
    Maybe ConnId ->
    Connect ->
    ConversationSubsystem m (StoredConversation, Bool)
  InternalGetClientIds :: [UserId] -> ConversationSubsystem m Clients
  InternalGetLocalMember ::
    ConvId ->
    UserId ->
    ConversationSubsystem m (Maybe LocalMember)
  DeleteConversation :: ConvId -> ConversationSubsystem m ()

makeSem ''ConversationSubsystem
