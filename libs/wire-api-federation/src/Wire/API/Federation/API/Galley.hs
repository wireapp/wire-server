{-# LANGUAGE DerivingVia #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.Federation.API.Galley where

import Servant.API (JSON, Post, ReqBody, (:>))
import Servant.API.Generic ((:-))

type ConversationMemberChange = ()

type ConversationMemberChangeResponse = ()

type JoinConversation = ()

type JoinConversationResponse = ()

data Api routes = Api
  { conversationMemberChange ::
      routes
        :- "federation"
        :> "notify-a-conversation-update"
        :> ReqBody '[JSON] ConversationMemberChange
        :> Post '[JSON] ConversationMemberChangeResponse,
    addToConversation ::
      routes
        :- "federation"
        :> "join-conversation"
        :> ReqBody '[JSON] JoinConversation
        :> Post '[JSON] JoinConversationResponse
  }

-- Usecase:
--
-- backend A: conversation 1 (members: alice@A alice2@A)
-- backend B: Bob, Bob2
--
--
-- alice adds Bob: /add-to-conversation(bob)@A
--   A -> B: check bob exists on B
--   A: add B to conversation database entry
--   A -> B: by the way, B is now in one of my conversations. (B writes in its DB: Bob exists in a conversation in A)
--
-- Bob adds Bob2: /add-to-conversation(bob2)@B
--   B -> A: add Bob2 to your conversation
--
--
-- Bob adds Charlie@C /add-to-conversation(charlie)@B
--   B -> C check Charlie exists?
--   B -> A: Add Charlie@C to your conversation 1.
--
