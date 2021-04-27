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
        -- for the usecase:
        -- given alice,alice2 @A and bob @B:
        -- alice adds Bob: /add-to-conversation(bob)@A
        --   A -> B: check bob exists on B
        --   A: add B to conversation database entry
        --   A -> B: by the way, B is now in one of my conversations.
        --   (B writes this in its DB: "Bob exists in a conversation with ID 1 in A)
        :> "conversation-member-change"
        :> ReqBody '[JSON] ConversationMemberChange
        :> Post '[JSON] ConversationMemberChangeResponse,
    addToConversation ::
      routes
        :- "federation"
        -- can we merge the "add member to conversation" (claiming to be a user already in that conv)
        -- with "join conversation by id" (where we only look at the conv id, not at a user id.
        -- ?
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
--
-- Galley conversation client-server API:
--
-- yes - get conversation (self, cnvId)
-- yes - get conversation roles (self, cnvId)
-- yes - join conversation by id
-- yes - add users to existing conv
-- yes - rename conversation
-- [yes] delete member of a conversation
-- [yes] send a message! (cnvId/otr/messages)
--
-- [yes] other member update (e.g. make someone conv admin)
-- [yes? low prio] - join conversation by reusable code
-- [yes? low prio] code check, get conversation code
-- [yes? low prio] update conversation access mode (e.g. allow guests into a conv)
-- [yes? low prio] - change read receipts mode on a conv
-- [yes? low prio] - change timer settings on a conv
--
-- [no] get a user's conversation IDs (self)
-- [no] get a user's conversation by ID/start (self)
-- [no] create group conversation (self, newConvData)
-- [no] create self conversation (self)
-- [?] create one2one conversation (self, newConvData)
-- [no] update/delete conversation code
-- [no] get/put self conv
-- [skip it not useful?] typing notifications
-- [maybe?] otr/broadcast (i.e. account status updates)
--
-- [?] teams/:tid/conversations/ endpoints:
--  - get (by id, all)
--  - get roles
--  - delete
