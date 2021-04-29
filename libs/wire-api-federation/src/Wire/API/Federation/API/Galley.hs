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

data Api routes = Api
  { getConversation ::
      routes
        :- "federation"
        :> "conversations"
        -- usecases:
        -- - e.g. upon registering a new client to your account, get the list of your conversations
        :> "list-conversations"
        :> ReqBody '[JSON] ListConversations
        :> Post '[JSON] ListConversationsResponse,
    conversationMemberChange ::
      routes
        :- "federation"
        :> "conversations"
        -- for the usecase:
        -- given alice,alice2 @A and bob @B:
        -- alice adds Bob: /add-to-conversation(bob)@A
        --   A -> B: check bob exists on B
        --   A: add B to conversation database entry
        --   A -> B: by the way, B is now in one of my conversations.
        --   (B writes this in its DB: "Bob exists in a conversation with ID 1 in A)
        :> "member-change"
        :> ReqBody '[JSON] ConversationMemberChange
        :> Post '[JSON] ConversationMemberChangeResponse
  }

-- FUTUREWORK: data types, json instances, more endpoints. See https://wearezeta.atlassian.net/wiki/spaces/CORE/pages/356090113/Federation+Galley+Conversation+API for the current list we need.
type ConversationMemberChange = ()

type ConversationMemberChangeResponse = ()

type ListConversations = ()

type ListConversationsResponse = ()
