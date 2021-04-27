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

import Data.Id (ConvId)
import Data.Text
import Imports
import Servant.API (JSON, Post, ReqBody, (:>))
import Servant.API.Generic ((:-))
import Wire.API.Arbitrary
import qualified Wire.API.Event.Conversation as Public

data Api routes = Api
  { conversationRename ::
      routes
        :- "federation"
        :> "conversations"
        -- usecase: Alice@A wants to change the title of a conversation containing her but hosted in B.
        :> "rename"
        :> ReqBody '[JSON] RenameConversation
        :> Post '[JSON] RenameConversationResponse,
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
        :> Post '[JSON] ConversationMemberChangeResponse,
    addToConversation ::
      routes
        :- "federation"
        :> "conversations"
        -- can we merge the "add member to conversation" (claiming to be a user already in that conv)
        -- with "join conversation by id" (where we only look at the conv id, not at a user id.
        -- ? Question of trust and/or possibility for bugs?
        --
        -- Usecase: given conversation-1@A with members Alice@A, Bob@B
        --
        -- Bob@B adds BobTwo@B: /add-to-conversation(bob2)@B
        --   B -> A: add Bob2 to your conversation, please
        :> "join"
        :> ReqBody '[JSON] JoinConversation
        :> Post '[JSON] JoinConversationResponse
  }

data RenameConversation = RenameConversation
  { renameConv :: ConvId,
    renameNewName :: Text
  }
  deriving (Show, Eq, Generic, Typeable)
  deriving (Arbitrary) via (GenericUniform RenameConversation)

newtype RenameConversationResponse = RenameConversationResponse
  { renameEvent :: Public.Event
  }
  deriving (Show, Eq, Generic, Typeable)
  deriving (Arbitrary) via (GenericUniform RenameConversationResponse)

-- FUTUREWORK: data types, json instances, more endpoints. See https://wearezeta.atlassian.net/wiki/spaces/CORE/pages/356090113/Federation+Galley+Conversation+API for the current list we need.
type ConversationMemberChange = ()

type ConversationMemberChangeResponse = ()

type JoinConversation = ()

type JoinConversationResponse = ()
