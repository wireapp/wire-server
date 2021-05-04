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

import Data.Id (UserId)
import Imports
import Servant.API (JSON, Post, ReqBody, (:>))
import Servant.API.Generic ((:-))
import Wire.API.Conversation (Conversation)
import Wire.API.Federation.Event (ConversationEvent, MembersJoin)

-- FUTUREWORK: data types, json instances, more endpoints. See
-- https://wearezeta.atlassian.net/wiki/spaces/CORE/pages/356090113/Federation+Galley+Conversation+API
-- for the current list we need.

data Api routes = Api
  { getConversation ::
      routes
        :- "federation"
        :> "conversations"
        -- usecases:
        -- - e.g. upon registering a new client to your account, get the list of your conversations
        :> "list-conversations"
        :> ReqBody '[JSON] UserId
        :> Post '[JSON] [Conversation],
    -- Notify callee that [uid1@callee, ..] were added convid@caller
    conversationMembersJoin ::
      routes
        :- "federation"
        :> "conversations"
        :> "members-join"
        :> ReqBody '[JSON] (ConversationEvent MembersJoin)
        :> Post '[JSON] ()
  }
  deriving (Generic)
