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

module Galley.Data.Conversation.Types where

import Data.Id
import Galley.Types.Conversations.Members
import Galley.Types.UserList
import Galley.Validation
import Imports
import Wire.API.Conversation hiding (Conversation)
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role

-- | Internal conversation type, corresponding directly to database schema.
-- Should never be sent to users (and therefore doesn't have 'FromJSON' or
-- 'ToJSON' instances).
data Conversation = Conversation
  { convId :: ConvId,
    convLocalMembers :: [LocalMember],
    convRemoteMembers :: [RemoteMember],
    convDeleted :: Maybe Bool,
    convMetadata :: ConversationMetadata,
    convProtocol :: Protocol
  }
  deriving (Show)

data NewConversation = NewConversation
  { ncMetadata :: ConversationMetadata,
    ncUsers :: ConvSizeChecked UserList UserId,
    ncRole :: RoleName,
    ncProtocol :: ProtocolTag
  }
