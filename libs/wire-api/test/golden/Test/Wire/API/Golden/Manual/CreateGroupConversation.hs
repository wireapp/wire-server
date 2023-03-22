-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2023 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Wire.API.Golden.Manual.CreateGroupConversation where

import Data.Domain
import Data.Id
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.UUID as UUID (fromString)
import Imports
import Test.Wire.API.Golden.Generated.Conversation_user
import Wire.API.Conversation

unreachableDomain1, unreachableDomain2 :: Domain
unreachableDomain1 = Domain "golden-unreachable-1.example.com"
unreachableDomain2 = Domain "golden-unreachable-2.example.com"

user1, user2 :: UserId
user1 = Id (fromJust (UUID.fromString "a0000001-0000-0001-0000-000200000007"))
user2 = Id (fromJust (UUID.fromString "f0000001-b000-0001-0000-000200060005"))

testObject_CreateGroupConversation_1 :: CreateGroupConversation
testObject_CreateGroupConversation_1 =
  CreateGroupConversation
    { cgcConversation = testObject_Conversation_user_1,
      cgcFailedToAdd = Map.empty
    }

testObject_CreateGroupConversation_2 :: CreateGroupConversation
testObject_CreateGroupConversation_2 =
  CreateGroupConversation
    { cgcConversation = testObject_Conversation_user_1,
      cgcFailedToAdd =
        Map.singleton unreachableDomain1 $ Set.fromList $ [user1, user2]
    }

testObject_CreateGroupConversation_3 :: CreateGroupConversation
testObject_CreateGroupConversation_3 =
  CreateGroupConversation
    { cgcConversation = testObject_Conversation_user_1,
      cgcFailedToAdd =
        Map.fromList
          [ (unreachableDomain1, Set.singleton user1),
            (unreachableDomain2, Set.singleton user2)
          ]
    }
