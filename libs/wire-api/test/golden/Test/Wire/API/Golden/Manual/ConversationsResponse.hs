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

module Test.Wire.API.Golden.Manual.ConversationsResponse
  ( testObject_ConversationsResponse_1,
  )
where

import Data.Domain
import Data.Id (Id (Id))
import Data.Qualified
import Data.UUID qualified as UUID
import Imports
import Test.Wire.API.Golden.Generated.Conversation_user
import Wire.API.Conversation

domain :: Domain
domain = Domain "golden.example.com"

testObject_ConversationsResponse_1 :: ConversationsResponse
testObject_ConversationsResponse_1 =
  ConversationsResponse
    { crFound = [testObject_Conversation_user_5, testObject_Conversation_user_3],
      crNotFound =
        [ Qualified (Id (fromJust (UUID.fromString "00000018-0000-0020-0000-000e00000002"))) domain,
          Qualified (Id (fromJust (UUID.fromString "00000018-0000-0020-0000-111111111112"))) (Domain "golden2.example.com")
        ],
      crFailed =
        [ Qualified (Id (fromJust (UUID.fromString "00000018-4444-0020-0000-000e00000002"))) domain,
          Qualified (Id (fromJust (UUID.fromString "99999999-0000-0020-0000-111111111112"))) (Domain "golden3.example.com")
        ]
    }
