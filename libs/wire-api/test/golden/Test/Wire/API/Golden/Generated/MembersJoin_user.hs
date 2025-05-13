{-# LANGUAGE OverloadedLists #-}

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

module Test.Wire.API.Golden.Generated.MembersJoin_user where

import Data.Domain
import Data.Id (Id (Id))
import Data.Qualified
import Data.UUID qualified as UUID (fromString)
import Imports (fromJust)
import Wire.API.Conversation.Role (parseRoleName)
import Wire.API.Event.Conversation

testObject_MembersJoin_user_1 :: MembersJoin
testObject_MembersJoin_user_1 =
  MembersJoin
    { mMembers =
        [ SimpleMember
            { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "0000001f-0000-002b-0000-005500000013"))) (Domain "faraway.example.com"),
              smConvRoleName = fromJust (parseRoleName "n0_wu2h66nj3lerw_blivsh6by09a")
            }
        ],
      addType = InternalAdd
    }

testObject_MembersJoin_user_2 :: MembersJoin
testObject_MembersJoin_user_2 =
  MembersJoin {mMembers = [], addType = InternalAdd}
