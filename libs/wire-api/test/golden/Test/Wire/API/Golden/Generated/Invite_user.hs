{-# LANGUAGE OverloadedLists #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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
module Test.Wire.API.Golden.Generated.Invite_user where

import Data.Id (Id (Id))
import qualified Data.List.NonEmpty as NonEmpty (fromList)
import Data.List1 (List1 (List1))
import qualified Data.UUID as UUID (fromString)
import Imports (fromJust)
import Wire.API.Conversation (Invite (..))
import Wire.API.Conversation.Role (parseRoleName, roleNameWireAdmin)

testObject_Invite_user_1 :: Invite
testObject_Invite_user_1 =
  Invite
    { invUsers =
        List1
          ( NonEmpty.fromList
              [ Id (fromJust (UUID.fromString "00000002-0000-0058-0000-003c00000079")),
                Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))
              ]
          ),
      invRoleName =
        fromJust
          (parseRoleName "t0xs1a2pemtt5f133cklsuqsxvrq25q5awgxjbuf5m2hf679oxxjcop794lmnuj2rd3t1sp5qya0tmn4qhpw2wxepd")
    }

testObject_Invite_user_2 :: Invite
testObject_Invite_user_2 =
  Invite
    { invUsers =
        List1
          ( NonEmpty.fromList
              [Id (fromJust (UUID.fromString "00000002-0000-0058-0000-003c00000079"))]
          ),
      invRoleName = roleNameWireAdmin
    }
