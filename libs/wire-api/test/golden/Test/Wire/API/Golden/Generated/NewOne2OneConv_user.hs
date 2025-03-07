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

module Test.Wire.API.Golden.Generated.NewOne2OneConv_user where

import Data.Id
import Data.UUID qualified as UUID (fromString)
import Imports
import Wire.API.Conversation

testObject_NewOne2OneConv_user_1 :: NewOne2OneConv
testObject_NewOne2OneConv_user_1 =
  NewOne2OneConv
    { users =
        [ Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")),
          Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))
        ],
      qualifiedUsers = [],
      name = Nothing,
      team =
        Just
          ( ConvTeamInfo
              { cnvTeamId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))
              }
          )
    }

testObject_NewOne2OneConv_user_3 :: NewOne2OneConv
testObject_NewOne2OneConv_user_3 =
  NewOne2OneConv
    { users = [],
      qualifiedUsers = [],
      name = Nothing,
      team = Nothing
    }
