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
module Test.Wire.API.Golden.Generated.UserConnectionList_user where

import Data.Domain (Domain (..))
import Data.Id (Id (Id))
import Data.Json.Util (readUTCTimeMillis)
import Data.Qualified (Qualified (..))
import qualified Data.UUID as UUID (fromString)
import Imports (Bool (..), Maybe (..), fromJust)
import Wire.API.Connection
  ( Relation (..),
    UserConnection (..),
    UserConnectionList (..),
  )

testObject_UserConnectionList_user_1 :: UserConnectionList
testObject_UserConnectionList_user_1 =
  UserConnectionList
    { clConnections =
        [ UserConnection
            { ucFrom = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")),
              ucTo = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))) (Domain "faraway.golden.example.com"),
              ucStatus = Pending,
              ucLastUpdate = fromJust (readUTCTimeMillis "1864-05-09T06:44:37.367Z"),
              ucConvId = Nothing
            },
          UserConnection
            { ucFrom = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")),
              ucTo = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))) (Domain "faraway.golden.example.com"),
              ucStatus = Accepted,
              ucLastUpdate = fromJust (readUTCTimeMillis "1864-05-09T00:43:52.049Z"),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))
            }
        ],
      clHasMore = False
    }

testObject_UserConnectionList_user_2 :: UserConnectionList
testObject_UserConnectionList_user_2 = UserConnectionList {clConnections = [], clHasMore = True}
