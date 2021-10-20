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
module Test.Wire.API.Golden.Generated.ClientMismatch_user where

import Data.Id (ClientId (ClientId, client), Id (Id))
import Data.Json.Util (toUTCTimeMillis)
import qualified Data.UUID as UUID (fromString)
import GHC.Exts (IsList (fromList))
import Imports (fromJust, read)
import Wire.API.Message (ClientMismatch (ClientMismatch), UserClients (UserClients, userClients))

testObject_ClientMismatch_user_1 :: ClientMismatch
testObject_ClientMismatch_user_1 =
  ClientMismatch
    (toUTCTimeMillis (read "1864-04-12 12:22:43.673 UTC"))
    ( UserClients
        { userClients =
            fromList
              [ ( Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000002")),
                  fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                ),
                ( Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000000")),
                  fromList []
                )
              ]
        }
    )
    (UserClients {userClients = fromList []})
    ( UserClients
        { userClients =
            fromList
              [ ( Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000700000000")),
                  fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                ),
                ( Id (fromJust (UUID.fromString "00000005-0000-0000-0000-000600000008")),
                  fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                )
              ]
        }
    )
