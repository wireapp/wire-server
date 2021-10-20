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

module Test.Wire.API.Federation.Golden.NewConnectionRequest where

import Data.Id
import qualified Data.UUID as UUID
import Imports
import Wire.API.Federation.API.Brig

testObject_NewConnectionRequest1 :: NewConnectionRequest
testObject_NewConnectionRequest1 =
  NewConnectionRequest
    { ncrFrom = Id (fromJust (UUID.fromString "69f66843-6cf1-48fb-8c05-1cf58c23566a")),
      ncrTo = Id (fromJust (UUID.fromString "1669240c-c510-43e0-bf1a-33378fa4ba55")),
      ncrAction = RemoteConnect
    }

testObject_NewConnectionRequest2 :: NewConnectionRequest
testObject_NewConnectionRequest2 =
  NewConnectionRequest
    { ncrFrom = Id (fromJust (UUID.fromString "69f66843-6cf1-48fb-8c05-1cf58c23566a")),
      ncrTo = Id (fromJust (UUID.fromString "1669240c-c510-43e0-bf1a-33378fa4ba55")),
      ncrAction = RemoteRescind
    }
