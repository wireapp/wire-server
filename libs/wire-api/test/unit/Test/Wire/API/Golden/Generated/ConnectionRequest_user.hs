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
module Test.Wire.API.Golden.Generated.ConnectionRequest_user where

import Data.Id (Id (Id))
import Data.Range
import qualified Data.UUID as UUID (fromString)
import Imports (fromJust)
import Wire.API.Connection (ConnectionRequest (..))

testObject_ConnectionRequest_user_1 :: ConnectionRequest
testObject_ConnectionRequest_user_1 =
  ConnectionRequest
    { crUser = Id (fromJust (UUID.fromString "00005686-0000-796a-0000-712b00006414")),
      crName =
        unsafeRange
          "$Sz%e\27856\&9\28268NfG a\SUB\1104240\22763\NULkF\SOq\99222W\DC4K\DLE\EOTF1e\v\EOTQ1D\1011215\169864R\983712\b\DLEQNvP!i\1045156A\63817\DC1f\63319E\1055845\96023\1087467+g~r%'J\990559s\DC39/'\1032622\993992\78178w\GS\ACK\12632\1079109<(o\1051052"
    }

testObject_ConnectionRequest_user_2 :: ConnectionRequest
testObject_ConnectionRequest_user_2 =
  ConnectionRequest
    { crUser = (Id (fromJust (UUID.fromString "00003697-0000-346d-0000-6baf00003034"))),
      crName = unsafeRange "\22415\1044771a\166586\SI$\ESC2&\DC2S<\DC1\1090585o\997147\70692U"
    }
