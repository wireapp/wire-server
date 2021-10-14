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

module Test.Wire.API.Federation.Golden.ConversationDelete where

import Data.Domain (Domain (Domain))
import Data.Id (Id (Id))
import Data.Qualified (Qualified (Qualified))
import qualified Data.UUID as UUID
import Imports
import Wire.API.Federation.API.Galley (ConversationDelete (..))

testObject_ConversationDelete1 :: ConversationDelete
testObject_ConversationDelete1 =
  ConversationDelete
    { cdTime = read "1864-04-12 12:22:43.673 UTC",
      cdOriginUserId =
        Qualified
          (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000007")))
          (Domain "golden.example.com"),
      cdConvId =
        Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000006")),
      cdMembers =
        [ Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000008")),
          Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000009"))
        ]
    }
