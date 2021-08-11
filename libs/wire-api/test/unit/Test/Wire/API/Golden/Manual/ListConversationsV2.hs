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

module Test.Wire.API.Golden.Manual.ListConversationsV2 where

import Data.Domain (Domain (Domain))
import Data.Id (Id (Id))
import Data.Qualified (Qualified (Qualified))
import qualified Data.UUID as UUID
import Imports
import Wire.API.Conversation (ListConversationsV2 (..))
import Data.Range (unsafeRange)

testObject_ListConversationsV2_1 :: ListConversationsV2
testObject_ListConversationsV2_1 =
  ListConversationsV2
    ( unsafeRange
        [ Qualified (Id (fromJust (UUID.fromString "00000018-0000-0020-0000-000e00000002"))) (Domain "domain.example.com"),
          Qualified (Id (fromJust (UUID.fromString "00000018-0000-0020-0000-111111111112"))) (Domain "domain2.example.com")
        ]
    )
