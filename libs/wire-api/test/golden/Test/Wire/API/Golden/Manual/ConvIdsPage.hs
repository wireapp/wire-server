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

module Test.Wire.API.Golden.Manual.ConvIdsPage where

import Data.Domain (Domain (Domain))
import Data.Id (Id (Id))
import Data.Qualified
import qualified Data.UUID as UUID
import Imports
import Test.Wire.API.Golden.Manual.ConversationPagingState (testObject_ConversationPagingState_1)
import Wire.API.Conversation

testObject_ConvIdsPage_1 :: ConvIdsPage
testObject_ConvIdsPage_1 = ConvIdsPage [] False testObject_ConversationPagingState_1

testObject_ConvIdsPage_2 :: ConvIdsPage
testObject_ConvIdsPage_2 = ConvIdsPage [Qualified (Id (fromJust (UUID.fromString "00000018-0000-0020-0000-000e00000002"))) (Domain "domain.example.com")] True testObject_ConversationPagingState_1
