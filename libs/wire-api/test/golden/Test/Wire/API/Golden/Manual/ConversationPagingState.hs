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

module Test.Wire.API.Golden.Manual.ConversationPagingState where

import qualified Data.ByteString as BS
import Imports
import Wire.API.Conversation
import Wire.API.Routes.MultiTablePaging

testObject_ConversationPagingState_1 :: ConversationPagingState
testObject_ConversationPagingState_1 = ConversationPagingState PagingLocals Nothing

testObject_ConversationPagingState_2 :: ConversationPagingState
testObject_ConversationPagingState_2 = ConversationPagingState PagingLocals (Just (BS.pack [0, 1, 88, 99, 125, 88]))

testObject_ConversationPagingState_3 :: ConversationPagingState
testObject_ConversationPagingState_3 = ConversationPagingState PagingRemotes Nothing

testObject_ConversationPagingState_4 :: ConversationPagingState
testObject_ConversationPagingState_4 = ConversationPagingState PagingRemotes (Just (BS.pack [88, 12, 23]))
