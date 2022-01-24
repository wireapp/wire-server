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

module Test.Wire.API.Federation.Golden.LeaveConversationResponse where

import Imports
import Wire.API.Federation.API.Galley

testObject_LeaveConversationResponse1 :: LeaveConversationResponse
testObject_LeaveConversationResponse1 = LeaveConversationResponse $ Right ()

testObject_LeaveConversationResponse2 :: LeaveConversationResponse
testObject_LeaveConversationResponse2 = LeaveConversationResponse $ Left RemoveFromConversationErrorRemovalNotAllowed

testObject_LeaveConversationResponse3 :: LeaveConversationResponse
testObject_LeaveConversationResponse3 = LeaveConversationResponse $ Left RemoveFromConversationErrorNotFound

testObject_LeaveConversationResponse8 :: LeaveConversationResponse
testObject_LeaveConversationResponse8 = LeaveConversationResponse $ Left RemoveFromConversationErrorUnchanged
