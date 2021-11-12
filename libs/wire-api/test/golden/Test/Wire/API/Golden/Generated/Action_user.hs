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
module Test.Wire.API.Golden.Generated.Action_user where

import Wire.API.Conversation.Role (Action (..))

testObject_Action_user_1 :: Action
testObject_Action_user_1 = AddConversationMember

testObject_Action_user_2 :: Action
testObject_Action_user_2 = RemoveConversationMember

testObject_Action_user_3 :: Action
testObject_Action_user_3 = ModifyConversationName

testObject_Action_user_4 :: Action
testObject_Action_user_4 = ModifyConversationMessageTimer

testObject_Action_user_5 :: Action
testObject_Action_user_5 = ModifyConversationReceiptMode

testObject_Action_user_6 :: Action
testObject_Action_user_6 = ModifyConversationAccess

testObject_Action_user_7 :: Action
testObject_Action_user_7 = ModifyOtherConversationMember

testObject_Action_user_8 :: Action
testObject_Action_user_8 = LeaveConversation

testObject_Action_user_9 :: Action
testObject_Action_user_9 = DeleteConversation
