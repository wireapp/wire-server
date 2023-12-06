-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2023 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Wire.API.Golden.Manual.ConversationRemoveMembers where

import Data.Domain
import Data.Id
import Data.List.NonEmpty
import Data.Qualified
import Data.UUID qualified as UUID
import Imports
import Wire.API.Conversation
import Wire.API.Event.LeaveReason

domain1, domain2 :: Domain
domain1 = Domain "example.com"
domain2 = Domain "test.net"

user1, user2 :: Qualified UserId
user1 = Qualified (Id . fromJust $ UUID.fromString "4f201a43-935e-4e19-8fe0-0a878d3d6e74") domain1
user2 = Qualified (Id . fromJust $ UUID.fromString "eb48b095-d96f-4a94-b4ec-2a1d61447e13") domain2

testObject_ConversationRemoveMembers_1 :: ConversationRemoveMembers
testObject_ConversationRemoveMembers_1 =
  ConversationRemoveMembers
    { crmTargets = pure user1,
      crmReason = EdReasonLeft
    }

testObject_ConversationRemoveMembers_2 :: ConversationRemoveMembers
testObject_ConversationRemoveMembers_2 =
  ConversationRemoveMembers
    { crmTargets = user1 :| [user2],
      crmReason = EdReasonDeleted
    }

testObject_ConversationRemoveMembers_3 :: ConversationRemoveMembers
testObject_ConversationRemoveMembers_3 =
  ConversationRemoveMembers
    { crmTargets = pure user2,
      crmReason = EdReasonRemoved
    }
