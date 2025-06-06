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

module Test.Wire.API.Federation.Golden.ConversationUpdate
  ( testObject_ConversationUpdate1V0,
    testObject_ConversationUpdate2V0,
    testObject_ConversationUpdate1,
    testObject_ConversationUpdate2,
  )
where

import Data.Domain (Domain (Domain))
import Data.Id (Id (Id), UserId)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Qualified (Qualified (Qualified))
import Data.Singletons (sing)
import Data.UUID qualified as UUID
import Imports
import Wire.API.Conversation
import Wire.API.Conversation.Action
import Wire.API.Conversation.Role (roleNameWireAdmin)
import Wire.API.Federation.API.Galley

qAlice, qBob :: Qualified UserId
qAlice =
  Qualified
    (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100004007")))
    (Domain "golden.example.com")
qBob =
  Qualified
    (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100005007")))
    (Domain "golden2.example.com")

chad, dee :: UserId
chad = Id (fromJust (UUID.fromString "00000fff-0000-0000-0000-000100005007"))
dee = Id (fromJust (UUID.fromString "00000fff-0000-aaaa-0000-000100005007"))

testObject_ConversationUpdate1V0 :: ConversationUpdateV0
testObject_ConversationUpdate1V0 =
  ConversationUpdateV0
    { cuTime = read "1864-04-12 12:22:43.673 UTC",
      cuOrigUserId =
        Qualified
          (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000007")))
          (Domain "golden.example.com"),
      cuConvId =
        Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000006")),
      cuAlreadyPresentUsers = [],
      cuAction = SomeConversationAction (sing @'ConversationJoinTag) (ConversationJoin (qAlice :| [qBob]) roleNameWireAdmin InternalAdd)
    }

testObject_ConversationUpdate2V0 :: ConversationUpdateV0
testObject_ConversationUpdate2V0 =
  ConversationUpdateV0
    { cuTime = read "1864-04-12 12:22:43.673 UTC",
      cuOrigUserId =
        Qualified
          (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000007")))
          (Domain "golden.example.com"),
      cuConvId =
        Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000006")),
      cuAlreadyPresentUsers = [chad, dee],
      cuAction = SomeConversationAction (sing @'ConversationLeaveTag) ()
    }

testObject_ConversationUpdate1 :: ConversationUpdate
testObject_ConversationUpdate1 =
  ConversationUpdate
    { time = read "1864-04-12 12:22:43.673 UTC",
      origUserId =
        Qualified
          (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000007")))
          (Domain "golden.example.com"),
      convId =
        Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000006")),
      alreadyPresentUsers = [],
      action = SomeConversationAction (sing @'ConversationJoinTag) (ConversationJoin (qAlice :| [qBob]) roleNameWireAdmin InternalAdd)
    }

testObject_ConversationUpdate2 :: ConversationUpdate
testObject_ConversationUpdate2 =
  ConversationUpdate
    { time = read "1864-04-12 12:22:43.673 UTC",
      origUserId =
        Qualified
          (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000007")))
          (Domain "golden.example.com"),
      convId =
        Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000006")),
      alreadyPresentUsers = [chad, dee],
      action = SomeConversationAction (sing @'ConversationLeaveTag) ()
    }
