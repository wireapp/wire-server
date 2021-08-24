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

module Test.Wire.API.Federation.Golden.ConversationMemberUpdate where

import Data.Domain (Domain (Domain))
import Data.Id (Id (Id), UserId)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Qualified (Qualified (Qualified))
import qualified Data.UUID as UUID
import Imports
import Wire.API.Conversation.Role (roleNameWireAdmin, roleNameWireMember)
import Wire.API.Federation.API.Galley (ConversationMemberUpdate (..), ConversationMembersAction (..))

-- data ConversationMembersAction
--   = ConversationMembersActionAdd (NonEmpty (Qualified UserId, RoleName))
--   | ConversationMembersActionRemove (NonEmpty (Qualified UserId))
--   deriving stock (Eq, Show, Generic)
--   deriving (Arbitrary) via (GenericUniform ConversationMembersAction)
--   deriving (ToJSON, FromJSON) via (CustomEncoded ConversationMembersAction)

-- data ConversationMemberUpdate = ConversationMemberUpdate
--   { cmuTime :: UTCTime,
--     cmuOrigUserId :: Qualified UserId,
--     cmuConvId :: Qualified ConvId,
--     -- | A list of users from a remote backend that need to be sent
--     -- notifications about this change. This is required as we do not expect a
--     -- non-conversation owning backend to have an indexed mapping of
--     -- conversation to users.
--     cmuAlreadyPresentUsers :: [UserId],
--     -- | Users that got either added to or removed from the conversation.
--     cmuAction :: ConversationMembersAction
--   }

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

testObject_ConversationMemberUpdate1 :: ConversationMemberUpdate
testObject_ConversationMemberUpdate1 =
  ConversationMemberUpdate
    { cmuTime = read "1864-04-12 12:22:43.673 UTC",
      cmuOrigUserId =
        Qualified
          (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000007")))
          (Domain "golden.example.com"),
      cmuConvId =
        Qualified
          (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000006")))
          (Domain "golden2.example.com"),
      cmuAlreadyPresentUsers = [],
      cmuAction = ConversationMembersActionAdd ((qAlice, roleNameWireMember) :| [(qBob, roleNameWireAdmin)])
    }

testObject_ConversationMemberUpdate2 :: ConversationMemberUpdate
testObject_ConversationMemberUpdate2 =
  ConversationMemberUpdate
    { cmuTime = read "1864-04-12 12:22:43.673 UTC",
      cmuOrigUserId =
        Qualified
          (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000007")))
          (Domain "golden.example.com"),
      cmuConvId =
        Qualified
          (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000006")))
          (Domain "golden2.example.com"),
      cmuAlreadyPresentUsers = [chad, dee],
      cmuAction = ConversationMembersActionRemove (qAlice :| [qBob])
    }
