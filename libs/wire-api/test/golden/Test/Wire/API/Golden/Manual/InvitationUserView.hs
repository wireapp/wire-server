-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2024 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Wire.API.Golden.Manual.InvitationUserView where

import Data.Id (Id (Id))
import Data.Json.Util (readUTCTimeMillis)
import Data.UUID qualified as UUID (fromString)
import Imports
import Wire.API.Team.Invitation
import Wire.API.Team.Role
import Wire.API.User.Identity
import Wire.API.User.Profile (Name (Name, fromName))

testObject_InvitationUserView_team_1 :: InvitationUserView
testObject_InvitationUserView_team_1 =
  InvitationUserView
    { invitation =
        Invitation
          { team = Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000002")),
            role = RoleAdmin,
            invitationId = Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000000")),
            createdAt = fromJust (readUTCTimeMillis "1864-05-11T20:13:15.856Z"),
            createdBy = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000001"))),
            inviteeEmail = unsafeEmailAddress "some" "example",
            inviteeName = Nothing,
            inviteeUrl = Nothing
          },
      inviterEmail = Just $ unsafeEmailAddress "some" "example"
    }

testObject_InvitationUserView_team_2 :: InvitationUserView
testObject_InvitationUserView_team_2 =
  InvitationUserView
    { invitation =
        Invitation
          { team = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")),
            role = RoleExternalPartner,
            invitationId = Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000002")),
            createdAt = fromJust (readUTCTimeMillis "1864-05-12T14:47:35.551Z"),
            createdBy = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000001"))),
            inviteeEmail = unsafeEmailAddress "some" "example",
            inviteeName = Just (Name {fromName = "\1067847} 2pGEW+\rT\171609p\174643\157218&\146145v0\b"}),
            inviteeUrl = Nothing
          },
      inviterEmail = Nothing
    }
