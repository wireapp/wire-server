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

module Test.Teams where

import API.Brig
import API.BrigInternal (createUser, getInvitationCode)
import SetupHelpers
import Testlib.JSON
import Testlib.Prelude

testInvitePersonalUserToTeam :: (HasCallStack) => App ()
testInvitePersonalUserToTeam = do
  (owner, tid, _) <- createTeam OwnDomain 0
  personalUser <- createUser OwnDomain def >>= getJSON 201
  email <- personalUser %. "email" >>= asString
  inv <- postInvitation owner (PostInvitation $ Just email) >>= getJSON 201
  resp <- getInvitationCode owner inv >>= getJSON 200
  code <- resp %. "code" & asString
  acceptTeamInvitation personalUser code >>= assertSuccess
  user <- getSelf personalUser >>= getJSON 200
  user %. "team" `shouldMatch` tid
