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

module Test.Wire.API.Golden.Generated.TeamConversation_team where

import Data.Id (Id (Id))
import qualified Data.UUID as UUID (fromString)
import Imports
import Wire.API.Team.Conversation (TeamConversation, newTeamConversation)

testObject_TeamConversation_team_1 :: TeamConversation
testObject_TeamConversation_team_1 =
  newTeamConversation (Id (fromJust (UUID.fromString "00000054-0000-0032-0000-001d0000003e")))

testObject_TeamConversation_team_2 :: TeamConversation
testObject_TeamConversation_team_2 =
  newTeamConversation (Id (fromJust (UUID.fromString "00000021-0000-0059-0000-00390000004c")))
