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

module Galley.API.Public.TeamMember where

import Galley.API.Teams
import Galley.API.Teams.Export qualified as Export
import Galley.App
import Wire.API.Routes.API
import Wire.API.Routes.Public.Galley.TeamMember

teamMemberAPI :: API TeamMemberAPI GalleyEffects
teamMemberAPI =
  mkNamedAPI @"get-team-members" getTeamMembers
    <@> mkNamedAPI @"get-team-member" getTeamMember
    <@> mkNamedAPI @"get-team-members-by-ids" bulkGetTeamMembers
    <@> mkNamedAPI @"add-team-member" addTeamMember
    <@> mkNamedAPI @"delete-team-member" deleteTeamMember
    <@> mkNamedAPI @"delete-non-binding-team-member" deleteNonBindingTeamMember
    <@> mkNamedAPI @"update-team-member" updateTeamMember
    <@> mkNamedAPI @"get-team-members-csv" Export.getTeamMembersCSV
