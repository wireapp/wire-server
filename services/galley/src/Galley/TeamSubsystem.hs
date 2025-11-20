-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Galley.TeamSubsystem where

import Imports
import Polysemy
import Wire.API.Team.HardTruncationLimit
import Wire.API.Team.Member
import Wire.API.Team.Member.Info (TeamMemberInfoList (TeamMemberInfoList))
import Wire.TeamStore (TeamStore)
import Wire.TeamStore qualified as E
import Wire.TeamSubsystem

-- This interpreter exists so galley code doesn't end up depending on
-- GalleyAPIAccess, while it is possible to implement that, it'd add unnecesary
-- HTTP calls for tiny things.
--
-- When we actually implement TeamSubsystem this can move to wire-subsystem.
-- Moving this to wire-subsystem before that would be too much work as the Store
-- effects in galley are not as thin as we're doing them in wire-subsystems.
-- They also depend on entire galley env.
interpretTeamSubsystem :: (Member TeamStore r) => InterpreterFor TeamSubsystem r
interpretTeamSubsystem = interpret $ \case
  InternalGetTeamMember uid tid -> E.getTeamMember tid uid
  InternalGetTeamMembers tid maxResults ->
    E.getTeamMembersWithLimit tid $ fromMaybe hardTruncationLimitRange maxResults
  InternalSelectTeamMemberInfos tid uids -> TeamMemberInfoList <$> E.selectTeamMemberInfos tid uids
  InternalGetTeamAdmins tid -> do
    admins <- E.getTeamAdmins tid
    membs <- E.selectTeamMembers tid admins
    pure $ newTeamMemberList membs ListComplete
