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

module Wire.TeamSubsystem.GalleyAPI where

import Imports
import Polysemy
import Wire.GalleyAPIAccess (GalleyAPIAccess)
import Wire.GalleyAPIAccess qualified as GalleyAPIAccess
import Wire.TeamSubsystem

interpretTeamSubsystemToGalleyAPI :: (Member GalleyAPIAccess r) => InterpreterFor TeamSubsystem r
interpretTeamSubsystemToGalleyAPI = interpret $ \case
  InternalGetTeamMember userId teamId -> GalleyAPIAccess.getTeamMember userId teamId
  InternalGetTeamMembersWithLimit teamId maxResults -> GalleyAPIAccess.getTeamMembersWithLimit teamId maxResults
  InternalSelectTeamMemberInfos teamId userIds -> GalleyAPIAccess.selectTeamMemberInfos teamId userIds
  InternalSelectTeamMembers teamId userIds -> GalleyAPIAccess.selectTeamMembers teamId userIds
  InternalGetTeamAdmins teamId -> GalleyAPIAccess.getTeamAdmins teamId
  InternalFinalizeDeleteTeam lusr mcon teamId -> GalleyAPIAccess.finalizeDeleteTeam lusr mcon teamId
