{-# LANGUAGE TemplateHaskell #-}

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

module Wire.TeamSubsystem where

import Data.Id
import Data.Qualified
import Data.Range
import Imports
import Polysemy
import Wire.API.Team.Member
import Wire.API.Team.Member.Info (TeamMemberInfoList)

data TeamSubsystem m a where
  InternalGetTeamMember :: UserId -> TeamId -> TeamSubsystem m (Maybe TeamMember)
  InternalGetTeamMembers :: TeamId -> TeamSubsystem m [TeamMember]
  InternalGetTeamMembersWithLimit :: TeamId -> Maybe (Range 1 HardTruncationLimit Int32) -> TeamSubsystem m TeamMemberList
  InternalSelectTeamMembers :: TeamId -> [UserId] -> TeamSubsystem m [TeamMember]
  InternalSelectTeamMemberInfos :: TeamId -> [UserId] -> TeamSubsystem m TeamMemberInfoList
  InternalGetTeamAdmins :: TeamId -> TeamSubsystem m TeamMemberList
  InternalFinalizeDeleteTeam :: Local UserId -> Maybe ConnId -> TeamId -> TeamSubsystem m ()

makeSem ''TeamSubsystem
