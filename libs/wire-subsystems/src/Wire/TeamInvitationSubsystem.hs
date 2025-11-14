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

module Wire.TeamInvitationSubsystem where

import Data.Id
import Data.Qualified
import Imports
import Polysemy
import Wire.API.Team.Invitation
import Wire.API.Team.Role
import Wire.API.User (InvitationCode)
import Wire.API.User.EmailAddress

data TeamInvitationSubsystem m a where
  InviteUser :: Local UserId -> TeamId -> InvitationRequest -> TeamInvitationSubsystem m (Invitation, InvitationLocation)
  -- | This function exists to support migration in this subsystem, after the
  -- migration this would just be an internal detail of the subsystem
  InternalCreateInvitation :: TeamId -> Maybe InvitationId -> Role -> Local (Maybe UserId) -> EmailAddress -> InvitationRequest -> TeamInvitationSubsystem m (Invitation, InvitationCode)

makeSem ''TeamInvitationSubsystem
