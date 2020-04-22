-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Brig.Team.Util where -- TODO: remove this module and move contents to Brig.IO.Intra?

import Brig.API.Error
import Brig.App
import qualified Brig.IO.Intra as Intra
import Control.Error
import Control.Lens
import Data.Id
import qualified Data.Set as Set
import Galley.Types.Teams
import Imports

ensurePermissions :: UserId -> TeamId -> [Perm] -> ExceptT Error AppIO ()
ensurePermissions u t perms = do
  m <- lift $ Intra.getTeamMember u t
  unless (check m) $
    throwStd insufficientTeamPermissions
  where
    check :: Maybe TeamMember -> Bool
    check (Just m) = and $ hasPermission m <$> perms
    check Nothing = False

-- | Privilege escalation detection (make sure no `RoleMember` user creates a `RoleOwner`).
--
-- There is some code duplication with 'Galley.API.Teams.ensureNotElevated'.
ensurePermissionToAddUser :: UserId -> TeamId -> Permissions -> ExceptT Error AppIO ()
ensurePermissionToAddUser u t inviteePerms = do
  minviter <- lift $ Intra.getTeamMember u t
  unless (check minviter) $
    throwStd insufficientTeamPermissions
  where
    check :: Maybe TeamMember -> Bool
    check (Just inviter) =
      hasPermission inviter AddTeamMember
        && and (mayGrantPermission inviter <$> Set.toList (inviteePerms ^. self))
    check Nothing = False
