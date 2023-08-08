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

module Brig.Team.Util where -- TODO: remove this module and move contents to Brig.IO.Intra?

import Brig.API.Error
import Brig.App
import Brig.Data.User qualified as Data
import Brig.Effects.GalleyProvider (GalleyProvider)
import Brig.Effects.GalleyProvider qualified as GalleyProvider
import Brig.Types.User (HavePendingInvitations (NoPendingInvitations))
import Control.Error
import Control.Lens
import Data.Id
import Data.Set qualified as Set
import Galley.Types.Teams
import Imports
import Polysemy (Member)
import Wire.API.Team.Member
import Wire.API.Team.Permission
import Wire.API.User (User (userTeam))

-- | If the user is in a team, it has to have these permissions.  If not, it is a personal
-- user with account validation and thus given the permission implicitly.  (Used for
-- `SearchContactcs`.)
ensurePermissionsOrPersonalUser :: (Member GalleyProvider r, IsPerm perm) => UserId -> [perm] -> ExceptT Error (AppT r) ()
ensurePermissionsOrPersonalUser u perms = do
  mbUser <- lift $ wrapHttp $ Data.lookupUser NoPendingInvitations u
  maybe (pure ()) (\tid -> ensurePermissions u tid perms) (userTeam =<< mbUser :: Maybe TeamId)

ensurePermissions :: (Member GalleyProvider r, IsPerm perm) => UserId -> TeamId -> [perm] -> ExceptT Error (AppT r) ()
ensurePermissions u t perms = do
  m <- lift $ liftSem $ GalleyProvider.getTeamMember u t
  unless (check m) $
    throwStd insufficientTeamPermissions
  where
    check :: Maybe TeamMember -> Bool
    check (Just m) = and $ hasPermission m <$> perms
    check Nothing = False

-- | Privilege escalation detection (make sure no `RoleMember` user creates a `RoleOwner`).
--
-- There is some code duplication with 'Galley.API.Teams.ensureNotElevated'.
ensurePermissionToAddUser :: Member GalleyProvider r => UserId -> TeamId -> Permissions -> ExceptT Error (AppT r) ()
ensurePermissionToAddUser u t inviteePerms = do
  minviter <- lift $ liftSem $ GalleyProvider.getTeamMember u t
  unless (check minviter) $
    throwStd insufficientTeamPermissions
  where
    check :: Maybe TeamMember -> Bool
    check (Just inviter) =
      hasPermission inviter AddTeamMember
        && and (mayGrantPermission inviter <$> Set.toList (inviteePerms ^. self))
    check Nothing = False
