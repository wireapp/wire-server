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

-- | Every team must have at least one owner with an email address for billing and
-- administration.  'TeamOwnershipStatus' distinguishes all the relevant cases.
data TeamOwnershipStatus
  = IsOnlyTeamOwnerWithEmail
  | IsOneOfManyTeamOwnersWithEmail
  | IsTeamOwnerWithoutEmail
  | IsNotTeamOwner
  deriving (Eq, Show, Bounded, Enum)

teamOwnershipStatus :: UserId -> TeamId -> AppIO TeamOwnershipStatus
teamOwnershipStatus uid tid = compute <$> Intra.getTeamOwnersWithEmail tid
  where
    compute :: [(TeamMember, Bool)] -> TeamOwnershipStatus
    compute owners = search (getuid <$> owners) (getuid <$> ownersWithEmail)
      where
        ownersWithEmail = filter (^. _2) owners
        getuid = (^. _1 . userId)
    search :: [UserId] -> [UserId] -> TeamOwnershipStatus
    search [] [] = IsNotTeamOwner -- this shouldn't happen, but we don't handle that here.
    search (Set.fromList -> owners) (Set.fromList -> ownersWithEmail) =
      case (uid `Set.member` owners, uid `Set.member` ownersWithEmail) of
        (False, _) -> IsNotTeamOwner
        (True, False) -> IsTeamOwnerWithoutEmail
        (True, True) ->
          if Set.null (Set.delete uid ownersWithEmail)
            then IsOnlyTeamOwnerWithEmail
            else IsOneOfManyTeamOwnersWithEmail

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
