{-# LANGUAGE ViewPatterns #-}

module Brig.Team.Util where  -- TODO: remove this module and move contents to Brig.IO.Intra?

import Imports
import Brig.App
import Control.Lens
import Data.Id
import Galley.Types.Teams
import Brig.API.Error
import Control.Error

import qualified Brig.IO.Intra as Intra
import qualified Data.Set as Set

data TeamOwnershipStatus = IsOnlyTeamOwner | IsOneOfManyTeamOwners | IsNotTeamOwner | NoTeamOwnersAreLeft
  deriving (Eq, Show, Bounded, Enum)

-- | A team owner is a team member with full permissions *and* an email address.
teamOwnershipStatus :: UserId -> TeamId -> AppIO TeamOwnershipStatus
teamOwnershipStatus uid tid = teamOwnershipStatus' uid . fmap (^. userId) <$> Intra.getTeamOwnersWithEmail tid

teamOwnershipStatus' :: UserId -> [UserId] -> TeamOwnershipStatus
teamOwnershipStatus' _ [] = NoTeamOwnersAreLeft
teamOwnershipStatus' uid (Set.fromList -> owners)
    | uid `Set.notMember` owners       = IsNotTeamOwner
    | Set.null (Set.delete uid owners) = IsOnlyTeamOwner
    | otherwise                        = IsOneOfManyTeamOwners

ensurePermissions :: UserId -> TeamId -> [Perm] -> ExceptT Error AppIO ()
ensurePermissions u t perms = do
    m <- lift $ Intra.getTeamMember u t
    unless (check m) $
        throwStd insufficientTeamPermissions
  where
    check :: Maybe TeamMember -> Bool
    check (Just m) = and $ hasPermission m <$> perms
    check Nothing  = False

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
        hasPermission inviter AddTeamMember &&
        and (mayGrantPermission inviter <$> Set.toList (inviteePerms ^. self))
    check Nothing  = False
