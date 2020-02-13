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
