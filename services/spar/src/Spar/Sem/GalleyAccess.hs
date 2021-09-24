module Spar.Sem.GalleyAccess where

import Data.Id (TeamId, UserId)
import Galley.Types.Teams (IsPerm, TeamMember)
import Imports
import Polysemy

data GalleyAccess m a where
  GetTeamMembers :: TeamId -> GalleyAccess m [TeamMember]
  AssertHasPermission :: (Show perm, IsPerm perm) => TeamId -> perm -> UserId -> GalleyAccess m ()
  AssertSSOEnabled :: TeamId -> GalleyAccess m ()
  IsEmailValidationEnabledTeam :: TeamId -> GalleyAccess m Bool

makeSem ''GalleyAccess
