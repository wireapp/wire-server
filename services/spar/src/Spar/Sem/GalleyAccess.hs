{-# LANGUAGE TemplateHaskell #-}

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

module Spar.Sem.GalleyAccess -- TODO: use GalleyAPIAccess from wire-subsystems instead.
  ( GalleyAccess (..),
    getTeamMembers,
    getTeamMember,
    assertHasPermission,
    assertSSOEnabled,
    isEmailValidationEnabledTeam,
    updateTeamMember,
  )
where

import Data.Id (TeamId, UserId)
import Imports
import Polysemy
import Wire.API.Team.Member
import Wire.API.Team.Role

data GalleyAccess m a where
  GetTeamMembers :: TeamId -> GalleyAccess m [TeamMember]
  GetTeamMember :: TeamId -> UserId -> GalleyAccess m (Maybe TeamMember)
  AssertHasPermission :: (Show perm, IsPerm TeamMember perm) => TeamId -> perm -> UserId -> GalleyAccess m ()
  AssertSSOEnabled :: TeamId -> GalleyAccess m ()
  IsEmailValidationEnabledTeam :: TeamId -> GalleyAccess m Bool
  UpdateTeamMember :: UserId -> TeamId -> Role -> GalleyAccess m ()

makeSem ''GalleyAccess
