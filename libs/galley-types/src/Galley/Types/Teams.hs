{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

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

module Galley.Types.Teams
  ( GetFeatureDefaults (..),
    module Wire.API.Team.Feature.Defaults,
    notTeamMember,
    findTeamMember,
    isTeamMember,
    isTeamOwner,
    canSeePermsOf,
  )
where

import Control.Lens (view)
import Data.Id (UserId)
import Data.Set qualified as Set
import Imports
import Wire.API.Team.Feature.Defaults
import Wire.API.Team.Member
import Wire.API.Team.Permission

notTeamMember :: [UserId] -> [TeamMember] -> [UserId]
notTeamMember uids tmms =
  Set.toList $
    Set.fromList uids `Set.difference` Set.fromList (map (view userId) tmms)

isTeamMember :: (Foldable m) => UserId -> m TeamMember -> Bool
isTeamMember u = isJust . findTeamMember u

findTeamMember :: (Foldable m) => UserId -> m TeamMember -> Maybe TeamMember
findTeamMember u = find ((u ==) . view userId)

isTeamOwner :: TeamMemberOptPerms -> Bool
isTeamOwner tm = optionalPermissions tm == Just fullPermissions

canSeePermsOf :: TeamMember -> TeamMember -> Bool
canSeePermsOf seeer seeee =
  seeer `hasPermission` GetMemberPermissions || seeer == seeee
