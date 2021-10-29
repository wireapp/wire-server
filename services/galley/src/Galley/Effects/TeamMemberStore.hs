-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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

module Galley.Effects.TeamMemberStore
  ( -- * Team member store effect
    TeamMemberStore (..),

    -- * Team member pagination
    listTeamMembers,
    listTeamMemberIds,
  )
where

import Data.Id
import Data.Range
import Galley.Effects.Paging
import Galley.Types.Teams
import Imports
import Polysemy

data TeamMemberStore p limit m a where
  ListTeamMembers ::
    TeamId ->
    PagingState p UserId ->
    Range 1 limit Int32 ->
    TeamMemberStore p limit m (Page p TeamMember)
  ListTeamMemberIds ::
    TeamId ->
    PagingState p UserId ->
    Range 1 limit Int32 ->
    TeamMemberStore p limit m (Page p UserId)

makeSem ''TeamMemberStore
