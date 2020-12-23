{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

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

module Brig.User.Search.BrowseTeam
  ( browseTeam,
    browseTeamQuery,
  )
where

import Brig.Data.Instances ()
import Brig.Types.Search
import Brig.User.Search.Index
import Data.Id (TeamId)
import Data.Range (Range (..))
import qualified Database.Bloodhound as ES
import Imports hiding (log, searchable)
import Wire.API.Team.Role
import Wire.API.User.Search

browseTeam ::
  (HasCallStack, MonadIndexIO m) =>
  TeamId ->
  Maybe Text ->
  Maybe [Role] ->
  Maybe Text ->
  Maybe Text ->
  Range 1 500 Int32 ->
  m (SearchResult TeamContact)
browseTeam _tid _mQuery Nothing Nothing Nothing _mSize = do
  undefined
browseTeam _ _ _ _ _ _ = error "not implemented."

-- | Pure query constructor.
--
-- TODO: Maybe (sortby=<name|handle|email|saml_idp|managed_by|role|created_at>, Maybe ES.SortOrder)
-- TODO: Maybe [Role]
browseTeamQuery ::
  TeamId ->
  Maybe Text ->
  Range 1 500 Int32 ->
  ES.Query
browseTeamQuery _team _searchText _mSize = undefined
