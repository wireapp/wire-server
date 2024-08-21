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

module Brig.User.API.Search
  ( teamUserSearch,
    refreshIndex,
  )
where

import Brig.API.Handler
import Brig.Team.Util (ensurePermissions)
import Brig.User.Search.Index
import Brig.User.Search.TeamUserSearch qualified as Q
import Data.Id
import Data.Range
import Imports
import Polysemy
import Wire.API.Team.Permission qualified as Public
import Wire.API.User.Search
import Wire.API.User.Search qualified as Public
import Wire.GalleyAPIAccess (GalleyAPIAccess)

teamUserSearch ::
  (Member GalleyAPIAccess r) =>
  UserId ->
  TeamId ->
  Maybe Text ->
  Maybe RoleFilter ->
  Maybe TeamUserSearchSortBy ->
  Maybe TeamUserSearchSortOrder ->
  Maybe (Range 1 500 Int32) ->
  Maybe PagingState ->
  (Handler r) (Public.SearchResult Public.TeamContact)
teamUserSearch uid tid mQuery mRoleFilter mSortBy mSortOrder size mPagingState = do
  ensurePermissions uid tid [Public.AddTeamMember] -- limit this to team admins to reduce risk of involuntary DOS attacks.  (also, this way we don't need to worry about revealing confidential user data to other team members.)
  Q.teamUserSearch tid mQuery mRoleFilter mSortBy mSortOrder (fromMaybe (unsafeRange 15) size) mPagingState
