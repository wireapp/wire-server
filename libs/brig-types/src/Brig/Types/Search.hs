{-# LANGUAGE OverloadedStrings #-}
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

module Brig.Types.Search
  ( TeamSearchInfo (..),

    -- * re-exports
    SearchResult (..),
    Contact (..),
  )
where

import Data.Id (TeamId)
import Wire.API.User.Search

data TeamSearchInfo
  = -- | When searching user is not part of a team.
    NoTeam
  | -- | When searching user is part of a team and 'Brig.Options.setSearchSameTeamOnly' is True
    --   OR the searching user belongs to a team with SearchVisibilityNoNameOutsideTeam
    TeamOnly TeamId
  | -- | When searching user is part of a team and 'Brig.Options.setSearchSameTeamOnly' is False
    TeamAndNonMembers TeamId
