{-# LANGUAGE StrictData #-}

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

module Brig.Types.Search
  ( TeamSearchInfo (..),
  )
where

import Data.Id (TeamId)

-- | Outbound search restrictions configured by team admin of the searcher. This
-- value restricts the set of user that are searched.
--
-- See 'optionallySearchWithinTeam' for the effect on full-text search.
--
-- See 'mkTeamSearchInfo' for the business logic that defines the TeamSearchInfo
-- value.
--
-- Search results might be affected by the inbound search restriction settings of
-- the searched user. ('SearchVisibilityInbound')
data TeamSearchInfo
  = -- | Only users that are not part of any team are searched
    NoTeam
  | -- | Only users from the same team as the searcher are searched
    TeamOnly TeamId
  | -- | No search restrictions, all users are searched
    AllUsers
