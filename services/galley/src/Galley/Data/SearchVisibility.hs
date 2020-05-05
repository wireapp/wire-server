{-# LANGUAGE ViewPatterns #-}

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

module Galley.Data.SearchVisibility
  ( setSearchVisibilityTeamConfig,
    getSearchVisibilityTeamConfig,
    setSearchVisibility,
    getSearchVisibility,
    resetSearchVisibility,
  )
where

import Cassandra
import Data.Id
import Galley.Data.Instances ()
import Galley.Data.Queries
import Galley.Types.Teams.SearchVisibility
import Imports

-- | Return whether a given team is allowed to enable/disable sso
getSearchVisibilityTeamConfig :: MonadClient m => TeamId -> m (Maybe TeamSearchVisibilityEnabledView)
getSearchVisibilityTeamConfig tid = join . fmap toSearchVisibilityTeamConfig <$> do
  retry x1 $ query1 selectTeamSearchVisibilityEnabledView (params Quorum (Identity tid))
  where
    toSearchVisibilityTeamConfig (Identity Nothing) = Nothing
    toSearchVisibilityTeamConfig (Identity (Just status)) = Just $ TeamSearchVisibilityEnabledView status

-- | Determines whether a given team is allowed to enable/disable sso
setSearchVisibilityTeamConfig :: MonadClient m => TeamId -> TeamSearchVisibilityEnabledView -> m ()
setSearchVisibilityTeamConfig tid (TeamSearchVisibilityEnabledView isenabled) = do
  retry x5 $ write updateTeamSearchVisibilityEnabledView (params Quorum (isenabled, tid))

-- | Return whether a given team is allowed to enable/disable sso
getSearchVisibility :: MonadClient m => TeamId -> m TeamSearchVisibilityView
getSearchVisibility tid = toSearchVisibility <$> do
  retry x1 $ query1 selectSearchVisibility (params Quorum (Identity tid))
  where
    -- The value is either set or we return the default
    toSearchVisibility :: (Maybe (Identity (Maybe TeamSearchVisibility))) -> TeamSearchVisibilityView
    toSearchVisibility (Just (Identity (Just status))) = TeamSearchVisibilityView status
    toSearchVisibility _ = TeamSearchVisibilityView SearchVisibilityStandard

-- | Determines whether a given team is allowed to enable/disable sso
setSearchVisibility :: MonadClient m => TeamId -> TeamSearchVisibilityView -> m ()
setSearchVisibility tid (TeamSearchVisibilityView visibilityType) = do
  retry x5 $ write updateSearchVisibility (params Quorum (visibilityType, tid))

resetSearchVisibility :: MonadClient m => TeamId -> m ()
resetSearchVisibility tid = do
  retry x5 $ write updateSearchVisibility (params Quorum (SearchVisibilityStandard, tid))
