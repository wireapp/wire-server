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
  )
where

import Cassandra
import Data.Id
import Galley.Data.Instances ()
import Galley.Data.Queries
import Galley.Types.Teams.SearchVisibility
import Imports

-- | Return whether a given team is allowed to enable/disable sso
getSearchVisibilityTeamConfig :: MonadClient m => TeamId -> m (Maybe CustomSearchVisibilityTeamConfig)
getSearchVisibilityTeamConfig tid = fmap toSearchVisibilityTeamConfig <$> do
  retry x1 $ query1 selectCustomVisibilityTeamConfig (params Quorum (Identity tid))
  where
    toSearchVisibilityTeamConfig (Identity Nothing) = undefined -- SSOTeamConfig SSODisabled
    toSearchVisibilityTeamConfig (Identity (Just status)) = undefined -- SSOTeamConfig status

-- | Determines whether a given team is allowed to enable/disable sso
setSearchVisibilityTeamConfig :: MonadClient m => TeamId -> CustomSearchVisibilityTeamConfig -> m ()
setSearchVisibilityTeamConfig tid CustomSearchVisibilityTeamConfig {customSearchVisibilityTeamConfigStatus} = do
  retry x5 $ write updateCustomSearchVisibilityTeamConfig (params Quorum (customSearchVisibilityTeamConfigStatus, tid))
