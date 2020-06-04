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

module Galley.Data.SSO
  ( setSSOTeamConfig,
    getSSOTeamConfig,
  )
where

import Cassandra
import Data.Id
import Galley.Data.Instances ()
import Galley.Data.Queries
import Imports
import Wire.API.Team.Feature (TeamFeatureStatus (..))

-- | Return whether a given team is allowed to enable/disable sso.
-- Defaults to 'TeamFeatureDisabled' if null in the DB
getSSOTeamConfig :: MonadClient m => TeamId -> m (Maybe TeamFeatureStatus)
getSSOTeamConfig tid = fmap toSSOTeamConfig <$> do
  retry x1 $ query1 selectSSOTeamConfig (params Quorum (Identity tid))
  where
    toSSOTeamConfig (Identity Nothing) = TeamFeatureDisabled
    toSSOTeamConfig (Identity (Just status)) = status

-- | Determines whether a given team is allowed to enable/disable sso
setSSOTeamConfig :: MonadClient m => TeamId -> TeamFeatureStatus -> m ()
setSSOTeamConfig tid ssoTeamConfigStatus = do
  retry x5 $ write updateSSOTeamConfig (params Quorum (ssoTeamConfigStatus, tid))
