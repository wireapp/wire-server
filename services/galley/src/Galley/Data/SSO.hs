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
import Galley.Types.Teams.SSO
import Imports

-- | Return whether a given team is allowed to enable/disable sso
getSSOTeamConfig :: MonadClient m => TeamId -> m (Maybe SSOTeamConfig)
getSSOTeamConfig tid = fmap toSSOTeamConfig <$> do
  retry x1 $ query1 selectSSOTeamConfig (params Quorum (Identity tid))
  where
    toSSOTeamConfig (Identity Nothing) = SSOTeamConfig SSODisabled
    toSSOTeamConfig (Identity (Just status)) = SSOTeamConfig status

-- | Determines whether a given team is allowed to enable/disable sso
setSSOTeamConfig :: MonadClient m => TeamId -> SSOTeamConfig -> m ()
setSSOTeamConfig tid SSOTeamConfig {ssoTeamConfigStatus} = do
  retry x5 $ write updateSSOTeamConfig (params Quorum (ssoTeamConfigStatus, tid))
