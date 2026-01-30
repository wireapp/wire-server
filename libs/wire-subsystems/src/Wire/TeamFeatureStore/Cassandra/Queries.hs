

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.TeamFeatureStore.Cassandra.Queries where

import Cassandra
import Data.Id
import Imports
import Wire.API.Team.Feature

select :: PrepQuery R (TeamId, Text) (Maybe FeatureStatus, Maybe LockStatus, Maybe DbConfig)
select = "select status, lock_status, config from team_features_dyn where team = ? and feature = ?"

writeStatus :: PrepQuery W (FeatureStatus, TeamId, Text) ()
writeStatus = "update team_features_dyn set status = ? where team = ? and feature = ?"

writeLockStatus :: PrepQuery W (LockStatus, TeamId, Text) ()
writeLockStatus = "update team_features_dyn set lock_status = ? where team = ? and feature = ?"

writeConfig :: PrepQuery W (DbConfig, TeamId, Text) ()
writeConfig = "update team_features_dyn set config = ? where team = ? and feature = ?"

selectAllByTeam :: PrepQuery R (Identity TeamId) (Text, Maybe FeatureStatus, Maybe LockStatus, Maybe DbConfig)
selectAllByTeam = "select feature, status, lock_status, config from team_features_dyn where team = ?"

selectAll :: PrepQuery R () (TeamId, Text, Maybe FeatureStatus, Maybe LockStatus, Maybe DbConfig)
selectAll = "select team, feature, status, lock_status, config from team_features_dyn"
