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

module Wire.TeamFeatureStore.Postgres.Queries where

import Data.Id
import Data.Vector (Vector)
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Wire.API.PostgresMarshall
import Wire.API.Team.Feature

select :: Hasql.Statement (TeamId, Text) (Maybe (Maybe FeatureStatus, Maybe LockStatus, Maybe DbConfig))
select =
  dimapPG
    [maybeStatement|SELECT 
                      status :: int?,
                      lock_status :: int?,
                      config :: jsonb?
                    FROM team_features
                    WHERE team = ($1 :: uuid) AND feature = ($2 :: text)
                    |]

exists :: Hasql.Statement (TeamId, Text) Bool
exists =
  dimapPG
    [singletonStatement|SELECT EXISTS (
                           SELECT 1
                           FROM team_features
                           WHERE team = ($1 :: uuid) AND feature = ($2 :: text)
                         ) :: bool|]

upsertPatch :: Hasql.Statement (TeamId, Text, Maybe FeatureStatus, Maybe LockStatus, Maybe DbConfig) ()
upsertPatch =
  lmapPG
    [resultlessStatement|INSERT INTO team_features (team, feature, status, lock_status, config)
                         VALUES ($1 :: uuid, $2 :: text, $3 :: int?, $4 :: int?, $5 :: jsonb?)
                         ON CONFLICT (team, feature) DO UPDATE
                         SET status = COALESCE(EXCLUDED.status, team_features.status),
                             lock_status = COALESCE(EXCLUDED.lock_status, team_features.lock_status),
                             config = COALESCE(EXCLUDED.config, team_features.config)
                        |]

writeLockStatus :: Hasql.Statement (TeamId, Text, LockStatus) ()
writeLockStatus =
  lmapPG
    [resultlessStatement|INSERT INTO team_features (team, feature, lock_status)
                         VALUES ($1 :: uuid, $2 :: text, $3 :: int)
                         ON CONFLICT (team, feature) DO UPDATE
                         SET lock_status = EXCLUDED.lock_status
                        |]

selectAll :: Hasql.Statement TeamId (Vector (Text, Maybe FeatureStatus, Maybe LockStatus, Maybe DbConfig))
selectAll =
  dimapPG
    [vectorStatement|SELECT (feature :: text),
                             (status :: int?),
                             (lock_status :: int?),
                             (config :: jsonb?)
                      FROM team_features
                      WHERE team = ($1 :: uuid)
                    |]
