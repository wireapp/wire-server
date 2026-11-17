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

module Wire.LegalHoldStore.Postgres
  ( interpretLegalHoldStoreWithPostgresTeamMembers,
  )
where

import Cassandra (ClientState)
import Imports
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog
import Wire.API.Team.Feature (LegalholdConfig)
import Wire.API.Team.FeatureFlags (FeatureDefaults (..))
import Wire.LegalHoldStore (LegalHoldStore (..))
import Wire.LegalHoldStore.Cassandra (legalHoldStoreToCassandra)
import Wire.LegalHoldStore.Env (LegalHoldEnv (..))
import Wire.Postgres
import Wire.TeamStore.Postgres (setUserLegalHoldStatusPg)
import Wire.Util (logEffect)

-- | 'LegalHoldStore' for deployments whose team members are stored in
-- Postgres: the per-member legal hold status lives in the Postgres
-- @team_member@ table, everything else (settings, pending prekeys, the
-- team whitelist) is still stored in Cassandra.
interpretLegalHoldStoreWithPostgresTeamMembers ::
  ( PGConstraints r,
    Member (Input ClientState) r,
    Member (Input LegalHoldEnv) r,
    Member TinyLog r
  ) =>
  FeatureDefaults LegalholdConfig ->
  Sem (LegalHoldStore ': r) a ->
  Sem r a
interpretLegalHoldStoreWithPostgresTeamMembers lh = interpret $ \case
  SetUserLegalHoldStatus tid uid st -> do
    logEffect "LegalHoldStore.SetUserLegalHoldStatus"
    setUserLegalHoldStatusPg tid uid st
  other -> legalHoldStoreToCassandra lh other
