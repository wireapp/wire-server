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

module Test.Migration.TeamStore (testTeamStoreMigration) where

import API.Galley
import Control.Monad.Codensity
import Control.Monad.Reader
import SetupHelpers (createTeam)
import Test.Migration.Util (waitForMigration)
import Testlib.Prelude
import Testlib.ResourcePool

-- | Migrate the team store (teams, team members, user_team, billing team
-- members, team admins) from Cassandra to PostgreSQL.
--
-- Phase 1: Cassandra only — a team with members is created while Cassandra is
-- the source of truth.
-- Phase 2: dual-write on, copy worker off — writes are mirrored to Postgres.
-- Phase 3: dual-write on, copy worker on — the five tables are backfilled.
-- Phase 4: worker off again after the migration ran — steady state.
-- Phase 5: PostgreSQL only — the team and its members are served from
-- PostgreSQL and membership writes keep working.
testTeamStoreMigration :: (HasCallStack) => App ()
testTeamStoreMigration = do
  resourcePool <- asks (.resourcePool)
  runCodensity (acquireResources 1 resourcePool) $ \[backend] -> do
    let domain = backend.berDomain

    -- phase 1: Cassandra only: create a team with members.
    (owner, tid, members) <- runCodensity (startDynamicBackend backend (phase1 backend)) $ \_ -> do
      (owner, tid, members) <- createTeam domain 4
      pure (owner, tid, members)

    -- phase 2: dual-write on, worker off: membership writes are mirrored to
    -- Postgres while reads remain on Cassandra.
    runCodensity (startDynamicBackend backend (phase2 backend)) $ \_ -> do
      checkTeam owner tid members

    -- phase 3: worker on: the copy loop backfills the tables, including
    -- rows that predate the migration setting.
    runCodensity (startDynamicBackend backend (phase3 backend)) $ \_ -> do
      waitForMigration domain counterName
      checkTeam owner tid members

    -- phase 4: worker off again after the migration ran: steady state.
    runCodensity (startDynamicBackend backend (phase4 backend)) $ \_ -> do
      checkTeam owner tid members

    -- phase 5: PostgreSQL only: the team and its members must be served from
    -- PostgreSQL, and team writes must work.
    runCodensity (startDynamicBackend backend (phase5 backend)) $ \_ -> do
      checkTeam owner tid members
      -- team creation goes through the Postgres-backed store
      (owner2, tid2, members2) <- createTeam domain 2
      checkTeam owner2 tid2 members2
  where
    counterName :: String
    counterName = "^wire_team_members_migration_finished"

    checkTeam :: (HasCallStack, MakesValue owner, MakesValue tid, MakesValue m) => owner -> tid -> [m] -> App ()
    checkTeam owner tid members = do
      membersResponse <- getTeamMembers owner tid >>= getJSON 200 >>= asList
      assertBool "expected all team members to survive the migration" (length membersResponse == length members)

    conf :: String -> Bool -> ServiceOverrides
    conf db runMigration =
      def
        { galleyCfg =
            setField "postgresMigration.teamStore" db,
          backgroundWorkerCfg =
            setField "postgresMigration.teamStore" db
              >=> setField "migrateTeamMembers" runMigration
        }

    phase1, phase2, phase3, phase4, phase5 :: (MakesValue backend) => backend -> ServiceOverrides
    phase1 backend = conf backend "cassandra" False
    phase2 backend = conf backend "migration-to-postgresql" False
    phase3 backend = conf backend "migration-to-postgresql" True
    phase4 backend = conf backend "migration-to-postgresql" False
    phase5 backend = conf backend "postgresql" False
