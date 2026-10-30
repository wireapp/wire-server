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

module Test.Migration.Connections (testConnectionsMigrationToPostgres) where

import API.Brig (getConnection, postConnection, putConnection)
import Control.Monad.Codensity
import Control.Monad.Reader
import GHC.Stack
import SetupHelpers
import Test.Migration.Util (assertMigrationSuccessful, waitForMigration)
import Testlib.Prelude
import Testlib.ResourcePool

-- | Drives the connections store through the full cutover lifecycle
-- (cassandra -> migration-to-postgresql -> postgresql) on a dynamic backend.
-- Every phase creates a fresh pair of users and connects them via the brig
-- public API, exercising the writes of all three interpreters (Cassandra,
-- dual-write, Postgres). Connections written by earlier interpreters must
-- remain readable afterwards: the pair connected during the pure-Cassandra
-- phase proves the background worker backfilled rows that only ever existed
-- in Cassandra.
--
-- Remote ('connection_remote') rows are not seeded explicitly here; the
-- migration's copy loop still runs over whatever dual-write produced, and
-- federated connections are covered by the brig integration suite.
testConnectionsMigrationToPostgres :: (HasCallStack) => App ()
testConnectionsMigrationToPostgres = do
  resourcePool <- asks (.resourcePool)
  runCodensity (acquireResources 1 resourcePool) $ \[backend] -> do
    let domain = backend.berDomain

    -- P1 cassandra: connect a pair via the pure Cassandra interpreter
    cassandraPair <-
      runCodensity (startDynamicBackend backend (conf "cassandra" False)) $ \_ ->
        connectFreshPair domain

    -- P2 migration-to-postgresql (worker off): reads still come from
    -- Cassandra, writes go to both stores
    dualWritePair <-
      runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" False)) $ \_ -> do
        checkConnection cassandraPair
        connectFreshPair domain

    -- P3 migration-to-postgresql (worker on): dual-write while backfilling
    workerOnPair <-
      runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" True)) $ \_ -> do
        checkConnection dualWritePair
        pair <- connectFreshPair domain
        waitForMigration domain connectionsMigrationFinishedCounterName
        -- The failed counter is only registered while the migration worker
        -- runs, so assert it in this phase (worker on), not in P4.
        assertMigrationSuccessful domain "^wire_connections_migration_failed"
        pure pair

    -- P4 postgresql: reads are served exclusively from Postgres
    runCodensity (startDynamicBackend backend (conf "postgresql" False)) $ \_ -> do
      -- Connections written by every interpreter are visible to the pure
      -- Postgres interpreter.
      checkConnection cassandraPair
      checkConnection dualWritePair
      checkConnection workerOnPair

      -- Write and read via the pure Postgres interpreter.
      pgPair <- connectFreshPair domain
      checkConnection pgPair
  where
    conf db runMigration =
      def
        { brigCfg = setField "postgresMigration.connections" db,
          backgroundWorkerCfg =
            setField "postgresMigration.connections" db
              >=> setField "migrateConnections" runMigration
        }

connectionsMigrationFinishedCounterName :: String
connectionsMigrationFinishedCounterName = "^wire_connections_migration_finished"

-- | Create two fresh users on the given domain and connect them (request +
-- accept) via the brig public connection API.
connectFreshPair :: (HasCallStack, MakesValue domain) => domain -> App (Value, Value)
connectFreshPair domain = do
  [u1, u2] <- createUsers [domain, domain]
  void $ postConnection u1 u2 >>= assertSuccess
  void $ putConnection u2 u1 "accepted" >>= assertSuccess
  pure (u1, u2)

-- | The connection between the two users is visible with status 'accepted'.
checkConnection :: (HasCallStack) => (Value, Value) -> App ()
checkConnection (u1, u2) =
  getConnection u1 u2 `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "status" `shouldMatch` "accepted"
