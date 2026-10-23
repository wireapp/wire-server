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

module Test.Migration.UserClientIndex (testUserClientIndexMigration) where

import API.GalleyInternal
import Control.Monad.Codensity
import Control.Monad.Reader
import SetupHelpers
import Test.Migration.Util (waitForMigration)
import Testlib.Prelude
import Testlib.ResourcePool

-- | Drives the galley user-client index store through the full cutover
-- lifecycle (cassandra -> migration-to-postgresql -> postgresql). Clients
-- added before the cutover must still be served by the pure-Postgres
-- interpreter afterwards, which proves the background-worker backfill and the
-- Postgres read path.
testUserClientIndexMigration :: (HasCallStack) => App ()
testUserClientIndexMigration = do
  resourcePool <- asks (.resourcePool)
  runCodensity (acquireResources 1 resourcePool) $ \[backend] -> do
    let domain = backend.berDomain

    -- P1 cassandra: create a client entry via the pure Cassandra interpreter
    (owner, cid1) <-
      runCodensity (startDynamicBackend backend (conf "cassandra" False)) $ \_ -> do
        (owner, _, _) <- createTeam domain 1
        let cid1 = newClientId 1
        testAddClient owner cid1 >>= assertStatus 200
        checkClients owner [cid1]
        pure (owner, cid1)

    -- P2 migration-to-postgresql (worker off): reads still come from
    -- Cassandra, writes go to both stores
    runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" False)) $ \_ -> do
      checkClients owner [cid1]
      let cid2 = newClientId 2
      testAddClient owner cid2 >>= assertStatus 200
      checkClients owner [cid1, cid2]

    -- P3 migration-to-postgresql (worker on): dual-write while backfilling
    runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" True)) $ \_ -> do
      checkClients owner [newClientId 1, newClientId 2]
      waitForMigration domain counterName
      let cid3 = newClientId 3
      testAddClient owner cid3 >>= assertStatus 200

    -- P4 postgresql: reads are served exclusively from Postgres
    runCodensity (startDynamicBackend backend (conf "postgresql" False)) $ \_ -> do
      -- entries created before the cutover are served from Postgres
      checkClients owner [newClientId 1, newClientId 2, newClientId 3]
      let cid4 = newClientId 4
      testAddClient owner cid4 >>= assertStatus 200
      checkClients owner [newClientId 1, newClientId 2, newClientId 3, cid4]
  where
    conf db runMigration =
      def
        { galleyCfg = setField "postgresMigration.userClientIndex" db,
          backgroundWorkerCfg = setField "migrateUserClientIndex" runMigration
        }
    counterName = "^wire_user_client_index_migration_finished"

    -- \| Client ids are hex numerals of a Word64; fixed distinct hex strings are
    -- valid client ids for the internal test endpoints.
    newClientId :: Int -> String
    newClientId n = "deadbeef" <> show n

    checkClients owner expected = do
      cids <- testGetClients owner >>= getJSON 200 >>= asList
      cids' <- mapM asString cids
      sort cids' `shouldMatch` sort expected
