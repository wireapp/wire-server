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

module Test.Migration.ClientStore (testClientStoreMigration) where

import API.Brig
import API.BrigCommon (AddClient (..))
import Control.Monad.Codensity
import Control.Monad.Reader
import SetupHelpers
import Test.Migration.Util (waitForMigration)
import Testlib.Prelude
import Testlib.ResourcePool

-- | Drives the brig client store through the full cutover lifecycle
-- (cassandra -> migration-to-postgresql -> postgresql). Clients registered
-- before the cutover must still be served by the pure-Postgres interpreter
-- afterwards, which proves both the background-worker backfill and the
-- Postgres read path; the prekey claim proves the prekeys table copy.
testClientStoreMigration :: (HasCallStack) => App ()
testClientStoreMigration = do
  resourcePool <- asks (.resourcePool)
  runCodensity (acquireResources 1 resourcePool) $ \[backend] -> do
    let domain = backend.berDomain

    -- P1 cassandra: register a client via the pure Cassandra interpreter
    (owner, c1) <-
      runCodensity (startDynamicBackend backend (conf "cassandra" False)) $ \_ -> do
        (owner, _, _) <- createTeam domain 1
        c1 <- addClient owner def >>= getJSON 201
        checkClient owner c1
        pure (owner, c1)

    -- P2 migration-to-postgresql (worker off): reads still come from
    -- Cassandra, writes go to both stores
    runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" False)) $ \_ -> do
      checkClient owner c1
      void $ addClient owner def {clabel = "c2-migrated"} >>= getJSON 201

    -- P3 migration-to-postgresql (worker on): dual-write while backfilling
    runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" True)) $ \_ -> do
      checkClient owner c1
      waitForMigration domain counterName
      void $ addClient owner def {clabel = "c3-dual-write"} >>= getJSON 201

    -- P4 postgresql: reads are served exclusively from Postgres
    runCodensity (startDynamicBackend backend (conf "postgresql" False)) $ \_ -> do
      checkClient owner c1
      -- clients registered before the cutover are served from Postgres
      selfClients <- getSelfClients owner >>= getJSON 200
      uid <- owner %. "id" & asString
      cids <- selfClients %. uid >>= asList
      length cids `shouldMatchInt` 3
      -- write via the pure Postgres interpreter and read it back
      c4 <- addClient owner def {clabel = "c4-postgres"} >>= getJSON 201
      checkClient owner c4
      -- claim a prekey from Postgres
      c4id <- c4 %. "id" & asString
      prekey <- getUsersPrekeysClient owner owner c4id >>= getJSON 200
      void $ prekey %. "prekey.id"
  where
    conf db runMigration =
      def
        { brigCfg = setField "postgresMigration.clients" db,
          backgroundWorkerCfg = setField "migrateClients" runMigration
        }
    counterName = "^wire_clients_migration_finished"

    checkClient owner client = do
      cid <- client %. "id" & asString
      resp <- getClient owner cid >>= getJSON 200
      cid' <- resp %. "id" & asString
      cid' `shouldMatch` cid
