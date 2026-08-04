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

module Test.Migration.MLSCommitLock where

import Control.Monad.Codensity
import Control.Monad.Reader
import MLS.Util
import SetupHelpers
import Test.Migration.Util (waitForMigration)
import Testlib.Prelude
import Testlib.ResourcePool

-- | Verifies the MLS commit-lock store migration end to end. Every MLS commit
-- acquires and releases the commit lock, so driving commits through the three
-- storage locations exercises the lock in Cassandra, the dual-write mirror, and
-- Postgres-only.
testMLSCommitLockMigration :: (HasCallStack) => App ()
testMLSCommitLockMigration = do
  resourcePool <- asks (.resourcePool)
  runCodensity (acquireResources 1 resourcePool) $ \[backend] -> do
    let domain = backend.berDomain

    -- Cassandra: create an MLS group and commit once. This acquires and
    -- releases the commit lock against Cassandra.
    (alice1, convId) <- runCodensity (startDynamicBackend backend (conf "cassandra" False)) $ \_ -> do
      alice <- randomUser domain def
      alice1 <- createMLSClient def alice
      bob <- randomUser domain def
      bob1 <- createMLSClient def bob
      void $ uploadNewKeyPackage def bob1
      convId <- createNewGroup def alice1
      void $ createAddCommit alice1 convId [bob] >>= sendAndConsumeCommitBundle
      pure (alice1, convId)

    -- Dual-write + backfill: a commit is mirrored to Postgres, and the worker
    -- copies any live locks until it reports completion.
    runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" True)) $ \_ -> do
      charlie <- randomUser domain def
      charlie1 <- createMLSClient def charlie
      void $ uploadNewKeyPackage def charlie1
      void $ createAddCommit alice1 convId [charlie] >>= sendAndConsumeCommitBundle
      waitForMigration domain counterName

    -- Postgres-only: a commit acquires and releases the lock against Postgres.
    runCodensity (startDynamicBackend backend (conf "postgresql" False)) $ \_ -> do
      dave <- randomUser domain def
      dave1 <- createMLSClient def dave
      void $ uploadNewKeyPackage def dave1
      void $ createAddCommit alice1 convId [dave] >>= sendAndConsumeCommitBundle

conf :: String -> Bool -> ServiceOverrides
conf db runMigration =
  def
    { galleyCfg = setField "postgresMigration.mlsCommitLocks" db,
      backgroundWorkerCfg = setField "migrateMLSCommitLocks" runMigration
    }

counterName :: String
counterName = "^wire_mls_commit_locks_migration_finished"
