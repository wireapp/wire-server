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

module Test.Migration.BlockList (testBlockListMigration) where

import qualified API.BrigInternal as BrigInternal
import API.Common
import Control.Monad.Codensity
import Control.Monad.Reader
import Test.Migration.Util (waitForMigration)
import Testlib.Prelude
import Testlib.ResourcePool

-- | Migrate the 'blacklist' store (brig) from Cassandra to PostgreSQL.
--
-- The blacklist holds email keys with no read-back payload, so the migration is
-- a straight copy: a key blacklisted in Cassandra must survive the cutover and
-- remain deletable once PostgreSQL is the sole source of truth.
testBlockListMigration :: (HasCallStack) => App ()
testBlockListMigration = do
  resourcePool <- asks (.resourcePool)
  email <- randomEmail
  runCodensity (acquireResources 1 resourcePool) $ \[backend] -> do
    let domain = backend.berDomain

    -- Cassandra: blacklist an email key and confirm it is reported as such.
    runCodensity (startDynamicBackend backend (conf "cassandra" False)) $ \_ -> do
      assertSuccess =<< BrigInternal.addBlacklist domain email
      assertStatus 200 =<< BrigInternal.checkBlacklist domain email

    -- migration-to-postgresql with the worker running: backfill the existing key
    -- and confirm it is still blacklisted once the migration is finished.
    runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" True)) $ \_ -> do
      waitForMigration domain counterName
      assertStatus 200 =<< BrigInternal.checkBlacklist domain email

    -- PostgreSQL only: the migrated key must persist, and deleting it must
    -- remove it.
    runCodensity (startDynamicBackend backend (conf "postgresql" False)) $ \_ -> do
      assertStatus 200 =<< BrigInternal.checkBlacklist domain email
      assertSuccess =<< BrigInternal.deleteBlacklist domain email
      assertStatus 404 =<< BrigInternal.checkBlacklist domain email
  where
    conf :: String -> Bool -> ServiceOverrides
    conf db runMigration =
      def
        { brigCfg = setField "postgresMigration.blockList" db,
          backgroundWorkerCfg =
            setField "postgresMigration.blockList" db
              >=> setField "migrateBlockList" runMigration
        }

    counterName :: String
    counterName = "^wire_block_list_migration_finished"
