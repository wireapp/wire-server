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

module Test.Migration.UserKeys (testUserKeysMigration) where

import qualified API.BrigInternal as BrigInternal
import Control.Monad.Codensity
import Control.Monad.Reader
import SetupHelpers (randomId)
import Test.Migration.Util (waitForMigration)
import Testlib.Prelude
import Testlib.ResourcePool

-- | Migrate the 'user_keys' store (brig) from Cassandra to PostgreSQL.
--
-- The store is the normalized email -> user lookup index. The migration is a
-- copy-only backfill. A key written while Cassandra is the source of truth
-- must survive the copy loop and remain readable after the cutover to
-- PostgreSQL, a key written while dual-writing must land in PostgreSQL via the
-- mirror, and a key written after the cutover must live in PostgreSQL.
testUserKeysMigration :: (HasCallStack) => App ()
testUserKeysMigration = do
  resourcePool <- asks (.resourcePool)
  uid1 <- randomId
  uid2 <- randomId
  uid3 <- randomId
  let key1 = uid1 <> "@example.com"
      key2 = uid2 <> "@example.com"
      key3 = uid3 <> "@example.com"
  runCodensity (acquireResources 1 resourcePool) $ \[backend] -> do
    let domain = backend.berDomain
        expectOwner key uid = do
          resp <- BrigInternal.getUserKey domain key
          assertStatus 200 resp
          asString resp.json >>= (`shouldMatch` uid)

    -- Cassandra: insert a key and read it back.
    runCodensity (startDynamicBackend backend (conf "cassandra" False)) $ \_ -> do
      assertSuccess =<< BrigInternal.putUserKey domain uid1 key1
      expectOwner key1 uid1

    -- migration-to-postgresql with the worker running: copy the key and
    -- confirm it is still readable (via Cassandra, the source of truth), and
    -- dual-write a new key that must reach PostgreSQL through the mirror.
    runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" True)) $ \_ -> do
      waitForMigration domain counterName
      expectOwner key1 uid1
      assertSuccess =<< BrigInternal.putUserKey domain uid3 key3

    -- PostgreSQL only: the migrated key and the dual-written key must persist,
    -- and a key written after the cutover must be readable from PostgreSQL too.
    runCodensity (startDynamicBackend backend (conf "postgresql" False)) $ \_ -> do
      expectOwner key1 uid1
      expectOwner key3 uid3
      assertSuccess =<< BrigInternal.putUserKey domain uid2 key2
      expectOwner key2 uid2
  where
    conf :: String -> Bool -> ServiceOverrides
    conf db runMigration =
      def
        { brigCfg = setField "postgresMigration.userKey" db,
          backgroundWorkerCfg =
            setField "postgresMigration.userKey" db
              >=> setField "migrateUserKeys" runMigration
        }

    counterName :: String
    counterName = "^wire_user_keys_migration_finished"
