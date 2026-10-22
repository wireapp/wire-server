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

module Test.Migration.ProviderPassword (testProviderPasswordMigration) where

import qualified API.BrigInternal as BrigInternal
import API.Common
import Control.Monad.Codensity
import Control.Monad.Reader
import Test.Migration.Util (waitForMigration)
import SetupHelpers (randomId)
import Testlib.Prelude
import Testlib.ResourcePool

-- | Migrate the 'provider_password' store (brig) from Cassandra to PostgreSQL.
--
-- The store is read-only from the perspective of normal traffic (provider login
-- lookups), so the migration is a copy-only backfill with no dual-write
-- interpreter. A password set while Cassandra is the source of truth must keep
-- verifying after the copy loop runs and after the cutover to PostgreSQL.
testProviderPasswordMigration :: (HasCallStack) => App ()
testProviderPasswordMigration = do
  resourcePool <- asks (.resourcePool)
  password <- randomPassword
  password2 <- randomPassword
  pid <- randomId
  runCodensity (acquireResources 1 resourcePool) $ \[backend] -> do
    let domain = backend.berDomain

    -- Cassandra: set a provider password and confirm it verifies.
    runCodensity (startDynamicBackend backend (conf "cassandra" False)) $ \_ -> do
      assertSuccess =<< BrigInternal.putProviderPassword domain pid password
      assertSuccess =<< BrigInternal.checkProviderPassword domain pid password
      assertStatus 404 =<< BrigInternal.checkProviderPassword domain pid (password <> "-wrong")

    -- migration-to-postgresql with the worker running: copy the row and
    -- confirm the password still verifies from Cassandra (source of truth).
    runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" True)) $ \_ -> do
      waitForMigration domain counterName
      assertSuccess =<< BrigInternal.checkProviderPassword domain pid password

    -- PostgreSQL only: the copied password must verify from the new store,
    -- and a password written after the cutover must live in PostgreSQL too.
    runCodensity (startDynamicBackend backend (conf "postgresql" False)) $ \_ -> do
      assertSuccess =<< BrigInternal.checkProviderPassword domain pid password
      assertSuccess =<< BrigInternal.putProviderPassword domain pid password2
      assertSuccess =<< BrigInternal.checkProviderPassword domain pid password2
      assertStatus 404 =<< BrigInternal.checkProviderPassword domain pid password

  where
    conf :: String -> Bool -> ServiceOverrides
    conf db runMigration =
      def
        { brigCfg = setField "postgresMigration.providerPassword" db,
          backgroundWorkerCfg =
            setField "postgresMigration.providerPassword" db
              >=> setField "migrateProviderPasswords" runMigration
        }

    counterName :: String
    counterName = "^wire_provider_passwords_migration_finished"
