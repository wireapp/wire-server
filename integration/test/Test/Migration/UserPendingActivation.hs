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

module Test.Migration.UserPendingActivation (testUserPendingActivationMigration) where

import qualified API.BrigInternal as BrigInternal
import Control.Monad.Codensity
import Control.Monad.Reader
import SetupHelpers (randomId)
import Test.Migration.Util (waitForMigration)
import Testlib.Prelude
import Testlib.ResourcePool

-- | Migrate the 'users_pending_activation' store (brig) from Cassandra to
-- PostgreSQL.
--
-- The store holds pending SCIM invitations; the migration is a copy-only
-- backfill. A row added while Cassandra is the source of truth must survive
-- the copy loop and remain readable after the cutover to PostgreSQL, and a row
-- added after the cutover must live in PostgreSQL.
testUserPendingActivationMigration :: (HasCallStack) => App ()
testUserPendingActivationMigration = do
  resourcePool <- asks (.resourcePool)
  uid1 <- randomId
  uid2 <- randomId
  let expiresAt = "2100-01-01T00:00:00Z"
  runCodensity (acquireResources 1 resourcePool) $ \[backend] -> do
    let domain = backend.berDomain

    -- Cassandra: add a pending activation row and read it back.
    runCodensity (startDynamicBackend backend (conf "cassandra" False)) $ \_ -> do
      assertSuccess =<< BrigInternal.putUserPendingActivation domain uid1 expiresAt
      resp <- BrigInternal.getUserPendingActivations domain
      assertStatus 200 resp
      resp.json %. "0.0" >>= asString >>= (`shouldMatch` uid1)
      resp.json %. "0.1" >>= asString >>= (`shouldMatch` expiresAt)

    -- migration-to-postgresql with the worker running: copy the row and
    -- confirm it is still readable (via Cassandra, the source of truth).
    runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" True)) $ \_ -> do
      waitForMigration domain counterName
      resp <- BrigInternal.getUserPendingActivations domain
      assertStatus 200 resp
      resp.json %. "0.0" >>= asString >>= (`shouldMatch` uid1)

    -- PostgreSQL only: the migrated row must persist, and a row written after
    -- the cutover must be readable from PostgreSQL too.
    runCodensity (startDynamicBackend backend (conf "postgresql" False)) $ \_ -> do
      resp <- BrigInternal.getUserPendingActivations domain
      assertStatus 200 resp
      resp.json %. "0.0" >>= asString >>= (`shouldMatch` uid1)
      assertSuccess =<< BrigInternal.putUserPendingActivation domain uid2 expiresAt
      resp' <- BrigInternal.getUserPendingActivations domain
      uidA <- resp'.json %. "0.0" >>= asString
      uidB <- resp'.json %. "1.0" >>= asString
      shouldMatch [uidA, uidB] (sort [uid1, uid2])
  where
    conf :: String -> Bool -> ServiceOverrides
    conf db runMigration =
      def
        { brigCfg = setField "postgresMigration.userPendingActivation" db,
          backgroundWorkerCfg =
            setField "postgresMigration.userPendingActivation" db
              >=> setField "migrateUsersPendingActivation" runMigration
        }

    counterName :: String
    counterName = "^wire_users_pending_activation_migration_finished"
