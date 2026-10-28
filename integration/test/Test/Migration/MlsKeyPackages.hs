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

module Test.Migration.MlsKeyPackages (testMlsKeyPackagesMigration) where

import API.Brig
import Control.Monad.Codensity
import Control.Monad.Reader
import MLS.Util (ciphersuites, createMLSClient, generateKeyPackage)
import SetupHelpers
import Test.Migration.Util (waitForMigration)
import Testlib.Prelude
import Testlib.ResourcePool

-- | Drives the mls key package store through the full cutover lifecycle
-- (cassandra -> migration-to-postgresql -> postgresql) via the real brig API.
-- A single user and MLS client is created in the first phase and reused in
-- every later phase so that rows written by one interpreter are visible to
-- the interpreters of the following phases (and to the backfill).
testMlsKeyPackagesMigration :: (HasCallStack) => App ()
testMlsKeyPackagesMigration = do
  resourcePool <- asks (.resourcePool)
  runCodensity (acquireResources 1 resourcePool) $ \[backend] -> do
    let domain = backend.berDomain
        altSuite = Ciphersuite "0x0007"

    -- P1 cassandra: upload 2 default-suite + 1 alt-suite KPs, claim one
    -- default-suite KP.
    (alice, alice1, refAlt) <-
      runCodensity (startDynamicBackend backend (conf "cassandra" False)) $ \_ -> do
        alice <- randomUser OwnDomain def
        alice1 <- createMLSClient def {ciphersuites = [def, altSuite]} alice
        (kp1, _) <- generateKeyPackage alice1 def
        (kp2, _) <- generateKeyPackage alice1 def
        (kpAlt, refAlt) <- generateKeyPackage alice1 altSuite
        uploadKeyPackages alice1 [kp1, kp2] >>= assertStatus 201
        uploadKeyPackages alice1 [kpAlt] >>= assertStatus 201

        claimKeyPackages def alice1 alice >>= assertStatus 200

        assertCount def alice1 1
        assertCount altSuite alice1 1
        pure (alice, alice1, refAlt)

    -- P2 migration-to-postgresql (worker off): brig keeps serving Cassandra;
    -- nothing is written to Postgres.
    runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" False)) $ \_ -> do
      (kp, _) <- generateKeyPackage alice1 def
      uploadKeyPackages alice1 [kp] >>= assertStatus 201

      claimKeyPackages def alice1 alice >>= assertStatus 200
      assertCount def alice1 1
      assertCount altSuite alice1 1

    -- P3 migration-to-postgresql (worker on): copy loop backfills Postgres;
    -- brig reads still come from Cassandra.
    runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" True)) $ \_ -> do
      (kp, _) <- generateKeyPackage alice1 def
      uploadKeyPackages alice1 [kp] >>= assertStatus 201

      waitForMigration domain counterName

      claimKeyPackages def alice1 alice >>= assertStatus 200
      assertCount def alice1 1
      assertCount altSuite alice1 1

    -- P4 postgresql: reads are served exclusively from Postgres.
    runCodensity (startDynamicBackend backend (conf "postgresql" False)) $ \_ -> do
      -- The alt-suite row written in P1 (pure Cassandra) must have been
      -- backfilled by the background worker; delete it through Postgres.
      assertCount altSuite alice1 1
      deleteKeyPackages altSuite alice1 [refAlt] >>= assertStatus 200
      assertCount altSuite alice1 0

      -- replaceKeyPackages (PUT) exercises deleteAll + insert in Postgres.
      (kp, _) <- generateKeyPackage alice1 def
      replaceKeyPackages alice1 [def] [kp] >>= assertStatus 200
      assertCount def alice1 1

      claimKeyPackages def alice1 alice >>= assertStatus 200
      assertCount def alice1 0
  where
    conf db runMigration =
      def
        { brigCfg = setField "postgresMigration.mlsKeyPackages" db,
          backgroundWorkerCfg =
            setField "postgresMigration.mlsKeyPackages" db
              >=> setField "migrateMlsKeyPackages" runMigration
        }
    counterName = "^wire_mls_key_packages_migration_finished"
    assertCount suite cid n =
      countKeyPackages suite cid `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json %. "count" `shouldMatchInt` n
