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

module Test.Migration.Properties (testPropertiesMigration) where

import API.Brig
import Control.Monad.Codensity
import Control.Monad.Reader
import SetupHelpers (createUsers)
import Test.Migration.Util (waitForMigration)
import Testlib.Prelude
import Testlib.ResourcePool

-- | Migrate the 'properties' store (brig) from Cassandra to PostgreSQL.
--
-- User properties are plain key/value rows, so the migration is a straight
-- copy: a property set in Cassandra must survive the cutover, be updateable,
-- and be deletable once PostgreSQL is the sole source of truth.
testPropertiesMigration :: (HasCallStack) => App ()
testPropertiesMigration = do
  resourcePool <- asks (.resourcePool)
  runCodensity (acquireResources 1 resourcePool) $ \[backend] -> do
    let domain = backend.berDomain

    -- Cassandra: set a property and confirm it is readable.
    runCodensity (startDynamicBackend backend (conf "cassandra" False)) $ \_ -> do
      [alice] <- createUsers [domain]
      setProperty alice "color" ("green" :: String) `bindResponse` \resp ->
        resp.status `shouldMatchInt` 200

      -- migration-to-postgresql with the worker running: backfill the existing
      -- property and confirm it survives once the migration is finished.
      -- New writes go to both stores.
      runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" True)) $ \_ -> do
        waitForMigration domain counterName
        getProperty alice "color" `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 200
          resp.json `shouldMatch` toJSON ("green" :: String)
        setProperty alice "size" ("large" :: String) `bindResponse` \resp ->
          resp.status `shouldMatchInt` 200

      -- PostgreSQL only: migrated and dual-written properties must persist,
      -- and deleting must remove them.
      runCodensity (startDynamicBackend backend (conf "postgresql" False)) $ \_ -> do
        getProperty alice "color" `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 200
          resp.json `shouldMatch` toJSON ("green" :: String)
        getProperty alice "size" `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 200
          resp.json `shouldMatch` toJSON ("large" :: String)
        deleteProperty alice "color" `bindResponse` \resp ->
          resp.status `shouldMatchInt` 200
        getProperty alice "color" `bindResponse` \resp ->
          resp.status `shouldMatchInt` 404
  where
    conf :: String -> Bool -> ServiceOverrides
    conf db runMigration =
      def
        { brigCfg = setField "postgresMigration.properties" db,
          backgroundWorkerCfg =
            setField "postgresMigration.properties" db
              >=> setField "migrateProperties" runMigration
        }

    counterName :: String
    counterName = "^wire_properties_migration_finished"
