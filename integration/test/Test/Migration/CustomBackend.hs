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

module Test.Migration.CustomBackend (testCustomBackendMigration) where

import API.Galley
import API.GalleyInternal
import Control.Monad.Codensity
import Control.Monad.Reader
import SetupHelpers
import Test.Migration.Util (waitForMigration)
import Testlib.Prelude
import Testlib.ResourcePool

-- | Drives the custom-backend store through the full cutover lifecycle
-- (cassandra -> migration-to-postgresql -> postgresql). Every phase writes and
-- reads, so all three interpreters (Cassandra, dual-write, Postgres) are
-- exercised on both paths.
--
-- Rows created before the cutover (d1: pure Cassandra writes, d2: dual-write)
-- must be served by the pure-Postgres interpreter afterwards — d1 proves the
-- background worker backfilled a row that only ever existed in Cassandra. The
-- re-put of d1 during the backfill proves the Postgres interpreter upserts
-- (UPDATE-as-insert, like the Cassandra store).
testCustomBackendMigration :: (HasCallStack) => App ()
testCustomBackendMigration = do
  resourcePool <- asks (.resourcePool)
  runCodensity (acquireResources 1 resourcePool) $ \[backend] -> do
    let domain = backend.berDomain

    -- P1 cassandra: write via the pure Cassandra interpreter
    owner <-
      runCodensity (startDynamicBackend backend (conf "cassandra" False)) $ \_ -> do
        (owner, _, _) <- createTeam domain 1
        putCustomBackend owner d1Domain d1Payload >>= assertStatus 201
        checkCustomBackend owner d1Domain d1ConfigUrl d1WelcomeUrl
        pure owner

    -- P2 migration-to-postgresql (worker off): reads still come from
    -- Cassandra, writes go to both stores
    runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" False)) $ \_ -> do
      checkCustomBackend owner d1Domain d1ConfigUrl d1WelcomeUrl
      putCustomBackend owner d2Domain d2Payload >>= assertStatus 201
      checkCustomBackend owner d2Domain d2ConfigUrl d2WelcomeUrl

    -- P3 migration-to-postgresql (worker on): dual-write while backfilling
    runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" True)) $ \_ -> do
      checkCustomBackend owner d1Domain d1ConfigUrl d1WelcomeUrl
      checkCustomBackend owner d2Domain d2ConfigUrl d2WelcomeUrl
      putCustomBackend owner d3Domain d3Payload >>= assertStatus 201
      -- Backfill done: dual-write is now the only writer, so the re-put below
      -- proves UPDATE-as-insert without racing the worker's stale copy.
      waitForMigration domain counterName
      -- UPDATE-as-insert: overwriting d1 must replace its config URL in both
      -- stores.
      putCustomBackend owner d1Domain d1Payload' >>= assertStatus 201
      checkCustomBackend owner d1Domain d1ConfigUrl' d1WelcomeUrl

    -- P4 postgresql: reads are served exclusively from Postgres
    runCodensity (startDynamicBackend backend (conf "postgresql" False)) $ \_ -> do
      checkCustomBackend owner d1Domain d1ConfigUrl' d1WelcomeUrl
      checkCustomBackend owner d2Domain d2ConfigUrl d2WelcomeUrl
      checkCustomBackend owner d3Domain d3ConfigUrl d3WelcomeUrl

      -- Write and delete via the pure Postgres interpreter.
      deleteCustomBackend owner d3Domain >>= assertStatus 200
      getCustomBackend owner d3Domain >>= assertStatus 404
  where
    conf db runMigration =
      def
        { galleyCfg = setField "postgresMigration.customBackend" db,
          backgroundWorkerCfg = setField "migrateCustomBackend" runMigration
        }
    counterName = "^wire_custom_backend_migration_finished"

    d1Domain = "example.com"
    d1ConfigUrl = "https://example.com/config.json"
    d1ConfigUrl' = "https://example.com/config-v2.json"
    d1WelcomeUrl = "https://example.com/welcome"
    d1Payload =
      object
        [ "config_json_url" .= d1ConfigUrl,
          "webapp_welcome_url" .= d1WelcomeUrl
        ]
    d1Payload' =
      object
        [ "config_json_url" .= d1ConfigUrl',
          "webapp_welcome_url" .= d1WelcomeUrl
        ]

    d2Domain = "example.org"
    d2ConfigUrl = "https://example.org/config.json"
    d2WelcomeUrl = "https://example.org/welcome"
    d2Payload =
      object
        [ "config_json_url" .= d2ConfigUrl,
          "webapp_welcome_url" .= d2WelcomeUrl
        ]

    d3Domain = "example.net"
    d3ConfigUrl = "https://example.net/config.json"
    d3WelcomeUrl = "https://example.net/welcome"
    d3Payload =
      object
        [ "config_json_url" .= d3ConfigUrl,
          "webapp_welcome_url" .= d3WelcomeUrl
        ]

    checkCustomBackend user dom configUrl welcomeUrl =
      bindResponse (getCustomBackend user dom) $ \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json %. "config_json_url" `shouldMatch` configUrl
        resp.json %. "webapp_welcome_url" `shouldMatch` welcomeUrl
