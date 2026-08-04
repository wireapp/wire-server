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

module Test.Migration.Service where

import API.Brig
import API.Galley
import Control.Monad.Codensity
import Control.Monad.Reader
import Data.String.Conversions (cs)
import SetupHelpers
import Test.Bot (mkBotService)
import Test.Migration.Util (waitForMigration)
import Testlib.MockIntegrationService (MockServerSettings, withMockServer)
import Testlib.Prelude
import Testlib.ResourcePool

-- | Migrate the 'ServiceStore' (old bot provider/service connection data) from
-- Cassandra to PostgreSQL. A service is created while galley reads from
-- Cassandra; after the background worker backfills it, the service must still be
-- usable once galley reads exclusively from PostgreSQL (verified by adding a bot
-- to a conversation, which forces galley to look up the service).
testServiceMigration :: (HasCallStack) => App ()
testServiceMigration = do
  resourcePool <- asks (.resourcePool)
  let settings = def :: MockServerSettings
  runCodensity (acquireResources 1 resourcePool) $ \[backend] -> do
    let domain = backend.berDomain
    -- The mock service must stay alive across all lifecycle stages, since adding
    -- a bot in the final (PostgreSQL) stage requires the service endpoint to be
    -- reachable.
    withMockServer settings mkBotService $ \(host, port) _chan -> do
      let serviceUrl = "https://" <> host <> ":" <> show port
      (alice, providerId, serviceId) <-
        runCodensity (startDynamicBackend backend (conf "cassandra" False)) $ \_ -> do
          alice <- randomUser OwnDomain def
          password <- randomString 20
          provider <- setupProvider alice def {newProviderPassword = Just password}
          pid <- provider %. "id" & asString
          service <-
            newService OwnDomain pid
              $ def {newServiceUrl = serviceUrl, newServiceKey = cs settings.publicKey}
          sid <- asString $ service %. "id"
          assertStatus 200 =<< updateServiceConn OwnDomain pid sid (object ["enabled" .= True, "password" .= password])
          pure (alice, pid, sid)
      runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" True)) $ \_ ->
        waitForMigration domain counterName
      runCodensity (startDynamicBackend backend (conf "postgresql" False)) $ \_ -> do
        conv <- getJSON 201 =<< postConversation alice defProteus
        cid <- conv %. "qualified_id" & objId
        bindResponse (addBot alice providerId serviceId cid) $ \res ->
          res.status `shouldMatchInt` 201
  where
    conf :: String -> Bool -> ServiceOverrides
    conf db runMigration =
      def
        { galleyCfg = setField "postgresMigration.service" db,
          backgroundWorkerCfg = setField "migrateService" runMigration
        }

counterName :: String
counterName = "^wire_service_migration_finished"
