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
import API.Common
import API.Galley
import API.GalleyInternal (deleteServiceConnInternal)
import Control.Monad.Codensity
import Control.Monad.Reader
import Data.String.Conversions (cs)
import SetupHelpers
import Test.Bot (BotEvent (..), mkBotService, postBotMessage)
import Test.Migration.Util (waitForMigration)
import Testlib.MockIntegrationService (MockServerSettings (..), withMockServer)
import Testlib.Prelude
import Testlib.ResourcePool
import UnliftIO (Chan, readChan, timeout)

-- | Migrate the 'ServiceStore' (old bot provider/service connection data) from
-- Cassandra to PostgreSQL. The mock phase (Cassandra-only) creates service s1;
-- the dual-write phase (backfill done, dual-write interpreter is the only
-- writer) then drives three mutations through brig's provider API and galley's
-- dual-write interpreter:
--
--   1. /Update/ s1's connection to point at a different mock service — proves
--      PG serves the dual-write-updated URL after cutover (a stale backfilled
--      row would deliver the webhook to the wrong mock).
--   2. /Create/ s2 — a row Cassandra never had before the dual-write — proving
--      the mirror writes are live (the test fails if they are removed).
--   3. /Delete/ s3 — proving the delete is mirrored synchronously and no
--      backfill replay resurrects it.
--
-- After cutover (PostgreSQL-only), adding a bot forces galley to look the
-- service up in PostgreSQL; a posted message asserts the mock service actually
-- receives the BotMessage webhook, proving each migrated row end-to-end. Each
-- fixture service has its own mock server: @addBot@ never fails on a missing
-- service (galley inserts the bot member row unconditionally and the webhook
-- deliverer silently skips orphans), so a deleted service is proven by the
-- /absence/ of its webhook on its own channel.
testServiceMigration :: (HasCallStack) => App ()
testServiceMigration = do
  resourcePool <- asks (.resourcePool)
  let settings = def :: MockServerSettings
  runCodensity (acquireResources 1 resourcePool) $ \[backend] -> do
    let domain = backend.berDomain
    -- The three mock services must stay alive across all lifecycle stages,
    -- since adding bots in the final (PostgreSQL) stage requires the service
    -- endpoints to be reachable.
    withMockServer settings mkBotService $ \(host1, port1) chan1 ->
      withMockServer settings mkBotService $ \(host2, port2) chan2 ->
        withMockServer settings mkBotService $ \(host3, port3) chan3 -> do
          let serviceUrl1 = "https://" <> host1 <> ":" <> show port1
              serviceUrl2 = "https://" <> host2 <> ":" <> show port2
              serviceUrl3 = "https://" <> host3 <> ":" <> show port3
          -- P1: cassandra (worker off) — create s1 and connect it.
          (alice, providerId, password, sid1) <-
            runCodensity (startDynamicBackend backend (conf "cassandra" False)) $ \_ -> do
              alice <- randomUser domain def
              password <- randomString 20
              provider <- setupProvider alice def {newProviderPassword = Just password}
              pid <- provider %. "id" & asString
              service <-
                newService domain pid
                  $ def {newServiceUrl = serviceUrl1, newServiceKey = cs settings.publicKey}
              sid <- asString $ service %. "id"
              assertStatus 200 =<< updateServiceConn domain pid sid (object ["enabled" .= True, "password" .= password])
              pure (alice, pid, password, sid)
          -- P2: migration-to-postgresql (worker on) — after the backfill the
          -- dual-write interpreter is the only writer, so the fixtures below
          -- don't race the worker's stale replays.
          (sid2, sid3) <-
            runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" True)) $ \_ -> do
              waitForMigration domain counterName
              -- Update: s1's connection moves to the second mock service; PG
              -- now holds that URL while Cassandra still has the first one.
              assertStatus 200
                =<< updateServiceConn
                  domain
                  providerId
                  sid1
                  (object ["enabled" .= True, "password" .= password, "base_url" .= serviceUrl2])
              -- Create: s2, a row Cassandra never had before this write.
              let newEnabledService url = do
                    service <-
                      newService domain providerId
                        $ def {newServiceUrl = url, newServiceKey = cs settings.publicKey}
                    sid <- asString $ service %. "id"
                    assertStatus 200 =<< updateServiceConn domain providerId sid (object ["enabled" .= True, "password" .= password])
                    pure sid
              sid2 <- newEnabledService serviceUrl1
              -- Delete: s3 is removed through the same internal route brig's
              -- removeServiceConn RPC uses; the dual-write interpreter removes
              -- it from Cassandra and PostgreSQL synchronously, so no backfill
              -- replay can resurrect it.
              sid3 <- newEnabledService serviceUrl3
              assertStatus 200 =<< deleteServiceConnInternal alice providerId sid3
              pure (sid2, sid3)
          -- P3: postgresql (cutover, worker off).
          runCodensity (startDynamicBackend backend (conf "postgresql" False)) $ \_ -> do
            conv <- getJSON 201 =<< postConversation alice defProteus
            cid <- conv %. "qualified_id" & objId
            -- s2 (dual-write-created row) delivers to the first mock service.
            bindResponse (addBot alice providerId sid2 cid) $ \res -> do
              res.status `shouldMatchInt` 201
              botId <- res.json %. "id"
              botClient <- res.json %. "client"
              postBotMessage alice conv botId botClient
              assertBotMessageReceived chan1
            -- s1 (dual-write-updated row) delivers to the second mock service;
            -- if the stale backfilled (pre-update) row had won, the webhook
            -- would land on the first one instead.
            bindResponse (addBot alice providerId sid1 cid) $ \res -> do
              res.status `shouldMatchInt` 201
              botId <- res.json %. "id"
              botClient <- res.json %. "client"
              postBotMessage alice conv botId botClient
              assertBotMessageReceived chan2
            -- s3 (dual-write-deleted row): the member row is inserted
            -- regardless, but addBot's own BotCreated webhook must not reach
            -- the third mock service. Ordered last so the two positive checks
            -- prove the delivery pipeline is warm before the negative window
            -- opens.
            bindResponse (addBot alice providerId sid3 cid) $ \res ->
              res.status `shouldMatchInt` 201
            t <- asks (.timeOutSeconds)
            timeout (t * 1_000_000) (readChan chan3) >>= \case
              Just _ -> assertFailure "deleted service still receives webhooks after cutover"
              Nothing -> pure ()
  where
    conf :: String -> Bool -> ServiceOverrides
    conf db runMigration =
      def
        { galleyCfg = setField "postgresMigration.service" db,
          backgroundWorkerCfg = setField "migrateService" runMigration
        }

counterName :: String
counterName = "^wire_service_migration_finished"

assertBotMessageReceived :: (HasCallStack) => Chan BotEvent -> App ()
assertBotMessageReceived chan = do
  t <- asks (.timeOutSeconds)
  timeout (t * 1_000_000) go >>= \case
    Nothing -> assertFailure "mock service received no bot message after migration"
    Just () -> pure ()
  where
    go =
      readChan chan >>= \case
        BotMessage _ -> pure ()
        BotCreated -> go -- drain the addBot webhook first
