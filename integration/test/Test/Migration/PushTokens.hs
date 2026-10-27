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

module Test.Migration.PushTokens where

import API.Common (randomClientId)
import API.Gundeck
import Control.Monad.Codensity
import Control.Monad.Reader
import SetupHelpers
import Test.Migration.Util (waitForMigration)
import Testlib.Prelude
import Testlib.ResourcePool

testPushTokensMigration :: (HasCallStack) => App ()
testPushTokensMigration = do
  resourcePool <- asks (.resourcePool)
  runCodensity (acquireResources 1 resourcePool) $ \[backend] -> do
    let domain = backend.berDomain

    -- Phase 1: cassandra. Register one token; reads come from Cassandra.
    _tokens1 <- runCodensity (startDynamicBackend backend (conf "cassandra" False)) $ \_ -> do
      alice <- randomUser domain def
      cid <- randomClientId
      tok <- registerToken alice cid "tok-one"
      listed <- listPushTokens alice >>= getJSON 200 >>= asList
      tokenIsListed tok listed >>= assertBool "token from phase 1 is listed"
      pure [tok]

    -- Phase 2: migration-to-postgresql, worker off. Writes are mirrored to
    -- Postgres; reads still come from Cassandra.
    _tokens2 <- runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" False)) $ \_ -> do
      alice <- randomUser domain def
      cid <- randomClientId
      tok1 <- registerToken alice cid "tok-one"
      tok2 <- registerToken alice cid "tok-two"
      listed <- listPushTokens alice >>= getJSON 200 >>= asList
      tokenIsListed tok1 listed >>= assertBool "phase 1 token still listed"
      tokenIsListed tok2 listed >>= assertBool "phase 2 token listed"
      pure [tok1, tok2]

    -- Phase 3: migration-to-postgresql, worker on. The copy loop replicates
    -- the Cassandra rows written in this phase before the read below.
    _tokens3 <- runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" True)) $ \_ -> do
      alice <- randomUser domain def
      cid <- randomClientId
      tok1 <- registerToken alice cid "tok-one"
      tok2 <- registerToken alice cid "tok-two"
      waitForMigration domain counterName
      listed <- listPushTokens alice >>= getJSON 200 >>= asList
      tokenIsListed tok1 listed >>= assertBool "phase 1 token still listed"
      tokenIsListed tok2 listed >>= assertBool "phase 2 token still listed"
      pure [tok1, tok2]

    -- Phase 4: postgresql. Reads and writes come from Postgres alone;
    -- register, delete and list are all served from Postgres.
    runCodensity (startDynamicBackend backend (conf "postgresql" False)) $ \_ -> do
      alice <- randomUser domain def
      cid <- randomClientId
      tok3 <- registerToken alice cid "tok-three"
      deletePushToken alice tok3 >>= assertSuccess
      tok4 <- registerToken alice cid "tok-four"
      listed <- listPushTokens alice >>= getJSON 200 >>= asList
      tokenIsListed tok3 listed >>= assertBool "deleted token is not listed" . not
      tokenIsListed tok4 listed >>= assertBool "new token is listed"
  where
    conf :: String -> Bool -> ServiceOverrides
    conf db runMigration =
      def
        { gundeckCfg = setField "postgresMigration.pushTokens" db,
          backgroundWorkerCfg = setField "migratePushTokens" runMigration
        }

    counterName :: String
    counterName = "^wire_push_tokens_migration_finished"

    -- Register a push token for the given client and return the token string.
    registerToken :: (HasCallStack, MakesValue user, MakesValue client) => user -> client -> String -> App String
    registerToken user cid stem = do
      suffix <- generateToken 8
      let tok = stem <> "-" <> suffix
      clientId <- make cid & asString
      _ <- postPushToken user $ PushToken "GCM" "test" tok clientId
      pure tok

    tokenIsListed :: (MakesValue a) => String -> [a] -> App Bool
    tokenIsListed tok items = do
      tokens <- traverse (\item -> item %. "token" & asString) items
      pure $ tok `elem` tokens

    deletePushToken :: (HasCallStack, MakesValue user) => user -> String -> App Response
    deletePushToken user tok = do
      req <- baseRequest user Gundeck Versioned $ joinHttpPath ["/push/tokens", tok]
      submit "DELETE" req
