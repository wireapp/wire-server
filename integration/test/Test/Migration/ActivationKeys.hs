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

module Test.Migration.ActivationKeys where

import qualified API.Brig as Brig
import qualified API.BrigInternal as BrigI
import API.Common (randomEmail)
import Control.Monad.Codensity
import Control.Monad.Reader (asks)
import Test.Migration.Util (waitForMigration)
import Testlib.Prelude
import Testlib.ResourcePool
import Text.Printf (printf)

testActivationKeysMigration :: (HasCallStack) => App ()
testActivationKeysMigration = do
  resourcePool <- asks (.resourcePool)
  runCodensity (acquireResources 1 resourcePool) $ \[backend] -> do
    let domain = backend.berDomain

    -- cassandra: two pending registrations
    (k1, c1) <-
      runCodensity (startDynamicBackend backend (conf "cassandra" False))
        . const
        $ newPendingActivation domain
    (k2, c2) <-
      runCodensity (startDynamicBackend backend (conf "cassandra" False))
        . const
        $ newPendingActivation domain

    -- dual-write, worker off: one wrong attempt on the second registration
    -- (retries 3 -> 2, mirrored to Postgres)
    runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" False)) . const $ do
      wrongActivationCodeFails domain (k2, c2)

    -- dual-write, worker on: copy Cassandra rows; create a fresh pending
    -- registration
    (k4, c4) <-
      runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" True)) . const $ do
        waitForMigration domain counterName
        newPendingActivation domain

    -- postgresql: pre-migration codes activate from Postgres; retry state
    -- converged; exhaustion deletes the row
    runCodensity (startDynamicBackend backend (conf "postgresql" False)) . const $ do
      -- code created in cassandra mode activates (copied by the worker)
      activateCode domain (k1, c1)
      -- code that was wrong-attempted once in dual-write activates
      -- (mirrored writes kept Postgres in sync)
      activateCode domain (k2, c2)
      -- brute-force exhaustion: 3 wrong attempts decrement to 0, correct code
      -- still works, the 4th wrong attempt deletes the row, correct then fails
      forM_ [1 :: Int .. 3] $ \_ -> wrongActivationCodeFails domain (k4, c4)
      activateCode domain (k4, c4)
      wrongActivationCodeFails domain (k4, c4)
      bindResponse (Brig.activate domain k4 c4) $ \resp -> do
        resp.status `shouldMatchInt` 404
        resp.json %. "label" `shouldMatch` "invalid-code"
  where
    -- create a pending registration for a random email, then fetch its
    -- (key, code) via the internal API
    newPendingActivation domain = do
      email <- randomEmail
      Brig.activateSend domain email Nothing >>= assertSuccess
      bindResponse (BrigI.getActivationCode domain email) $ \resp -> do
        resp.status `shouldMatchInt` 200
        (,)
          <$> (resp.json %. "key" >>= asString)
          <*> (resp.json %. "code" >>= asString)

    activateCode domain (k, c) = Brig.activate domain k c >>= assertSuccess

    -- wrong code must 404 with 'invalid-code'
    wrongActivationCodeFails domain (k, c) = do
      let wrong = printf "%06d" $ (read @Int c + 1) `mod` 1000000
      bindResponse (Brig.activate domain k wrong) $ \resp -> do
        resp.status `shouldMatchInt` 404
        resp.json %. "label" `shouldMatch` "invalid-code"
    conf :: String -> Bool -> ServiceOverrides
    conf db runMigration =
      def
        { brigCfg = setField "postgresMigration.activationKeys" db,
          backgroundWorkerCfg =
            setField "postgresMigration.activationKeys" db
              >=> setField "migrateActivationKeys" runMigration
        }

    counterName :: String
    counterName = "^wire_activation_keys_migration_finished"
