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

module Test.Migration.Sessions (testSessionsMigration) where

import API.Brig (getCookies, login)
import API.Common (defPassword)
import Control.Monad.Codensity
import Control.Monad.Reader
import SetupHelpers
import Test.Migration.Util (waitForMigration)
import Testlib.Prelude
import Testlib.ResourcePool

-- | Drives the session-cookie store (brig user_cookies) through the full
-- cutover lifecycle (cassandra -> migration-to-postgresql -> postgresql).
-- Logging in creates a cookie, so each phase both writes with the active
-- interpreter and reads what earlier interpreters wrote. In particular, the
-- cookie of the user created in the pure-Cassandra phase must be served by the
-- pure-Postgres interpreter after the cutover, which proves the background
-- worker backfilled it.
testSessionsMigration :: (HasCallStack) => App ()
testSessionsMigration = do
  resourcePool <- asks (.resourcePool)
  runCodensity (acquireResources 1 resourcePool) $ \[backend] -> do
    let domain = backend.berDomain

    -- P1 cassandra: create a user and confirm its session cookie is listed.
    u1 <- runCodensity (startDynamicBackend backend (conf "cassandra" False)) $ \_ -> do
      u1 <- randomUser domain def
      checkCookies domain u1
      pure u1

    -- P2 migration-to-postgresql (worker off): dual-write without backfill.
    u2 <- runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" False)) $ \_ -> do
      checkCookies domain u1
      u2 <- randomUser domain def
      checkCookies domain u2
      pure u2

    -- P3 migration-to-postgresql (worker on): dual-write while backfilling.
    u3 <- runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" True)) $ \_ -> do
      checkCookies domain u1
      checkCookies domain u2
      u3 <- randomUser domain def
      checkCookies domain u3
      waitForMigration domain counterName
      pure u3

    -- P4 postgresql: reads are served exclusively from Postgres.
    runCodensity (startDynamicBackend backend (conf "postgresql" False)) $ \_ -> do
      -- The row that only ever existed in Cassandra (u1) proves the backfill.
      checkCookies domain u1
      checkCookies domain u2
      checkCookies domain u3

      -- Write and read via the pure Postgres interpreter.
      u4 <- randomUser domain def
      checkCookies domain u4
  where
    conf :: String -> Bool -> ServiceOverrides
    conf db runMigration =
      def
        { brigCfg = setField "postgresMigration.sessions" db,
          backgroundWorkerCfg =
            setField "postgresMigration.sessions" db
              >=> setField "migrateSessions" runMigration
        }

    counterName :: String
    counterName = "^wire_sessions_migration_finished"

    -- Logging in re-issues a session cookie and the list endpoint must report
    -- at least one cookie for the user afterwards.
    checkCookies :: (HasCallStack, MakesValue user, MakesValue domain) => domain -> user -> App ()
    checkCookies domain u = do
      email <- asString (u %. "email")
      login domain email defPassword >>= assertSuccess
      resp <- getCookies u [] >>= getJSON 200
      cookies <- resp & asList
      assertBool "expected at least one session cookie" (not (null cookies))
