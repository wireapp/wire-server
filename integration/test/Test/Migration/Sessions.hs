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
import Test.Migration.Util (assertMigrationSuccessful, retryTransient, waitForMigration)
import Testlib.Prelude
import Testlib.ResourcePool

-- | Drives the session-cookie store (brig user_cookies) through the full
-- cutover lifecycle (cassandra -> migration-to-postgresql -> postgresql).
-- The cookie ids recorded in the pure-Cassandra phase must still be listed by
-- the pure-Postgres interpreter after the cutover, which proves the background
-- worker decoded the scan and backfilled them; logins during later phases
-- additionally prove that each interpreter's write path works.
testSessionsMigration :: (HasCallStack) => App ()
testSessionsMigration = do
  resourcePool <- asks (.resourcePool)
  runCodensity (acquireResources 1 resourcePool) $ \[backend] -> do
    let domain = backend.berDomain

    -- P1 cassandra: create a user and record its session cookies.  No login
    -- happens during the read, so the recorded ids come from the
    -- Cassandra-phase write alone and phase 4 can prove the backfill copied
    -- exactly these rows.
    (u1, ids1) <- runCodensity (startDynamicBackend backend (conf "cassandra" False)) $ \_ -> do
      u1 <- randomUser domain def
      ids1 <- readCookieIds u1
      assertBool "expected at least one session cookie after registration" (not (null ids1))
      pure (u1, ids1)

    -- P2 migration-to-postgresql (worker off): dual-write without backfill.
    u2 <- runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" False)) $ \_ -> do
      loginAndCheckCookies domain u1
      u2 <- randomUser domain def
      loginAndCheckCookies domain u2
      pure u2

    -- P3 migration-to-postgresql (worker on): dual-write while backfilling.
    -- A concurrent login can transiently hit the backfill's exclusive
    -- per-user migration lock (500 TimedOutAcquiringLock), so the
    -- login-and-check calls are retried; retried logins may duplicate
    -- cookies, which is harmless for the assertions used.
    u3 <- runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" True)) $ \_ -> do
      loginAndCheckCookiesRetry domain u1
      loginAndCheckCookiesRetry domain u2
      u3 <- randomUser domain def
      loginAndCheckCookiesRetry domain u3
      waitForMigration domain counterName
      assertMigrationSuccessful domain failedCounterName
      pure u3

    -- P4 postgresql: reads are served exclusively from Postgres.
    runCodensity (startDynamicBackend backend (conf "postgresql" False)) $ \_ -> do
      -- Before any login: the cookies recorded in the pure-Cassandra phase
      -- must be listed.  This only passes if the backfill actually decoded
      -- the scan and copied the rows (a login here would mint a fresh cookie
      -- and pass trivially).
      ids1Pg <- readCookieIds u1
      assertBool "cookies created in the Cassandra phase must be backfilled" (all (`elem` ids1Pg) ids1)

      -- Each user's cookies must be served by the pure Postgres interpreter.
      loginAndCheckCookies domain u1
      loginAndCheckCookies domain u2
      loginAndCheckCookies domain u3

      -- Write and read via the pure Postgres interpreter.
      u4 <- randomUser domain def
      loginAndCheckCookies domain u4
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

    failedCounterName :: String
    failedCounterName = "^wire_sessions_migration_failed"

    -- Internal endpoint, no login: returns the cookie ids currently stored for
    -- the user.
    readCookieIds :: (HasCallStack, MakesValue user) => user -> App [Value]
    readCookieIds u = do
      getCookies u [] `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        cookies <- resp.json %. "cookies" >>= asList
        traverse (\c -> c %. "id") cookies

    -- Write-path check: logging in re-issues a session cookie and the list
    -- endpoint must report at least one cookie for the user afterwards.
    loginAndCheckCookies :: (HasCallStack, MakesValue user, MakesValue domain) => domain -> user -> App ()
    loginAndCheckCookies = loginAndCheckCookiesWith id

    loginAndCheckCookiesRetry :: (HasCallStack, MakesValue user, MakesValue domain) => domain -> user -> App ()
    loginAndCheckCookiesRetry = loginAndCheckCookiesWith retryTransient

    loginAndCheckCookiesWith ::
      (HasCallStack, MakesValue user, MakesValue domain) =>
      (App Response -> App Response) ->
      domain ->
      user ->
      App ()
    loginAndCheckCookiesWith wrapLogin domain u = do
      email <- asString (u %. "email")
      wrapLogin (login domain email defPassword) >>= assertSuccess
      ids <- readCookieIds u
      assertBool "expected at least one session cookie" (not (null ids))
