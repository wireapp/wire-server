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

module Test.Migration.Budget (testBudgetMigration) where

import API.Brig (login)
import API.Common (defPassword)
import Control.Monad.Codensity
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as BSChar8
import SetupHelpers
import Testlib.Prelude
import Testlib.ResourcePool
import Text.Read (readMaybe)

-- | Drives the login rate-limit (budget) store through the cutover lifecycle
-- (cassandra -> migration-to-postgresql -> postgresql). Budget rows are
-- ephemeral TTL'd counters, so unlike the other store migrations there is no
-- backfill worker: Cassandra rows age out on their own while new writes flow
-- to Postgres. The 60s login-block timeout is what keeps rows alive across
-- backend restarts within this test.
--
-- P2 proves reads stay Cassandra-sourced during dual-write (u1's Cassandra row
-- still blocks). P3 proves a row dual-written under migration-to-postgresql is
-- visible to the pure Postgres interpreter (u2 still blocked), and that a
-- Cassandra-only row does NOT survive cutover (u1 unblocked) — the documented
-- no-backfill behavior.
testBudgetMigration :: (HasCallStack) => App ()
testBudgetMigration = do
  resourcePool <- asks (.resourcePool)
  runCodensity (acquireResources 1 resourcePool) $ \[backend] -> do
    let domain = backend.berDomain

    -- P1 cassandra: budget rows are written only to Cassandra
    u1 <-
      runCodensity (startDynamicBackend backend (conf "cassandra")) $ \_ -> do
        u1 <- randomUser domain def
        exhaustLogins domain u1
        pure u1

    -- P2 migration-to-postgresql: reads still come from Cassandra (source of
    -- truth), writes are mirrored to Postgres
    u2 <-
      runCodensity (startDynamicBackend backend (conf "migration-to-postgresql")) $ \_ -> do
        -- u1 is still blocked: its budget row lives in Cassandra and reads
        -- are served from there during migration.
        assertBlocked domain u1
        u2 <- randomUser domain def
        exhaustLogins domain u2
        pure u2

    -- P3 postgresql: reads and writes are served exclusively from Postgres
    runCodensity (startDynamicBackend backend (conf "postgresql")) $ \_ -> do
      -- u2 is still blocked: the dual-written row is visible to the
      -- Postgres-only interpreter.
      assertBlocked domain u2
      -- u1 is NOT blocked: its budget row only ever existed in Cassandra and
      -- was never backfilled (no backfill worker for ephemeral data).
      assertUnblocked domain u1
      -- Write and read via the pure Postgres interpreter.
      u3 <- randomUser domain def
      exhaustLogins domain u3
  where
    conf db =
      def
        { brigCfg =
            setField @_ @Int "optSettings.setLimitFailedLogins.timeout" 60
              >=> setField @_ @Int "optSettings.setPasswordHashingRateLimit.userLimit.inverseRate" 0
              >=> setField "postgresMigration.budget" db
        }

-- | Exhaust the failed-login budget of a user: 5 wrong passwords get plain
-- login-failure responses, and the 6th attempt (even with the correct
-- password) is blocked with a @Retry-After@ header.
exhaustLogins :: (HasCallStack) => String -> Value -> App ()
exhaustLogins domain u = do
  email <- asString (u %. "email")
  for_ [1 :: Int .. 5] $ \_ ->
    login domain email "wrong-password" `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 403
      resp.json %. "label" `shouldMatch` "invalid-credentials"
  assertBlockedEmail domain email

assertBlocked :: (HasCallStack) => String -> Value -> App ()
assertBlocked domain u = asString (u %. "email") >>= assertBlockedEmail domain

assertBlockedEmail :: (HasCallStack) => String -> String -> App ()
assertBlockedEmail domain email =
  login domain email defPassword `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "client-error"
    -- The Retry-After header distinguishes LoginBlocked from a plain
    -- login-failure.
    let Just (_retryAfter :: Int) = readMaybe . BSChar8.unpack =<< lookup (fromString "Retry-After") resp.headers
    pure ()

assertUnblocked :: (HasCallStack) => String -> Value -> App ()
assertUnblocked domain u = do
  email <- asString (u %. "email")
  login domain email defPassword >>= assertSuccess
