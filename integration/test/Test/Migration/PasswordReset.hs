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

module Test.Migration.PasswordReset (testPasswordResetMigration) where

import API.Brig
import API.BrigInternal (getPasswordResetCode)
import qualified API.BrigInternal as BI
import API.Common (defPassword)
import Control.Monad.Codensity
import Control.Monad.Reader
import SetupHelpers
import Test.Migration.Util (waitForMigration)
import Testlib.Prelude
import Testlib.ResourcePool

-- | Drives the password-reset store through the full cutover lifecycle
-- (cassandra -> migration-to-postgresql -> postgresql). Reset keys are
-- deterministic per user and a second reset for the same user is a silent
-- no-op, so every phase writes with a fresh user and reads (and verifies) the
-- write of the previous phase. This exercises the writes of all three
-- interpreters (Cassandra, dual-write, Postgres) as well as their reads.
--
-- The rows created before the cutover (u1: pure Cassandra write, u2/u3:
-- dual-write) must be served by the pure-Postgres interpreter afterwards — u1
-- proves the background worker backfilled a row that only ever existed in
-- Cassandra.
--
-- u4 also covers the retry edge cases: a wrong code decrements the remaining
-- retries (and refreshes the row's expiry), and exhausting the retries deletes
-- the row, after which even the correct code is rejected and the password is
-- unchanged.
testPasswordResetMigration :: (HasCallStack) => App ()
testPasswordResetMigration = do
  resourcePool <- asks (.resourcePool)
  runCodensity (acquireResources 1 resourcePool) $ \[backend] -> do
    let domain = backend.berDomain

    -- P1 cassandra: write via the pure Cassandra interpreter
    u1 <-
      runCodensity (startDynamicBackend backend (conf "cassandra" False)) $ \_ ->
        initiateReset domain

    -- P2 migration-to-postgresql (worker off): reads still come from
    -- Cassandra, writes go to both stores
    u2 <-
      runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" False)) $ \_ -> do
        checkCode domain u1
        initiateReset domain

    -- P3 migration-to-postgresql (worker on): dual-write while backfilling
    u3 <-
      runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" True)) $ \_ -> do
        checkCode domain u2
        -- A wrong code decrements the retries and re-inserts the row (with a
        -- refreshed expiry); the API rejects it with a 400.
        completePasswordReset domain u2.key (u2.code <> "X") "some-password" >>= assertStatus 400
        initiateReset domain >>= \u3' -> do
          waitForMigration domain counterName
          pure u3'

    -- P4 postgresql: reads are served exclusively from Postgres
    runCodensity (startDynamicBackend backend (conf "postgresql" False)) $ \_ -> do
      -- Rows written by every interpreter are visible to the pure Postgres
      -- interpreter.
      checkCode domain u1
      checkCode domain u2
      checkCode domain u3

      -- The decremented row from P3 still completes the flow.
      completePasswordReset domain u2.key u2.code "shiny-new-password" >>= assertSuccess
      login domain u2.email "shiny-new-password" >>= assertSuccess

      -- Write and read via the pure Postgres interpreter.
      u4 <- initiateReset domain
      checkCode domain u4

      -- Retry depletion: three wrong codes exhaust the retries (3 -> 2 -> 1
      -- -> deleted). Afterwards even the correct code is rejected and the
      -- password is unchanged.
      for_ [1 :: Int .. 3] $ \_ ->
        completePasswordReset domain u4.key (u4.code <> "X") "some-password" >>= assertStatus 400
      getPasswordResetCode domain u4.email >>= assertStatus 400
      completePasswordReset domain u4.key u4.code "shiny-new-password" >>= assertStatus 400
      login domain u4.email defPassword >>= assertSuccess
  where
    conf db runMigration =
      def
        { brigCfg = setField "postgresMigration.passwordReset" db,
          backgroundWorkerCfg =
            setField "postgresMigration.passwordReset" db
              >=> setField "migratePasswordReset" runMigration
        }
    counterName = "^wire_password_reset_migration_finished"

-- | A user together with the reset data of an initiated password reset.
data ResetUser = ResetUser
  { email :: String,
    key :: String,
    code :: String
  }

-- | Create a fresh user (with a known password) and initiate a password reset
-- for it, returning the reset key and code. Reset keys are deterministic per
-- user and a second reset for the same user is a silent no-op, which is why
-- every phase-write uses a fresh user.
initiateReset :: (HasCallStack) => String -> App ResetUser
initiateReset domain = do
  user <- randomUser domain def {BI.password = Just defPassword}
  email <- user %. "email" & asString
  passwordReset domain email >>= assertSuccess
  (key, code) <- getResetData domain email
  pure ResetUser {email = email, key = key, code = code}

getResetData :: (HasCallStack) => String -> String -> App (String, String)
getResetData domain email =
  bindResponse (getPasswordResetCode domain email) $ \resp -> do
    resp.status `shouldMatchInt` 200
    (,) <$> (resp.json %. "key" & asString) <*> (resp.json %. "code" & asString)

-- | The stored reset code is still readable and unchanged.
checkCode :: (HasCallStack) => String -> ResetUser -> App ()
checkCode domain u = do
  (key, code) <- getResetData domain u.email
  key `shouldMatch` u.key
  code `shouldMatch` u.code
