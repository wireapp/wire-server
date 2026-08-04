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
import Control.Monad.Codensity
import Control.Monad.Reader
import SetupHelpers
import Test.Migration.Util (waitForMigration)
import Testlib.Prelude
import Testlib.ResourcePool

-- | Drives the password-reset store through the full cutover lifecycle
-- (cassandra -> migration-to-postgresql -> postgresql). A reset code written to
-- Cassandra before migration must be served from Postgres after the cutover and
-- still complete the reset, proving the row was backfilled.
testPasswordResetMigration :: (HasCallStack) => App ()
testPasswordResetMigration = do
  resourcePool <- asks (.resourcePool)
  runCodensity (acquireResources 1 resourcePool) $ \[backend] -> do
    let domain = backend.berDomain

    -- Cassandra: create a user and initiate a password reset (writes the code to Cassandra)
    (email, key, code) <-
      runCodensity (startDynamicBackend backend (conf "cassandra" False)) $ \_ -> do
        user <- randomUser domain def
        email <- user %. "email" & asString
        passwordReset domain email >>= assertSuccess
        getResetData domain email

    -- migration-to-postgresql (worker off): the code is still served from Cassandra
    runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" False)) $ \_ ->
      checkCode domain email key code

    -- migration-to-postgresql (worker on): backfill the code to Postgres and wait for completion
    runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" True)) $ \_ ->
      waitForMigration domain counterName

    -- postgresql: the migrated code is served from Postgres and completes the reset
    runCodensity (startDynamicBackend backend (conf "postgresql" False)) $ \_ -> do
      checkCode domain email key code
      let newPassword = "shiny-new-password"
      completePasswordReset domain key code newPassword >>= assertSuccess
      login domain email newPassword >>= assertSuccess
  where
    conf db runMigration =
      def
        { galleyCfg = setField "postgresMigration.passwordReset" db,
          backgroundWorkerCfg = setField "migratePasswordReset" runMigration
        }
    counterName = "^wire_password_reset_migration_finished"
    getResetData dom email =
      bindResponse (getPasswordResetCode dom email) $ \resp -> do
        resp.status `shouldMatchInt` 200
        (,) <$> (resp.json %. "key" & asString) <*> (resp.json %. "code" & asString)
    checkCode dom email key code = do
      (key', code') <- getResetData dom email
      key' `shouldMatch` key
      code' `shouldMatch` code
