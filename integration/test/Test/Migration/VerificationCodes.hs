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

module Test.Migration.VerificationCodes (testVerificationCodesMigration) where

import API.BrigInternal (getVerificationCode)
import API.Common (defPassword)
import API.GalleyInternal (generateVerificationCode, setTeamFeatureLockStatus, setTeamFeatureStatus)
import API.Nginz (loginWith2ndFactor)
import Control.Concurrent (threadDelay)
import Control.Monad.Codensity
import Control.Monad.Reader
import qualified Data.Aeson as Aeson
import SetupHelpers
import Test.Migration.Util (waitForMigration)
import Testlib.Prelude
import Testlib.ResourcePool
import Text.Printf (printf)

-- | Drives the verification-code store through the full cutover lifecycle
-- (cassandra -> migration-to-postgresql -> postgresql). Each phase creates a
-- fresh team and generates a login verification code for its owner; codes are
-- left unconsumed so later phases can read them back unchanged, which
-- exercises the writes and reads of all three interpreters (Cassandra,
-- dual-write, Postgres).
--
-- The code created before the cutover (owner1: pure Cassandra write) must be
-- served by the pure-Postgres interpreter afterwards — owner1 proves the
-- background worker backfilled a row that only ever existed in Cassandra.
testVerificationCodesMigration :: (HasCallStack) => App ()
testVerificationCodesMigration = do
  resourcePool <- asks (.resourcePool)
  runCodensity (acquireResources 1 resourcePool) $ \[backend] -> do
    let domain = backend.berDomain

        conf db runMigration =
          def
            { brigCfg = setField "postgresMigration.verificationCodes" db,
              backgroundWorkerCfg =
                setField "postgresMigration.verificationCodes" db
                  >=> setField "migrateVerificationCodes" runMigration
            }

        confExpired =
          def
            { brigCfg =
                setField "postgresMigration.verificationCodes" "postgresql"
                  >=> setField "optSettings.setVerificationTimeout" (Aeson.Number 2)
            }

        counterName = "^wire_verification_codes_migration_finished"

    -- P1 cassandra: write and read via the pure Cassandra interpreter
    (owner1, code1) <-
      runCodensity (startDynamicBackend backend (conf "cassandra" False)) $ \_ -> do
        owner <- setupCode domain
        code <- getCode owner
        pure (owner, code)

    -- P2 migration-to-postgresql (worker off): reads still come from
    -- Cassandra, writes go to both stores
    (owner2, code2) <-
      runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" False)) $ \_ -> do
        assertCode owner1 code1
        owner <- setupCode domain
        code <- getCode owner
        pure (owner, code)

    -- P3 migration-to-postgresql (worker on): dual-write while backfilling
    (owner3, code2', code3) <-
      runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" True)) $ \_ -> do
        assertCode owner2 code2
        -- A wrong second-factor code decrements the remaining retries (a
        -- write through the dual-write interpreter) and login fails.
        wrongCode code2 >>= \bad ->
          bindResponse ((owner2 %. "email") >>= \email -> loginWith2ndFactor domain email defPassword bad) $ \resp -> do
            resp.status `shouldMatchInt` 403
            resp.json %. "label" `shouldMatch` "code-authentication-failed"
        -- Re-generate (createCodeOverwritePrevious) so owner2's stored code is
        -- fresh, then read back the new value through the dual-write path.
        regenerateCode owner2
        code2new <- getCode owner2
        waitForMigration domain counterName
        owner <- setupCode domain
        code <- getCode owner
        pure (owner, code2new, code)

    -- P4 postgresql: reads are served exclusively from Postgres
    runCodensity (startDynamicBackend backend (conf "postgresql" False)) $ \_ -> do
      -- Rows written by every interpreter are visible to the pure Postgres
      -- interpreter; owner1's row only ever existed in Cassandra.
      assertCode owner1 code1
      assertCode owner2 code2'
      assertCode owner3 code3
      -- Verify via the pure Postgres interpreter.
      (owner2 %. "email") >>= \email -> loginWith2ndFactor domain email defPassword code2' >>= assertStatus 200

    -- P5 postgresql with a short verification timeout: an expired code must
    -- vanish for the Postgres interpreter (the expires_at guard reproduces
    -- Cassandra's vanished-row semantics) instead of being served with a
    -- stale remaining lifetime.
    runCodensity (startDynamicBackend backend confExpired) $ \_ -> do
      owner5 <- setupCode domain
      code5 <- getCode owner5
      liftIO $ threadDelay 2_100_000
      getVerificationCode owner5 "login" >>= assertStatus 404
      bindResponse ((owner5 %. "email") >>= \email -> loginWith2ndFactor domain email defPassword code5) $ \resp -> do
        resp.status `shouldMatchInt` 403
        resp.json %. "label" `shouldMatch` "code-authentication-failed"

-- | Create a fresh team, enable the second-factor challenge feature for it,
-- and generate a login verification code for the team owner.
setupCode :: (HasCallStack) => String -> App Value
setupCode domain = do
  (owner, team, []) <- createTeam domain 0
  email <- owner %. "email"
  setTeamFeatureLockStatus owner team "sndFactorPasswordChallenge" "unlocked"
  assertSuccess =<< setTeamFeatureStatus owner team "sndFactorPasswordChallenge" "enabled"
  generateVerificationCode owner email
  pure owner

-- | Read the login verification code stored for a user.
getCode :: (HasCallStack) => Value -> App String
getCode owner = do
  code <- getVerificationCode owner "login" >>= getJSON 200
  asString code

assertCode :: (HasCallStack) => Value -> String -> App ()
assertCode owner expected = do
  code <- getCode owner
  code `shouldMatch` expected

regenerateCode :: (HasCallStack) => Value -> App ()
regenerateCode owner = do
  email <- owner %. "email"
  generateVerificationCode owner email

wrongCode :: (HasCallStack) => String -> App String
wrongCode correct = do
  let n = (read @Int correct + 1) `mod` 1000000
  pure $ printf "%06d" n
