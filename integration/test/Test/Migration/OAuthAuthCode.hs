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

module Test.Migration.OAuthAuthCode where

import API.Brig
import API.BrigInternal
import Control.Monad.Codensity (runCodensity)
import Control.Monad.Reader
import SetupHelpers
import Test.Migration.Util (waitForMigration)
import Testlib.Prelude
import Testlib.ResourcePool

-- | Drive the @oauth_auth_code@ store through the migration lifecycle:
-- cassandra → migration-to-postgresql (worker off / on) → postgresql.
-- Authorization codes are single-use (consume-on-read): every code is
-- exchanged at most once; replays must fail with 404 in every phase.
testOAuthAuthCodeMigration :: (HasCallStack) => App ()
testOAuthAuthCodeMigration = do
  resourcePool <- asks (.resourcePool)

  runCodensity (acquireResources 1 resourcePool) $ \[backend] -> do
    let domain = backend.berDomain
        scopes = ["write-only:conversations"]

    (user, cid, code1u, code1v) <- runCodensity (startDynamicBackend backend (conf "cassandra" False)) $ \_ -> do
      user <- randomUser OwnDomain def
      cid <- createOAuthClient user "oauth-migration" redirectUrl >>= getJSON 200 >>= flip (%.) "client_id"
      -- consumed on cassandra
      code1 <- genCode user cid scopes
      _ <- exchange user cid code1
      bindResponse (createOAuthAccessToken user cid code1 redirectUrl) $ \res ->
        res.status `shouldMatchInt` 404
      -- left unconsumed
      code1u <- genCode user cid scopes
      -- left unconsumed until the postgresql phase: exchanging this code
      -- there proves the background-worker backfill copied it
      code1v <- genCode user cid scopes
      pure (user, cid, code1u, code1v)

    (code2, code2u) <- runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" False)) $ \_ -> do
      -- dual-write: a code created and consumed here is deleted from BOTH stores
      code2 <- genCode user cid scopes
      _ <- exchange user cid code2
      bindResponse (createOAuthAccessToken user cid code2 redirectUrl) $ \res ->
        res.status `shouldMatchInt` 404
      -- codes created before the migration and never consumed still exist
      _ <- exchange user cid code1u
      code2u <- genCode user cid scopes
      pure (code2, code2u)

    code3u <- runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" True)) $ \_ -> do
      code3 <- genCode user cid scopes
      _ <- exchange user cid code3
      bindResponse (createOAuthAccessToken user cid code3 redirectUrl) $ \res ->
        res.status `shouldMatchInt` 404
      waitForMigration domain counterName
      -- left unconsumed: copied to postgresql by the backfill
      code3u <- genCode user cid scopes
      pure code3u

    code4 <- runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" False)) $ \_ -> do
      code4 <- genCode user cid scopes
      _ <- exchange user cid code4
      pure code4

    runCodensity (startDynamicBackend backend (conf "postgresql" False)) $ \_ -> do
      -- a code consumed during the dual-write phase must not have survived in
      -- postgresql
      bindResponse (createOAuthAccessToken user cid code2 redirectUrl) $ \res ->
        res.status `shouldMatchInt` 404
      bindResponse (createOAuthAccessToken user cid code4 redirectUrl) $ \res ->
        res.status `shouldMatchInt` 404
      -- unconsumed codes created before the cutover still work; code1v was
      -- created before the dual-write phase, so redeeming it here proves the
      -- background-worker copied cassandra-era rows
      _ <- exchange user cid code1v
      _ <- exchange user cid code2u
      _ <- exchange user cid code3u
      -- code generation and consumption work on postgresql
      code5 <- genCode user cid scopes
      _ <- exchange user cid code5
      bindResponse (createOAuthAccessToken user cid code5 redirectUrl) $ \res ->
        res.status `shouldMatchInt` 404
  where
    redirectUrl = "https://example.com"

    genCode user cid scopes = do
      res <- generateOAuthAuthorizationCode user cid scopes redirectUrl
      getJSON 201 res >>= flip (%.) "code" & asString

    exchange user cid code = do
      res <- createOAuthAccessToken user cid code redirectUrl
      assertSuccess res
      pure res

-- | Configuration overrides driving one phase of the migration lifecycle.
conf :: String -> Bool -> ServiceOverrides
conf db runMigration =
  def
    { brigCfg =
        setField "postgresMigration.oauthAuthCode" db
          >=> setField "settings.oAuthAuthorizationCodeExpirationTimeSecsInternal" (3600 :: Int),
      backgroundWorkerCfg = setField "migrateOAuthAuthCode" runMigration
    }

counterName :: String
counterName = "^wire_oauth_auth_code_migration_finished"
