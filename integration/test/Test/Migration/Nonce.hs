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

module Test.Migration.Nonce (testNonceMigration) where

import API.Brig
import qualified API.BrigInternal as BrigInternal
import API.Common
import Control.Monad.Codensity
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as BC
import qualified Data.CaseInsensitive as CI
import SetupHelpers (createTeam)
import Test.Migration.Util (waitForMigration)
import Testlib.Prelude
import Testlib.ResourcePool

-- | Migrate the 'nonce' store (brig, DPoP replay protection) from Cassandra to
-- PostgreSQL.
--
-- A nonce is issued on GET /clients/<cid>/nonce and consumed (lookup-and-
-- delete) on the first access-token request. A replayed attempt must be
-- rejected with 400 client-token-bad-nonce. This is verified in all three
-- storage modes; the postgresql leg is the load-bearing proof that consume
-- works with PostgreSQL as the sole source of truth.
testNonceMigration :: (HasCallStack) => App ()
testNonceMigration = do
  resourcePool <- asks (.resourcePool)
  runCodensity (acquireResources 1 resourcePool) $ \[backend] -> do
    let domain = backend.berDomain

        -- Issue a nonce, consume it with a garbage DPoP proof (the nonce is
        -- looked up and deleted before the proof is verified, so token
        -- generation fails with 400), then verify the replay is rejected.
        checkNonceRoundtrip = do
          (owner, _tid, _members) <- createTeam domain 1
          assertSuccess =<< putSelf owner (PutSelf Nothing Nothing (Just "nonce-owner") Nothing)
          handle <- randomHandle
          assertSuccess =<< putHandle owner handle
          client <- bindResponse (addClient owner def) $ getJSON 201
          cid <- client %. "id" & asString

          nonceResp <- BrigInternal.newNonce owner cid
          assertStatus 204 nonceResp
          assertBool "missing Replay-Nonce header"
            $ isJust (findHeader (CI.mk (BC.pack "Replay-Nonce")) nonceResp)

          -- First access-token attempt: nonce consumed, proof verification fails.
          assertStatus 400 =<< BrigInternal.createDPoPAccessToken owner cid

          -- Replay: nonce is gone, must be rejected as unacceptable.
          replayResp <- BrigInternal.createDPoPAccessToken owner cid
          assertStatus 400 replayResp
          replayResp.json %. "label" `shouldMatch` "client-token-bad-nonce"

    -- Cassandra: issue and consume a nonce.
    runCodensity (startDynamicBackend backend (conf "cassandra" False)) $ const checkNonceRoundtrip

    -- migration-to-postgresql with the worker running: backfill and confirm
    -- the consume path keeps working.
    runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" True)) $ \_ -> do
      waitForMigration domain counterName
      checkNonceRoundtrip

    -- PostgreSQL only: consume works with PostgreSQL as the source of truth.
    runCodensity (startDynamicBackend backend (conf "postgresql" False)) $ const checkNonceRoundtrip
  where
    conf :: String -> Bool -> ServiceOverrides
    conf db runMigration =
      def
        { brigCfg = setField "postgresMigration.nonce" db,
          backgroundWorkerCfg =
            setField "postgresMigration.nonce" db
              >=> setField "migrateNonce" runMigration
        }

    counterName :: String
    counterName = "^wire_nonce_migration_finished"
