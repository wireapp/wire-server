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

module Test.Migration.ScimStores (testScimStoresMigration) where

import API.Common
import API.Spar
import Control.Monad.Codensity
import Control.Monad.Reader
import SetupHelpers (createTeam, randomScimUserWithEmail)
import Test.Migration.Util (waitForMigration)
import Testlib.Prelude
import Testlib.ResourcePool

-- | Migrate spar's @scim token@ and @scim external id@ stores from Cassandra
-- to PostgreSQL.
--
-- A SCIM token and a SCIM user (keyed by an external id) are created while
-- Cassandra is the source of truth. The background worker then backfills both
-- stores into PostgreSQL, and after the cutover the token is listed, renameable
-- and deletable, and the SCIM user is still resolvable via its external id.
testScimStoresMigration :: (HasCallStack) => App ()
testScimStoresMigration = do
  resourcePool <- asks (.resourcePool)
  runCodensity (acquireResources 1 resourcePool) $ \[backend] -> do
    let domain = backend.berDomain

    -- Cassandra: create a team, a scim token and a scim user; both must resolve.
    (owner, bearer, tokenId, scid, externalId) <- runCodensity (startDynamicBackend backend (conf "cassandra" False)) $ \_ -> do
      (owner, _tid, _) <- createTeam domain 1
      tokenResponse <- createScimToken owner def >>= getJSON 200
      bearer <- tokenResponse %. "token" >>= asString
      tokenId <- tokenResponse %. "id" >>= asString
      tokens <- getScimTokens owner >>= getJSON 200 >>= asList
      assertBool "expected exactly one scim token" (length tokens == 1)
      externalId <- randomExternalId
      scimUser <- randomScimUserWithEmail externalId =<< randomEmail
      scid <- createScimUser domain bearer scimUser >>= getJSON 201 >>= (%. "id") >>= asString
      assertSuccess =<< findUsersByExternalId domain bearer externalId
      pure (owner, bearer, tokenId, scid, externalId)

    -- migration-to-postgresql with the worker running: backfill both stores and
    -- confirm the data is still served (reads remain on Cassandra here).
    runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" True)) $ \_ -> do
      waitForMigration domain tokensCounterName
      waitForMigration domain externalIdsCounterName
      tokens <- getScimTokens owner >>= getJSON 200 >>= asList
      assertBool "scim token did not survive the migration" (length tokens == 1)
      assertSuccess =<< findUsersByExternalId domain bearer externalId

    -- PostgreSQL only: the token and the external-id mapping must be served
    -- from PostgreSQL, and writes must work.
    runCodensity (startDynamicBackend backend (conf "postgresql" False)) $ \_ -> do
      assertSuccess =<< putScimTokenName owner tokenId "renamed"
      tokens <- getScimTokens owner >>= getJSON 200 >>= asList
      assertBool "expected exactly one scim token" (length tokens == 1)
      name <- head tokens %. "name" >>= asString
      name `shouldMatch` "renamed"
      assertSuccess =<< findUsersByExternalId domain bearer externalId
      assertSuccess =<< deleteScimUser domain bearer scid
      assertSuccess =<< deleteScimToken owner tokenId
      tokens' <- getScimTokens owner >>= getJSON 200 >>= asList
      assertBool "scim token was not deleted" (null tokens')
  where
    conf :: String -> Bool -> ServiceOverrides
    conf db runMigration =
      def
        { sparCfg =
            setField "postgresMigration.scimTokens" db
              >=> setField "postgresMigration.scimExternalIds" db,
          backgroundWorkerCfg =
            setField "postgresMigration.scimTokens" db
              >=> setField "postgresMigration.scimExternalIds" db
              >=> setField "migrateScimTokens" runMigration
              >=> setField "migrateScimExternalIds" runMigration
        }

    tokensCounterName :: String
    tokensCounterName = "^wire_scim_tokens_migration_finished"

    externalIdsCounterName :: String
    externalIdsCounterName = "^wire_scim_external_ids_migration_finished"
