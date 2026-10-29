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

module Test.Migration.ScimUserMeta (testScimUserMetaMigration) where

import API.Common
import API.Spar
import Control.Monad.Codensity
import Control.Monad.Reader
import SetupHelpers (createTeam, randomScimUserWithEmail)
import Test.Migration.Util (waitForMigration)
import Testlib.Prelude
import Testlib.ResourcePool

-- | Migrate spar's @scim user meta@ store from Cassandra to PostgreSQL.
--
-- A SCIM user is created while Cassandra is the source of truth; its metadata
-- (created/lastModified and email type/primary) is echoed by the SCIM API. The
-- background worker then backfills the store into PostgreSQL, and after the
-- cutover an update goes through the PostgreSQL write path and the metadata
-- survives.
testScimUserMetaMigration :: (HasCallStack) => App ()
testScimUserMetaMigration = do
  resourcePool <- asks (.resourcePool)
  runCodensity (acquireResources 1 resourcePool) $ \[backend] -> do
    let domain = backend.berDomain

    -- Cassandra: create a team, a scim token and a scim user; the metadata
    -- must be echoed on read.
    (owner, bearer, tokenId, scid, externalId) <- runCodensity (startDynamicBackend backend (conf "cassandra" False)) $ \_ -> do
      (owner, _tid, _) <- createTeam domain 1
      tokenResponse <- createScimToken owner def >>= getJSON 200
      bearer <- tokenResponse %. "token" >>= asString
      externalId <- randomExternalId
      scimUser <- randomScimUserWithEmail externalId =<< randomEmail
      tokenId <- tokenResponse %. "id" >>= asString
      scid <- createScimUser domain bearer scimUser >>= getJSON 201 >>= (%. "id") >>= asString
      _ <- getScimUser domain bearer scid >>= getJSON 200 >>= (%. "meta.created") >>= asString
      pure (owner, bearer, tokenId, scid, externalId)

    -- migration-to-postgresql with the worker running: backfill the store and
    -- confirm the data is still served (reads remain on Cassandra here).
    runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" True)) $ \_ -> do
      waitForMigration domain counterName
      assertSuccess =<< getScimUser domain bearer scid

    -- PostgreSQL only: an update goes through the PostgreSQL write path, and
    -- the metadata is still served.
    runCodensity (startDynamicBackend backend (conf "postgresql" False)) $ \_ -> do
      displayName <- randomName
      scimUser <- randomScimUserWithEmail externalId =<< randomEmail
      updatedUser <- setField "displayName" displayName scimUser
      assertSuccess =<< updateScimUser domain bearer scid updatedUser
      resp <- getScimUser domain bearer scid >>= getJSON 200
      displayName' <- resp %. "displayName" >>= asString
      displayName' `shouldMatch` displayName
      assertSuccess =<< deleteScimUser domain bearer scid
      assertSuccess =<< deleteScimToken owner tokenId
  where
    conf :: String -> Bool -> ServiceOverrides
    conf db runMigration =
      def
        { sparCfg =
            setField "postgresMigration.scimUserMeta" db,
          backgroundWorkerCfg =
            setField "postgresMigration.scimUserMeta" db
              >=> setField "migrateScimUserMeta" runMigration
        }

    counterName :: String
    counterName = "^wire_scim_user_meta_migration_finished"
