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

module Test.Migration.FederationConfig (testFederationConfigMigration) where

import qualified API.BrigInternal as BrigInternal
import Control.Monad.Codensity
import Control.Monad.Reader
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Test.Migration.Util (waitForMigration)
import Testlib.Prelude
import Testlib.ResourcePool

-- | Migrate the 'federation_remotes' store (brig) from Cassandra to PostgreSQL.
--
-- A remote domain with a search policy and a by-team restriction is created in
-- Cassandra; it must survive the copy loop and remain updatable once
-- PostgreSQL is the sole source of truth.  The remote domain must not be
-- @example.com@: the static config file shadows database rows for that domain.
testFederationConfigMigration :: (HasCallStack) => App ()
testFederationConfigMigration = do
  resourcePool <- asks (.resourcePool)
  teamId <- UUID.toString <$> liftIO UUID.nextRandom
  runCodensity (acquireResources 1 resourcePool) $ \[backend] -> do
    let domain = backend.berDomain
        remote = BrigInternal.FedConn "remote.example.com" "full_search" (Just [teamId])
        remote' = remote {BrigInternal.searchStrategy = "no_search", BrigInternal.restriction = Nothing}

    -- Cassandra: create the remote with a by-team restriction and confirm it
    -- is reported as such.
    runCodensity (startDynamicBackend backend (conf "cassandra" False)) $ \_ -> do
      _ <- BrigInternal.createFedConn domain remote
      assertRemoteListed domain remote
      BrigInternal.addFederationRemoteTeam domain remote.domain teamId

    -- migration-to-postgresql with the worker running: backfill the remote and
    -- confirm it is still listed once the migration is finished.
    runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" True)) $ \_ -> do
      waitForMigration domain counterName
      assertRemoteListed domain remote

    -- PostgreSQL only: the migrated remote must persist, remain updatable
    -- (exercising the LWT -> guarded-update translation), and its team
    -- restriction must be removable.
    runCodensity (startDynamicBackend backend (conf "postgresql" False)) $ \_ -> do
      assertRemoteListed domain remote
      _ <- BrigInternal.updateFedConn domain remote.domain remote'
      assertRemoteListed domain remote'
      BrigInternal.deleteFederationRemoteTeam domain remote.domain teamId
  where
    conf :: String -> Bool -> ServiceOverrides
    conf db runMigration =
      def
        { brigCfg = setField "postgresMigration.federationConfig" db,
          backgroundWorkerCfg =
            setField "postgresMigration.federationConfig" db
              >=> setField "migrateFederationConfig" runMigration
        }

    counterName :: String
    counterName = "^wire_federation_config_migration_finished"

    assertRemoteListed :: (HasCallStack, MakesValue domain) => domain -> BrigInternal.FedConn -> App ()
    assertRemoteListed domain want = do
      remotes <-
        bindResponse (BrigInternal.readFedConns domain) $ \res -> do
          res.status `shouldMatchInt` 200
          res.json %. "remotes" & asList
      remotes `shouldMatch` [want]
