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

module Test.Migration.Team where

import API.Galley
import Control.Monad.Codensity
import Control.Monad.Reader
import SetupHelpers
import Test.Migration.Util (waitForMigration)
import Testlib.Prelude
import Testlib.ResourcePool

testTeamMigration :: (HasCallStack) => App ()
testTeamMigration = do
  resourcePool <- asks (.resourcePool)
  runCodensity (acquireResources 1 resourcePool) $ \[backend] -> do
    let domain = backend.berDomain

    -- cassandra: team metadata is written to and read from Cassandra
    (admin, tid1) <- runCodensity (startDynamicBackend backend (conf "cassandra" False)) $ \_ -> do
      (admin, tid1, _) <- createTeam domain 2
      checkTeamRoundtrip admin tid1
      pure (admin, tid1)

    -- migration-to-postgresql (worker off): new teams are mirrored to Postgres
    tid2 <- runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" False)) $ \_ -> do
      checkTeamRoundtrip admin tid1
      (_, tid2, _) <- createTeam domain 1
      pure tid2

    -- migration-to-postgresql (worker on): the copy loop backfills the team
    -- table, including teams that predate the migration setting
    tid3 <- runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" True)) $ \_ -> do
      checkTeamRoundtrip admin tid1
      checkTeamRoundtrip admin tid2
      waitForMigration domain counterName
      (_, tid3, _) <- createTeam domain 1
      pure tid3

    -- postgresql: reads come from Postgres for all teams, old and new
    runCodensity (startDynamicBackend backend (conf "postgresql" False)) $ \_ -> do
      checkTeamRoundtrip admin tid1
      checkTeamRoundtrip admin tid2
      checkTeamRoundtrip admin tid3
      (_, tid4, _) <- createTeam domain 1
      checkTeamRoundtrip admin tid4
  where
    checkTeamRoundtrip admin tid =
      bindResponse (getTeam admin tid) $ \res -> do
        res.status `shouldMatchInt` 200
        tidStr <- asString tid
        res.json %. "id" `shouldMatch` tidStr

conf :: String -> Bool -> ServiceOverrides
conf db runMigration =
  def
    { galleyCfg = setField "postgresMigration.team" db,
      backgroundWorkerCfg = setField "migrateTeam" runMigration
    }

counterName :: String
counterName = "^wire_team_migration_finished"
