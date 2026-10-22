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

module Test.Migration.TeamMembers where

import API.Galley
import Control.Monad.Codensity
import Control.Monad.Reader
import SetupHelpers
import Test.Migration.Util (retryTransient, waitForMigration)
import Testlib.Prelude
import Testlib.ResourcePool

testTeamMembersMigration :: (HasCallStack) => App ()
testTeamMembersMigration = do
  resourcePool <- asks (.resourcePool)
  runCodensity (acquireResources 1 resourcePool) $ \[backend] -> do
    let domain = backend.berDomain

    -- cassandra: member rows live in Cassandra
    (admin1, tid1, mems1) <- runCodensity (startDynamicBackend backend (conf "cassandra" False)) $ \_ -> do
      r@(admin1, tid1, mems) <- createTeam domain 2
      checkMembersListed admin1 tid1 (admin1 : mems)
      pure r

    -- migration-to-postgresql (worker off): member writes go to both stores
    (admin2, tid2, mems2) <- runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" False)) $ \_ -> do
      checkMembersListed admin1 tid1 (admin1 : mems1)
      (admin2, tid2, mems) <- createTeam domain 2
      checkMembersListed admin2 tid2 (admin2 : mems)
      m : _ <- pure mems
      assertSuccess =<< updateTeamMember tid2 admin2 m Admin
      pure (admin2, tid2, mems)

    -- migration-to-postgresql (worker on): the copy loop migrates all member
    -- rows, including teams created before the migration setting
    (admin3, tid3) <- runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" True)) $ \_ -> do
      checkMembersListed admin1 tid1 (admin1 : mems1)
      checkMembersListed admin2 tid2 (admin2 : mems2)
      waitForMigration domain counterName
      (admin3, tid3, _) <- createTeam domain 2
      pure (admin3, tid3)

    -- postgresql: member reads and writes come from Postgres for all teams,
    -- old and new
    runCodensity (startDynamicBackend backend (conf "postgresql" False)) $ \_ -> do
      checkMembersListed admin1 tid1 (admin1 : mems1)
      checkMembersListed admin2 tid2 (admin2 : mems2)
      checkMembersListed admin3 tid3 [admin3]
      (admin4, tid4, mems4) <- createTeam domain 2
      m : _ <- pure mems4
      assertSuccess =<< updateTeamMember tid4 admin4 m Admin
      assertSuccess =<< deleteTeamMember tid4 admin4 m
      retryTransient (getTeamMember admin4 tid4 m) `bindResponse` \resp ->
        resp.status `shouldMatchInt` 404
  where
    checkMembersListed admin tid expected = do
      uids <- for expected ((%. "id") >=> asString)
      retryTransient (getTeamMembers admin tid) `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        listed <- resp.json %. "members" >>= asList >>= traverse (%. "user")
        listedIds <- traverse asString listed
        for_ uids $ \uid -> length (filter (== uid) listedIds) `shouldMatchInt` 1

conf :: String -> Bool -> ServiceOverrides
conf db runMigration =
  def
    { galleyCfg = setField "postgresMigration.team" db,
      backgroundWorkerCfg = setField "migrateTeamMembers" runMigration
    }

counterName :: String
counterName = "^wire_team_members_migration_finished"
