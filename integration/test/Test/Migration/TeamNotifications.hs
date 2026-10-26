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

module Test.Migration.TeamNotifications where

import API.Brig
import API.BrigInternal (getInvitationCode)
import API.Common (defPassword)
import API.Galley
import Control.Monad.Codensity
import Control.Monad.Reader
import SetupHelpers
import Test.Migration.Util (waitForMigration)
import Testlib.Prelude
import Testlib.ResourcePool

testTeamNotificationsMigration :: (HasCallStack) => App ()
testTeamNotificationsMigration = do
  resourcePool <- asks (.resourcePool)
  runCodensity (acquireResources 1 resourcePool) $ \[backend] -> do
    let domain = backend.berDomain

    -- Phase 1: cassandra. The team starts with owner + 1 member; the member
    -- join produced a team notification.
    (owner, count1, sinceId) <- runCodensity (startDynamicBackend backend (conf "cassandra" False)) $ \_ -> do
      (owner, _, _) <- createTeam domain 2
      (count1, sinceId) <- fetchCountAndLastId owner Nothing
      pure (owner, count1, sinceId)

    -- Phase 2: migration-to-postgresql, worker off. Writes are mirrored to
    -- Postgres; reads still come from Cassandra.
    count2 <- runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" False)) $ \_ -> do
      joinNewMember owner domain
      (count2, _) <- fetchCountAndLastId owner Nothing
      assertBool "notification count did not grow" (count2 > count1)
      pure count2

    -- Phase 3: migration-to-postgresql, worker on. The copy loop replicates
    -- the Cassandra rows written in earlier phases.
    count3 <- runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" True)) $ \_ -> do
      joinNewMember owner domain
      waitForMigration domain counterName
      (count3, _) <- fetchCountAndLastId owner Nothing
      assertBool "notification count did not grow" (count3 > count2)
      pure count3

    -- Phase 4: postgresql. All notifications (copied + mirrored) are visible;
    -- 'since' reads are inclusive of the since row itself.
    runCodensity (startDynamicBackend backend (conf "postgresql" False)) $ \_ -> do
      joinNewMember owner domain
      (count4, _) <- fetchCountAndLastId owner Nothing
      assertBool "notification count did not grow" (count4 > count3)
      getTeamNotifications owner (Just sinceId) `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        -- the first notification is the one matching the since id
        resp.json %. "notifications.0.id" `shouldMatch` sinceId
        resp.json %. "notifications.1.payload.0.type" `shouldMatch` "team.member-join"
  where
    fetchCountAndLastId user mSince =
      getTeamNotifications user mSince `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        count <- length <$> (resp.json %. "notifications" >>= asList)
        lastId <- resp.json %. "notifications.-1.id" & asString
        pure (count, lastId)

    joinNewMember owner domain = do
      user <- randomUser domain def
      email <- user %. "email" >>= asString
      inv <- postInvitation owner (PostInvitation (Just email) Nothing) >>= getJSON 201
      code <- getInvitationCode owner inv >>= getJSON 200 >>= (%. "code") & asString
      acceptTeamInvitation user code (Just defPassword) >>= assertSuccess

conf :: String -> Bool -> ServiceOverrides
conf db runMigration =
  def
    { galleyCfg = setField "postgresMigration.teamNotifications" db,
      backgroundWorkerCfg = setField "migrateTeamNotifications" runMigration
    }

counterName :: String
counterName = "^wire_team_notifications_migration_finished"
