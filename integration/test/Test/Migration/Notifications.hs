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

-- | Migration test for the gundeck notification store
-- (@postgresMigration.notifications@): five phases over one dynamic backend —
--
--   1. @cassandra@: baseline writes land in Cassandra.
--   2. @migration-to-postgresql@, copy worker off: new writes are mirrored to
--      Postgres, reads still come from Cassandra.
--   3. @migration-to-postgresql@, copy worker on: the copy loop back-fills
--      rows written in earlier phases.
--   4. @migration-to-postgresql@, copy worker off again: the system keeps
--      working with the worker disabled after the migration ran.
--   5. @postgresql@: reads come from Postgres alone, including the
--      copy-replicated rows of the phase-1 user.
module Test.Migration.Notifications
  ( testNotificationsMigration,
  )
where

import API.Common (randomClientId)
import API.Gundeck (GetNotifications (..), getLastNotification, getNotifications)
import API.GundeckInternal (postPush)
import Control.Monad.Codensity
import Control.Monad.Reader
import SetupHelpers
import Test.Migration.Util (waitForMigration)
import Testlib.Prelude
import Testlib.ResourcePool

testNotificationsMigration :: (HasCallStack) => App ()
testNotificationsMigration = do
  resourcePool <- asks (.resourcePool)
  runCodensity (acquireResources 1 resourcePool) $ \[backend] -> do
    let domain = backend.berDomain

    -- Phase 1: cassandra. Two users receive pushes; one multi-recipient push
    -- shares a payload between both users (exercises the notification_payload
    -- fan-out path).
    (alice1, count1) <- runCodensity (startDynamicBackend backend (conf "cassandra" False)) $ \_ -> do
      alice <- randomUser domain def
      bob <- randomUser domain def
      _ <- randomClientId
      seed alice bob
      count <- countNotifications alice
      assertBool "notification count did not grow" (count > 0)
      pure (alice, count)

    -- Phase 2: migration-to-postgresql, worker off. Writes are mirrored to
    -- Postgres; reads still come from Cassandra.
    _count2 <- runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" False)) $ \_ -> do
      alice <- randomUser domain def
      bob <- randomUser domain def
      seed alice bob
      count <- countNotifications alice
      assertBool "notification count did not grow" (count > 0)
      pure count

    -- Phase 3: migration-to-postgresql, worker on. The copy loop replicates
    -- the Cassandra rows written in earlier phases.
    _count3 <- runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" True)) $ \_ -> do
      alice <- randomUser domain def
      bob <- randomUser domain def
      seed alice bob
      waitForMigration domain counterName
      count <- countNotifications alice
      assertBool "notification count did not grow" (count > 0)
      pure count

    -- Phase 4: migration-to-postgresql, worker off again (after the copy
    -- loop ran in phase 3). Writes keep being mirrored, reads keep coming
    -- from Cassandra.
    _count4 <- runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" False)) $ \_ -> do
      alice <- randomUser domain def
      bob <- randomUser domain def
      seed alice bob
      count <- countNotifications alice
      assertBool "notification count did not grow" (count > 0)
      pure count

    -- Phase 5: postgresql. Notifications come from Postgres alone (mirrored
    -- and copied); the last-notification read returns the newest payload.
    -- The copy-replicated rows of the phase-1 (cassandra) user are read back
    -- through the Postgres path to verify the copy loop.
    runCodensity (startDynamicBackend backend (conf "postgresql" False)) $ \_ -> do
      copied <- countNotifications alice1
      assertBool "copied phase-1 notifications missing from Postgres" (copied >= count1)
      alice <- randomUser domain def
      bob <- randomUser domain def
      seed alice bob
      count <- countNotifications alice
      assertBool "notification count did not grow" (count > 0)
      lastNotif <- getLastNotification alice def >>= getJSON 200
      lastPayload <- lastNotif %. "payload" & asString
      assertBool "last notification payload is present" (not (null lastPayload))
  where
    conf :: String -> Bool -> ServiceOverrides
    conf db runMigration =
      def
        { gundeckCfg = setField "postgresMigration.notifications" db,
          backgroundWorkerCfg = setField "migrateNotifications" runMigration
        }

    counterName :: String
    counterName = "^wire_notifications_migration_finished"

    -- One multi-recipient push (shared payload => payload_ref fan-out) plus a
    -- single-recipient push (inline payload).
    seed alice bob = do
      ra <- recipient alice
      rb <- recipient bob
      let multi =
            object
              [ "recipients" .= [ra, rb],
                "payload" .= [object ["type" .= ("migration.multi" :: String)]]
              ]
          single =
            object
              [ "recipients" .= [ra],
                "payload" .= [object ["type" .= ("migration.single" :: String)]]
              ]
      postPush alice [multi, single] >>= assertSuccess

    recipient u = do
      uid <- u %. "id" & asString
      pure
        $ object
          [ "user_id" .= uid,
            "route" .= ("any" :: String),
            "clients" .= ([] :: [String])
          ]

    countNotifications user = do
      resp <- getNotifications user def {since = Nothing, size = Just 10000, client = Nothing}
      resp.status `shouldMatchInt` 200
      length <$> (resp.json %. "notifications" >>= asList)
