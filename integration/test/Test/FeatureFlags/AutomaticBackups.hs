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

module Test.FeatureFlags.AutomaticBackups where

import qualified API.Galley as Public
import qualified API.GalleyInternal as Internal
import Notifications
import SetupHelpers
import Test.FeatureFlags.Util
import Testlib.Prelude

featureName :: String
featureName = "automaticBackups"

defaultFeature :: Value
defaultFeature =
  object
    [ "status" .= "disabled",
      "lockStatus" .= "unlocked",
      "ttl" .= "unlimited",
      "config"
        .= object
          [ "schedule" .= "weekly",
            "enforced" .= False,
            "serverOnly" .= False
          ]
    ]

customConfig :: Value
customConfig =
  object
    [ "schedule" .= "daily",
      "enforced" .= True,
      "serverOnly" .= True
    ]

testAutomaticBackupsDefaults :: (HasCallStack) => App ()
testAutomaticBackupsDefaults = do
  (alice, tid, _) <- createTeam OwnDomain 0
  bindResponse (Public.getTeamFeature alice tid featureName) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json `shouldMatch` defaultFeature

testAutomaticBackupsPut :: (HasCallStack) => App ()
testAutomaticBackupsPut = do
  (alice, tid, [bob]) <- createTeam OwnDomain 2
  withWebSocket bob $ \ws -> do
    bindResponse
      (Public.setTeamFeatureConfig alice tid featureName (object ["status" .= "enabled", "config" .= customConfig]))
      $ \resp -> resp.status `shouldMatchInt` 200
    let expected = object ["status" .= "enabled", "lockStatus" .= "unlocked", "ttl" .= "unlimited", "config" .= customConfig]
    bindResponse (Public.getTeamFeature alice tid featureName) $ \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json `shouldMatch` expected
    notif <- awaitMatch isFeatureConfigUpdateNotif ws
    notif %. "payload.0.name" `shouldMatch` featureName
    notif %. "payload.0.data" `shouldMatch` expected

testAutomaticBackupsLocked :: (HasCallStack) => App ()
testAutomaticBackupsLocked = do
  (alice, tid, _) <- createTeam OwnDomain 0
  Internal.setTeamFeatureLockStatus alice tid featureName "locked"
  bindResponse
    (Public.setTeamFeatureConfig alice tid featureName (object ["status" .= "enabled", "config" .= customConfig]))
    $ \resp -> do
      resp.status `shouldMatchInt` 409
      resp.json %. "label" `shouldMatch` "feature-locked"

testAutomaticBackupsInvalidSchedule :: (HasCallStack) => App ()
testAutomaticBackupsInvalidSchedule = do
  (alice, tid, _) <- createTeam OwnDomain 0
  let invalid = object ["status" .= "enabled", "config" .= object ["schedule" .= "hourly", "enforced" .= False, "serverOnly" .= False]]
  bindResponse (Public.setTeamFeatureConfig alice tid featureName invalid) $ \resp ->
    resp.status `shouldMatchInt` 400

testPatchAutomaticBackups :: (HasCallStack) => App ()
testPatchAutomaticBackups = checkPatch OwnDomain featureName disabled
