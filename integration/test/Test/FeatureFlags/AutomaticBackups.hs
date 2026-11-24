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
import SetupHelpers
import Test.FeatureFlags.Util
import Testlib.Prelude

testAutomaticBackups :: (HasCallStack) => APIAccess -> App ()
testAutomaticBackups access =
  mkFeatureTests "automaticBackups"
    -- order matters: the 2nd update clears the schedule set by the 1st;
    -- runFeatureTests' final check expects the last update to survive lock/unlock
    & addUpdate (cfg ["enforcedSchedule" .= "daily", "enforced" .= True, "serverOnly" .= True])
    & addUpdate (cfg ["enforced" .= False, "serverOnly" .= True])
    & addInvalidUpdate (cfg ["enforcedSchedule" .= "hourly", "enforced" .= False, "serverOnly" .= False])
    & addInvalidUpdate (cfg ["enforcedSchedule" .= "Daily", "enforced" .= False, "serverOnly" .= False])
    & addInvalidUpdate (cfg ["enforced" .= ("yes" :: String), "serverOnly" .= False])
    & runFeatureTests OwnDomain access
  where
    cfg fields = object ["status" .= "enabled", "config" .= object fields]

testPatchAutomaticBackups :: (HasCallStack) => App ()
testPatchAutomaticBackups = do
  checkPatch OwnDomain "automaticBackups" $ object ["lockStatus" .= "unlocked"]
  checkPatch OwnDomain "automaticBackups" $ object ["lockStatus" .= "unlocked", "status" .= "enabled"]
  checkPatch OwnDomain "automaticBackups"
    $ object
      [ "lockStatus" .= "unlocked",
        "config" .= object ["enforcedSchedule" .= "weekly", "enforced" .= True, "serverOnly" .= False]
      ]

testAutomaticBackupsTtlIgnored :: (HasCallStack) => App ()
testAutomaticBackupsTtlIgnored = do
  (owner, tid, _) <- createTeam OwnDomain 0
  Internal.setTeamFeatureLockStatus owner tid "automaticBackups" "unlocked"
  let body =
        object
          [ "status" .= "enabled",
            "config" .= object ["enforcedSchedule" .= "monthly", "enforced" .= False, "serverOnly" .= False],
            "ttl" .= (60 :: Int)
          ]
  bindResponse (Public.setTeamFeatureConfig owner tid "automaticBackups" body) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "ttl" `shouldMatch` "unlimited"
  bindResponse (Public.getTeamFeature owner tid "automaticBackups") $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "ttl" `shouldMatch` "unlimited"
    resp.json %. "config.enforcedSchedule" `shouldMatch` "monthly"
