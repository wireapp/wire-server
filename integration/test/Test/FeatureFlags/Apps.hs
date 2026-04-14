-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Test.FeatureFlags.Apps where

import API.Brig (NewApp (..), createApp)
import qualified API.BrigInternal as BrigI
import qualified API.GalleyInternal as Internal
import SetupHelpers
import Test.FeatureFlags.Util
import Testlib.Prelude

testAppsInternal :: (HasCallStack) => App ()
testAppsInternal = do
  (alice, tid, _) <- createTeam OwnDomain 0
  Internal.setTeamFeatureLockStatus alice tid "apps" "unlocked"
  withWebSocket alice $ \ws -> do
    setFlag InternalAPI ws tid "apps" enabled
    setFlag InternalAPI ws tid "apps" disabled
  Internal.setTeamFeatureLockStatus alice tid "apps" "locked"
  setFeature InternalAPI alice tid "apps" enabled `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 409
    resp.json %. "label" `shouldMatch` "feature-locked"
  -- the feature does not have a public PUT endpoint
  setFeature PublicAPI alice tid "apps" enabled `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 404
    resp.json %. "label" `shouldMatch` "no-endpoint"

testPatchApps :: (HasCallStack) => App ()
testPatchApps = checkPatch OwnDomain "apps" disabled

-- | Disabling the apps feature for a team suspends all app users in that team.
-- Re-enabling it restores them to active.  Regular team members are unaffected.
testAppsSuspendOnDisable :: (HasCallStack) => App ()
testAppsSuspendOnDisable = do
  (owner, tid, [regularMember]) <- createTeam OwnDomain 2
  Internal.setTeamFeatureLockStatus owner tid "apps" "unlocked"

  -- Create an app user in the team
  app <-
    let newApp =
          NewApp
            { name = "poll-app",
              assets = Nothing,
              accentId = Nothing,
              category = "other",
              description = "also other"
            }
     in bindResponse (createApp owner tid newApp) $ \resp -> do
          resp.status `shouldMatchInt` 200
          resp.json %. "user"

  -- Verify initial account statuses are active
  BrigI.getAccountStatus app `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "status" `shouldMatch` "active"
  BrigI.getAccountStatus regularMember `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "status" `shouldMatch` "active"

  -- Disable the apps feature: app users should be suspended
  setFeature InternalAPI owner tid "apps" disabled >>= assertSuccess

  BrigI.getAccountStatus app `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "status" `shouldMatch` "suspended"

  -- Regular member must NOT be suspended
  BrigI.getAccountStatus regularMember `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "status" `shouldMatch` "active"

  -- Re-enable the apps feature: app users should be active again
  setFeature InternalAPI owner tid "apps" enabled >>= assertSuccess

  BrigI.getAccountStatus app `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "status" `shouldMatch` "active"
