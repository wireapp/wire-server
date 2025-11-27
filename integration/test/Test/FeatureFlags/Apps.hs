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
