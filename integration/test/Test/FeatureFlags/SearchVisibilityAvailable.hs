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

module Test.FeatureFlags.SearchVisibilityAvailable where

import qualified API.Galley as Public
import qualified API.GalleyInternal as Internal
import SetupHelpers
import Test.FeatureFlags.Util
import Testlib.Prelude

testPatchSearchVisibility :: (HasCallStack) => App ()
testPatchSearchVisibility = checkPatch OwnDomain "searchVisibility" enabled

testSearchVisibilityDisabledByDefault :: (HasCallStack) => App ()
testSearchVisibilityDisabledByDefault = do
  withModifiedBackend def {galleyCfg = setField "settings.featureFlags.teamSearchVisibility" "disabled-by-default"} $ \domain -> do
    (owner, tid, m : _) <- createTeam domain 2
    -- Test default
    checkFeature "searchVisibility" m tid disabled
    assertSuccess =<< Internal.setTeamFeatureStatus owner tid "searchVisibility" "enabled"
    checkFeature "searchVisibility" owner tid enabled
    assertSuccess =<< Internal.setTeamFeatureStatus owner tid "searchVisibility" "disabled"
    checkFeature "searchVisibility" owner tid disabled

testSearchVisibilityEnabledByDefault :: (HasCallStack) => App ()
testSearchVisibilityEnabledByDefault = do
  withModifiedBackend def {galleyCfg = setField "settings.featureFlags.teamSearchVisibility" "enabled-by-default"} $ \domain -> do
    (owner, tid, m : _) <- createTeam domain 2
    nonMember <- randomUser domain def
    assertForbidden =<< Public.getTeamFeature nonMember tid "searchVisibility"
    -- Test default
    checkFeature "searchVisibility" m tid enabled
    assertSuccess =<< Internal.setTeamFeatureStatus owner tid "searchVisibility" "disabled"
    checkFeature "searchVisibility" owner tid disabled
    assertSuccess =<< Internal.setTeamFeatureStatus owner tid "searchVisibility" "enabled"
    checkFeature "searchVisibility" owner tid enabled
