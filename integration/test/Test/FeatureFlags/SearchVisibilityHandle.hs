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

module Test.FeatureFlags.SearchVisibilityHandle where

import qualified API.Galley as Public
import SetupHelpers
import Test.FeatureFlags.Util
import Testlib.Prelude

testSearchVisibilityHandle :: (HasCallStack) => APIAccess -> App ()
testSearchVisibilityHandle access =
  mkFeatureTests "searchVisibilityHandle"
    & addUpdate disabled
    & addUpdate enabled
    & runFeatureTests OwnDomain access

testPatchSearchVisibilityHandle :: (HasCallStack) => App ()
testPatchSearchVisibilityHandle = do
  checkPatch OwnDomain "searchVisibilityHandle" disabled
  checkPatch OwnDomain "searchVisibilityHandle" $ object ["status" .= "enabled"]
  checkPatch OwnDomain "searchVisibilityHandle" $ object ["status" .= "disabled"]

-- | The flag is not readable by users outside the team, and defaults to
-- disabled.  Toggling it has no side effects (no reindexing); the
-- enforcement happens at request time in brig.
testSearchVisibilityHandleInternal :: (HasCallStack) => APIAccess -> App ()
testSearchVisibilityHandleInternal access = do
  let featureName = "searchVisibilityHandle"
  (alice, tid, _) <- createTeam OwnDomain 2
  eve <- randomUser OwnDomain def
  assertForbidden =<< Public.getTeamFeature eve tid featureName
  checkFeature featureName alice tid disabled

  void $ withWebSocket alice $ \ws -> do
    setFlag access ws tid featureName enabled
    setFlag access ws tid featureName disabled
