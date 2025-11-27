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

module Test.FeatureFlags.SearchVisibilityInbound where

import qualified API.Galley as Public
import SetupHelpers
import Test.FeatureFlags.Util
import Testlib.Prelude

testSearchVisibilityInboundInternal :: (HasCallStack) => APIAccess -> App ()
testSearchVisibilityInboundInternal access = do
  let featureName = "searchVisibilityInbound"
  (alice, tid, _) <- createTeam OwnDomain 2
  eve <- randomUser OwnDomain def
  assertForbidden =<< Public.getTeamFeature eve tid featureName
  checkFeature featureName alice tid disabled

  void $ withWebSocket alice $ \ws -> do
    setFlag access ws tid featureName enabled
    setFlag access ws tid featureName disabled
