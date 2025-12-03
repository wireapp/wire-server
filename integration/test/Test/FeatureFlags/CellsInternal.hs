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

module Test.FeatureFlags.CellsInternal where

import SetupHelpers
import Test.FeatureFlags.Util
import Testlib.Prelude

testCellsInternal :: (HasCallStack) => App ()
testCellsInternal = do
  (alice, tid, _) <- createTeam OwnDomain 0
  -- the feature does not have a public PUT endpoint
  setFeature PublicAPI alice tid "cellsInternal" enabled `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 404
    resp.json %. "label" `shouldMatch` "no-endpoint"
