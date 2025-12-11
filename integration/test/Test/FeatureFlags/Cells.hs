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

module Test.FeatureFlags.Cells where

import API.Galley (setTeamFeatureConfigVersioned)
import SetupHelpers
import Test.FeatureFlags.Util
import Testlib.Prelude

testCells :: (HasCallStack) => APIAccess -> App ()
testCells access =
  mkFeatureTests "cells"
    & addUpdate (validConfig True)
    & addUpdate (validConfig False)
    & addInvalidUpdate invalidConfig
    & runFeatureTests OwnDomain access

testPatchCells :: (HasCallStack) => App ()
testPatchCells = checkPatch OwnDomain "cells" (validConfig True)

validConfig :: Bool -> Value
validConfig b =
  object
    [ "status" .= if b then "enabled" else "disabled",
      "config" .= object ["foo" .= "bar"]
    ]

invalidConfig :: Value
invalidConfig =
  object
    [ "status" .= "enabled",
      "config" .= object ["foox" .= "bar"]
    ]

testCellsV13 :: (HasCallStack) => App ()
testCellsV13 = do
  (alice, tid, _) <- createTeam OwnDomain 1
  setTeamFeatureConfigVersioned
    (ExplicitVersion 13)
    alice
    tid
    "cells"
    enabled
    `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
