-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2023 Wire Swiss GmbH <opensource@wire.com>
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

module Test.FeatureFlags where

import qualified API.Galley as Public
import qualified API.GalleyInternal as Internal
import SetupHelpers
import Testlib.Prelude

testLimitedEventFanout :: HasCallStack => App ()
testLimitedEventFanout = do
  let featureName = "limitedEventFanout"
  (_alice, team, _) <- createTeam OwnDomain 1
  -- getTeamFeatureStatus OwnDomain team "limitedEventFanout" "enabled"
  bindResponse (Internal.getTeamFeature OwnDomain team featureName) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "status" `shouldMatch` "disabled"
  Internal.setTeamFeatureStatus OwnDomain team featureName "enabled"
  bindResponse (Internal.getTeamFeature OwnDomain team featureName) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "status" `shouldMatch` "enabled"

testLegalhold :: HasCallStack => App ()
testLegalhold = do
  (owner, tid, _) <- createTeam OwnDomain 1
  let expected = object ["lockStatus" .= "unlocked", "status" .= "disabled", "ttl" .= "unlimited"]
  bindResponse (Internal.getTeamFeature OwnDomain tid "legalhold") $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json `shouldMatch` expected
  bindResponse (Public.getFeatureConfigs owner) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "legalhold" `shouldMatch` expected
  bindResponse (Public.getTeamFeatures owner tid) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "legalhold" `shouldMatch` expected
  bindResponse (Public.getTeamFeature owner tid "legalhold") $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json `shouldMatch` expected
