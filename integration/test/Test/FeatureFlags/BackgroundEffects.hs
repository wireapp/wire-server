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

module Test.FeatureFlags.BackgroundEffects where

import SetupHelpers (createTeam)
import Test.FeatureFlags.Util
import Testlib.Prelude

testPatchBackgroundEffects :: (HasCallStack) => App ()
testPatchBackgroundEffects = withAPIVersion 16 $ checkPatch OwnDomain "backgroundEffects" enabled

testBackgroundEffects :: (HasCallStack) => APIAccess -> App ()
testBackgroundEffects access =
  withAPIVersion 16
    $ mkFeatureTests "backgroundEffects"
    & addUpdate enabled
    & runFeatureTests OwnDomain access

-- | WPB-27912: the public backgroundEffects endpoints are gated at v17 (404)
-- while remaining available through v16. Only the v16 GET is asserted here:
-- v16 PUT success is covered by 'testBackgroundEffects' (whose runFeatureTests
-- unlocks the feature first), and a public PUT in this test would 409
-- feature-locked against the default enabled+locked state.
testBackgroundEffectsRemovedAtV17 :: (HasCallStack) => App ()
testBackgroundEffectsRemovedAtV17 = do
  (owner, tid, _) <- createTeam OwnDomain 0
  let p = joinHttpPath ["teams", tid, "features", "backgroundEffects"]
      body = object ["status" .= "enabled", "lockStatus" .= "locked"]
  bindResponse (baseRequest owner Galley (ExplicitVersion 17) p >>= submit "GET") $ \resp -> do
    resp.status `shouldMatchInt` 404
  bindResponse (baseRequest owner Galley (ExplicitVersion 17) p <&> addJSON body >>= submit "PUT") $ \resp -> do
    resp.status `shouldMatchInt` 404
  bindResponse (baseRequest owner Galley (ExplicitVersion 16) p >>= submit "GET") $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "status" `shouldMatch` "enabled"
    resp.json %. "lockStatus" `shouldMatch` "locked"

-- | WPB-27912: the aggregate list endpoints 'GET /feature-configs' and
-- 'GET /teams/:tid/features' are version-agnostic — like 'From'-gated features
-- (e.g. MLS), they include every feature at every API version. Even though the
-- dedicated backgroundEffects endpoints 404 at v17, both list endpoints keep
-- returning the (default enabled+locked) backgroundEffects entry. This test
-- locks that behaviour in.
testBackgroundEffectsListedAtV17 :: (HasCallStack) => App ()
testBackgroundEffectsListedAtV17 = do
  (owner, tid, []) <- createTeam OwnDomain 0
  let assertBackgroundEffects resp = do
        resp.status `shouldMatchInt` 200
        be <- resp.json %. "backgroundEffects"
        be %. "status" `shouldMatch` "enabled"
        be %. "lockStatus" `shouldMatch` "locked"
      teamFeatures = joinHttpPath ["teams", tid, "features"]
  bindResponse (baseRequest owner Galley (ExplicitVersion 17) "/feature-configs" >>= submit "GET") assertBackgroundEffects
  bindResponse (baseRequest owner Galley (ExplicitVersion 17) teamFeatures >>= submit "GET") assertBackgroundEffects
