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

module Test.FeatureFlags.MeetingPremium where

import SetupHelpers (createTeam)
import Test.FeatureFlags.Util
import Testlib.Prelude

testPatchMeetingPremium :: (HasCallStack) => App ()
testPatchMeetingPremium = withAPIVersion 16 $ checkPatch OwnDomain "meetingsPremium" disabledLocked

testMeetingPremium :: (HasCallStack) => APIAccess -> App ()
testMeetingPremium access =
  withAPIVersion 16
    $ mkFeatureTests "meetingsPremium"
    & addUpdate enabled
    & runFeatureTests OwnDomain access

-- | WPB-26771: the public meetingsPremium endpoints are gated at v17 (404)
-- while remaining available through v16. Only the v16 GET is asserted here:
-- v16 PUT success is covered by 'testMeetingPremium' (whose runFeatureTests
-- unlocks the feature first), and a public PUT in this test would 409
-- feature-locked against the default enabled+locked state.
testMeetingPremiumRemovedAtV17 :: (HasCallStack) => App ()
testMeetingPremiumRemovedAtV17 = do
  (owner, tid, _) <- createTeam OwnDomain 0
  let p = joinHttpPath ["teams", tid, "features", "meetingsPremium"]
      body = object ["status" .= "enabled", "lockStatus" .= "locked"]
  bindResponse (baseRequest owner Galley (ExplicitVersion 17) p >>= submit "GET") $ \resp -> do
    resp.status `shouldMatchInt` 404
  bindResponse (baseRequest owner Galley (ExplicitVersion 17) p <&> addJSON body >>= submit "PUT") $ \resp -> do
    resp.status `shouldMatchInt` 404
  bindResponse (baseRequest owner Galley (ExplicitVersion 16) p >>= submit "GET") $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "status" `shouldMatch` "enabled"
    resp.json %. "lockStatus" `shouldMatch` "locked"

-- | WPB-26771: the aggregate list endpoints 'GET /feature-configs' and
-- 'GET /teams/:tid/features' are version-agnostic — like 'From'-gated features
-- (e.g. MLS), they include every feature at every API version. Even though the
-- dedicated meetingsPremium endpoints 404 at v17, both list endpoints keep
-- returning the (default enabled+locked) meetingsPremium entry. This test
-- locks that behaviour in.
testMeetingPremiumListedAtV17 :: (HasCallStack) => App ()
testMeetingPremiumListedAtV17 = do
  (owner, tid, _) <- createTeam OwnDomain 0
  let assertMeetingPremium resp = do
        resp.status `shouldMatchInt` 200
        mp <- resp.json %. "meetingsPremium"
        mp %. "status" `shouldMatch` "enabled"
        mp %. "lockStatus" `shouldMatch` "locked"
      teamFeatures = joinHttpPath ["teams", tid, "features"]
  bindResponse (baseRequest owner Galley (ExplicitVersion 17) "/feature-configs" >>= submit "GET") assertMeetingPremium
  bindResponse (baseRequest owner Galley (ExplicitVersion 17) teamFeatures >>= submit "GET") assertMeetingPremium
