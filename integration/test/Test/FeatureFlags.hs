{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

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
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Set as Set
import SetupHelpers
import Test.FeatureFlags.Util
import Testlib.Prelude

testLimitedEventFanout :: (HasCallStack) => FeatureTable -> App ()
testLimitedEventFanout ft = do
  let featureName = "limitedEventFanout"
  (_alice, team, _) <- createTeam OwnDomain 1
  updateMigrationState OwnDomain team ft
  bindResponse (Internal.getTeamFeature OwnDomain team featureName) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "status" `shouldMatch` "disabled"
  assertSuccess =<< Internal.setTeamFeatureStatus OwnDomain team featureName "enabled"
  bindResponse (Internal.getTeamFeature OwnDomain team featureName) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "status" `shouldMatch` "enabled"

-- | Call 'GET /teams/:tid/features' and 'GET /feature-configs', and check if all
-- features are there.
testAllFeatures :: (HasCallStack) => FeatureTable -> App ()
testAllFeatures ft = do
  (_, tid, m : _) <- createTeam OwnDomain 2
  updateMigrationState OwnDomain tid ft
  bindResponse (Public.getTeamFeatures m tid) $ \resp -> do
    resp.status `shouldMatchInt` 200
    defAllFeatures `shouldMatch` resp.json

  -- This block catches potential errors in the logic that reverts to default if there is a distinction made between
  -- 1. there is no row for a team_id in galley.team_features
  -- 2. there is a row for team_id in galley.team_features but the feature has a no entry (null value)
  Internal.setTeamFeatureConfig OwnDomain tid "conversationGuestLinks" enabled >>= assertSuccess

  bindResponse (Public.getTeamFeatures m tid) $ \resp -> do
    resp.status `shouldMatchInt` 200
    defAllFeatures `shouldMatch` resp.json

  bindResponse (Public.getFeatureConfigs m) $ \resp -> do
    resp.status `shouldMatchInt` 200
    defAllFeatures `shouldMatch` resp.json

  randomPersonalUser <- randomUser OwnDomain def

  bindResponse (Public.getFeatureConfigs randomPersonalUser) $ \resp -> do
    resp.status `shouldMatchInt` 200
    defAllFeatures `shouldMatch` resp.json

testFeatureConfigConsistency :: (HasCallStack) => FeatureTable -> App ()
testFeatureConfigConsistency ft = do
  (_, tid, m : _) <- createTeam OwnDomain 2
  updateMigrationState OwnDomain tid ft

  allFeaturesRes <- Public.getFeatureConfigs m >>= parseObjectKeys

  allTeamFeaturesRes <- Public.getTeamFeatures m tid >>= parseObjectKeys

  unless (allTeamFeaturesRes `Set.isSubsetOf` allFeaturesRes)
    $ assertFailure (show allTeamFeaturesRes <> " is not a subset of " <> show allFeaturesRes)
  where
    parseObjectKeys :: Response -> App (Set.Set String)
    parseObjectKeys res = do
      val <- res.json
      case val of
        (A.Object hm) -> pure (Set.fromList . map (show . A.toText) . KM.keys $ hm)
        x -> assertFailure ("JSON was not an object, but " <> show x)

testNonMemberAccess :: (HasCallStack) => FeatureTable -> Feature -> App ()
testNonMemberAccess ft (Feature featureName) = do
  (_, tid, _) <- createTeam OwnDomain 0
  updateMigrationState OwnDomain tid ft
  nonMember <- randomUser OwnDomain def
  Public.getTeamFeature nonMember tid featureName
    >>= assertForbidden
