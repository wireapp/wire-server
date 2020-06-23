{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Galley.Types where

import Control.Lens
import Data.Set hiding (drop)
import Galley.Types.IdMapping (PostIdMappingRequest (..), PostIdMappingResponse (..))
import Galley.Types.Teams
import Imports
import Test.Galley.Roundtrip (testRoundTrip)
import Test.QuickCheck (Arbitrary (arbitrary))
import qualified Test.QuickCheck as QC
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ testCase "owner has all permissions" $
        rolePermissions RoleOwner @=? fullPermissions,
      testCase "smaller roles (further to the left/top in the type def) are strictly more powerful" $
        -- we may not want to maintain this property in the future when adding more roles, but for
        -- now it's true, and it's nice to have that written down somewhere.
        forM_ [(r1, r2) | r1 <- [minBound ..], r2 <- drop 1 [r1 ..]] $
          \(r1, r2) -> do
            assertBool "owner.self" ((rolePermissions r2 ^. self) `isSubsetOf` (rolePermissions r1 ^. self))
            assertBool "owner.copy" ((rolePermissions r2 ^. copy) `isSubsetOf` (rolePermissions r1 ^. copy)),
      testCase "permissions for viewing feature flags" $
        -- We currently (at the time of writing this test) grant view permissions for all
        -- 'TeamFeatureName's to all roles.  If we add more features in the future and forget to
        -- add them, this test will fail, and remind us that there we should consider adding.
        -- If you want to handle view permissions for future features differntly, adopt the test
        -- accordingly.  Just maintain the property that adding a new feature name will break
        -- this test, and force future develpers to consider what permissions they want to set.
        assertBool "all covered" (all (roleHasPerm RoleExternalPartner) (ViewTeamFeature <$> [minBound ..])),
      testRoundTrip @FeatureFlags,
      testRoundTrip @PostIdMappingRequest,
      testRoundTrip @PostIdMappingRequest
    ]

instance Arbitrary FeatureFlags where
  arbitrary =
    FeatureFlags
      <$> QC.elements [minBound ..]
      <*> QC.elements [minBound ..]
      <*> QC.elements [minBound ..]

deriving newtype instance Arbitrary PostIdMappingRequest

deriving newtype instance Arbitrary PostIdMappingResponse
