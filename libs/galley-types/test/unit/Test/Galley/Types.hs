{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-incomplete-uni-patterns #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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
import qualified Data.Set as Set
import Galley.Types.Teams
import Imports
import Test.Galley.Roundtrip (testRoundTrip)
import qualified Test.QuickCheck as QC
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Wire.API.Team.Feature as Public
import Wire.API.Team.Permission
import Wire.API.Team.Role

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
      testRoundTrip @FeatureFlags,
      testGroup
        "permissionsRole, rolePermissions"
        [ testCase "'Role' maps to expected permissions" $ do
            assertEqual "role type changed" [minBound ..] [RoleOwner, RoleAdmin, RoleMember, RoleExternalPartner]
            assertEqual "owner" (permissionsRole =<< newPermissions (intToPerms 8191) (intToPerms 8191)) (Just RoleOwner)
            assertEqual "admin" (permissionsRole =<< newPermissions (intToPerms 5951) (intToPerms 5951)) (Just RoleAdmin)
            assertEqual "member" (permissionsRole =<< newPermissions (intToPerms 1587) (intToPerms 1587)) (Just RoleMember)
            assertEqual "external partner" (permissionsRole =<< newPermissions (intToPerms 1025) (intToPerms 1025)) (Just RoleExternalPartner),
          testCase "Role <-> Permissions roundtrip" $ do
            assertEqual "admin" (permissionsRole . rolePermissions <$> [minBound ..]) (Just <$> [minBound ..]),
          testProperty "Random, incoherent 'Permission' values gracefully translate to subsets." $
            let fakeSort (w, w') = (w `Set.union` w', w')
             in \(fakeSort -> (w, w')) -> do
                  let Just perms = newPermissions w w'
                  case permissionsRole perms of
                    Just role -> do
                      let perms' = rolePermissions role
                      assertEqual "eq" (perms' ^. self) (perms' ^. copy)
                      assertBool "self" ((perms' ^. self) `Set.isSubsetOf` (perms ^. self))
                      assertBool "copy" ((perms' ^. copy) `Set.isSubsetOf` (perms ^. copy))
                    Nothing -> do
                      let leastPermissions = rolePermissions maxBound
                      assertBool "no role for perms, but strictly more perms than max role" $
                        not
                          ( (leastPermissions ^. self) `Set.isSubsetOf` w
                              && (leastPermissions ^. copy) `Set.isSubsetOf` w'
                          )
        ]
    ]

instance Arbitrary FeatureFlags where
  arbitrary =
    FeatureFlags
      <$> QC.elements [minBound ..]
      <*> QC.elements [minBound ..]
      <*> QC.elements [minBound ..]
      -- the default lock status is implicitly added on deserialization and ignored on serialization, therefore we need to fix it to the default here
      -- we will be able to remove this once the lock status is explicitly included in the config
      <*> fmap (fmap unlocked) arbitrary
      <*> fmap unlocked arbitrary
      <*> arbitrary
      <*> fmap (fmap unlocked) arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> fmap (fmap unlocked) arbitrary
      <*> arbitrary
      <*> fmap (fmap unlocked) arbitrary
      <*> fmap (fmap unlocked) arbitrary
      <*> arbitrary
      <*> arbitrary
    where
      unlocked :: ImplicitLockStatus a -> ImplicitLockStatus a
      unlocked = ImplicitLockStatus . Public.setLockStatus Public.LockStatusUnlocked . _unImplicitLockStatus
