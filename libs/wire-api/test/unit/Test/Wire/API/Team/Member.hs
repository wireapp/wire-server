{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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

module Test.Wire.API.Team.Member (tests) where

import Data.Aeson
import Data.Set (isSubsetOf)
import Data.Set qualified as Set
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Wire.API.Team.Member
import Wire.API.Team.Permission
import Wire.API.Team.Role

-- NB: validateEveryToJSON from servant-swagger doesn't render these tests unnecessary!

tests :: TestTree
tests = testGroup "Wire.API.Team.Member" [commonTests, permissionTests, permissionConversionTests]

commonTests :: TestTree
commonTests =
  testGroup
    "Common (types vs. aeson)"
    [ testCase "{} is a valid TeamMemberDeleteData" $ do
        assertBool "{}" (isRight (eitherDecode @TeamMemberDeleteData "{}"))
    ]

permissionTests :: TestTree
permissionTests =
  testGroup
    "Permissions"
    [ testCase "owner has all permissions" $
        rolePermissions RoleOwner @=? fullPermissions,
      testCase "smaller roles (further to the left/top in the type def) are strictly more powerful" $
        -- we may not want to maintain this property in the future when adding more roles, but for
        -- now it's true, and it's nice to have that written down somewhere.
        forM_ [(r1, r2) | r1 <- [minBound ..], r2 <- drop 1 [r1 ..]] $
          \(r1, r2) -> do
            assertBool "owner.self" (((rolePermissions r2).self) `isSubsetOf` ((rolePermissions r1).self))
            assertBool "owner.copy" (((rolePermissions r2).copy) `isSubsetOf` ((rolePermissions r1).copy)),
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
                      assertEqual "eq" perms'.self perms'.copy
                      assertBool "self" (perms'.self `Set.isSubsetOf` perms.self)
                      assertBool "copy" (perms'.copy `Set.isSubsetOf` perms.copy)
                    Nothing -> do
                      let leastPermissions = rolePermissions maxBound
                      assertBool "no role for perms, but strictly more perms than max role" $
                        not
                          ( (leastPermissions.self) `Set.isSubsetOf` w
                              && (leastPermissions.copy) `Set.isSubsetOf` w'
                          )
        ]
    ]

permissionConversionTests :: TestTree
permissionConversionTests =
  testGroup
    "permsToInt / rolePermissions / serialization of `Role`s"
    [ testCase "partner" $ assertEqual "" (permsToInt . self $ rolePermissions RoleExternalPartner) 1025,
      testCase "member" $ assertEqual "" (permsToInt . self $ rolePermissions RoleMember) 1587,
      testCase "admin" $ assertEqual "" (permsToInt . self $ rolePermissions RoleAdmin) 5951,
      testCase "owner" $ assertEqual "" (permsToInt . self $ rolePermissions RoleOwner) 8191
    ]
