{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Galley.Types where

import Imports
import Control.Lens
import Galley.Types.Teams
import Test.Tasty
import Test.Tasty.HUnit
import Data.Set

tests :: TestTree
tests = testGroup "Tests"
    [ testCase "ownwer has all permissions" $
          rolePermissions RoleOwner @=? fullPermissions
    , testCase "smaller roles (further to the left/top in the type def) are strictly more powerful" $
          -- we may not want to maintain this property in the future when adding more roles, but for
          -- now it's true, and it's nice to have that written down somewhere.
          forM_ [(r1, r2) | r1 <- [minBound..], r2 <- [minBound..]] $ \(r1, r2) -> do
              assertBool "owner.self" ((rolePermissions r2 ^. self) `isSubsetOf` (rolePermissions r1 ^. self) || r1 >= r2)
              assertBool "owner.copy" ((rolePermissions r2 ^. copy) `isSubsetOf` (rolePermissions r1 ^. copy) || r1 >= r2)
    ]
