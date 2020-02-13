{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Galley.Types where

import Control.Lens
import Data.Set hiding (drop)
import Galley.Types.Teams
import Imports
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ testCase "owner has all permissions" $
        rolePermissions RoleOwner @=? fullPermissions,
      testCase "smaller roles (further to the left/top in the type def) are strictly more powerful"
        $
        -- we may not want to maintain this property in the future when adding more roles, but for
        -- now it's true, and it's nice to have that written down somewhere.
        forM_ [(r1, r2) | r1 <- [minBound ..], r2 <- drop 1 [r1 ..]]
        $ \(r1, r2) -> do
          assertBool "owner.self" ((rolePermissions r2 ^. self) `isSubsetOf` (rolePermissions r1 ^. self))
          assertBool "owner.copy" ((rolePermissions r2 ^. copy) `isSubsetOf` (rolePermissions r1 ^. copy))
    ]
