module Test.Wire.API.Routes.Version where

import Imports
import qualified Test.Tasty as T
import Test.Tasty.HUnit
import Wire.API.Routes.Version

tests :: T.TestTree
tests =
  T.testGroup "Version" $
    [ T.testGroup
        "toPathComponent"
        [testCase "serialise different versions" testToPathComponent]
    ]

testToPathComponent :: Assertion
testToPathComponent = do
  "v0" @=? toPathComponent V0
  "v1" @=? toPathComponent V1
  "v2" @=? toPathComponent V2
  "v3" @=? toPathComponent V3
  "v4" @=? toPathComponent V4
