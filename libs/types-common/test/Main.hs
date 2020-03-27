module Main (main) where

import Imports
import qualified Test.Domain as Domain
import qualified Test.Handle as Handle
import qualified Test.Properties as Properties
import qualified Test.Qualified as Qualified
import qualified Test.SizedHashMap as SizedHashMap
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "Tests"
      [ Properties.tests,
        SizedHashMap.tests,
        Domain.tests,
        Handle.tests,
        Qualified.tests
      ]
