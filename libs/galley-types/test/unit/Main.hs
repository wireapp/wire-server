module Main (main) where

import Imports
import qualified Test.Galley.Types
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "Tests"
      [ Test.Galley.Types.tests
      ]
