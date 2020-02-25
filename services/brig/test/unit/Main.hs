module Main (main) where

import Imports
import qualified Test.Brig.User.Search.Index.Types
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "Tests"
      [ Test.Brig.User.Search.Index.Types.tests
      ]
