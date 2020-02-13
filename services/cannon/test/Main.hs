module Main where

import qualified Bench as B
import qualified Cannon.API
import Data.Metrics.Test (pathsConsistencyCheck)
import Data.Metrics.WaiRoute (treeToPaths)
import Imports
import Network.Wai.Utilities.Server (compile)
import qualified Test.Cannon.Dict as D
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
  B.benchmark
  defaultMain $
    testGroup
      "Tests"
      [ testCase "sitemap" $
          assertEqual
            "inconcistent sitemap"
            mempty
            (pathsConsistencyCheck . treeToPaths . compile $ Cannon.API.sitemap),
        D.tests
      ]
