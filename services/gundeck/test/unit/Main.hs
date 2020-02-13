module Main (main) where

import Data.Metrics.Test (pathsConsistencyCheck)
import Data.Metrics.WaiRoute (treeToPaths)
import qualified DelayQueue
import qualified Gundeck.API
import Imports
import qualified Json
import qualified Native
import Network.Wai.Utilities.Server (compile)
import OpenSSL (withOpenSSL)
import qualified Push
import Test.Tasty
import Test.Tasty.HUnit
import qualified ThreadBudget

main :: IO ()
main =
  withOpenSSL . defaultMain $
    testGroup
      "Main"
      [ testCase "sitemap" $
          assertEqual
            "inconcistent sitemap"
            mempty
            (pathsConsistencyCheck . treeToPaths . compile $ Gundeck.API.sitemap),
        DelayQueue.tests,
        Json.tests,
        Native.tests,
        Push.tests,
        ThreadBudget.tests
      ]
