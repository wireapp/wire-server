module Main (main) where

import Imports

import Data.Metrics.Test (pathsConsistencyCheck)
import Data.Metrics.WaiRoute (treeToPaths)
import Network.Wai.Utilities.Server (compile)
import OpenSSL (withOpenSSL)
import Test.Tasty
import Test.Tasty.HUnit

import qualified DelayQueue
import qualified Gundeck.API
import qualified Json
import qualified Native
import qualified Push

main :: IO ()
main = withOpenSSL . defaultMain $
    testGroup "Main"
        [ testCase "sitemap" $ assertEqual "inconcistent sitemap"
            mempty
            (pathsConsistencyCheck . treeToPaths . compile $ Gundeck.API.sitemap)
        , DelayQueue.tests
        , Json.tests
        , Native.tests
        , Push.tests
        ]
