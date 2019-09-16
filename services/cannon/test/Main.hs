module Main where

import Imports

import Data.Metrics.Test (pathsConsistencyCheck)
import Data.Metrics.WaiRoute (treeToPaths)
import Network.Wai.Utilities.Server (compile)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Bench            as B
import qualified Cannon.API
import qualified Test.Cannon.Dict as D

main :: IO ()
main = do
    B.benchmark
    defaultMain $ testGroup "Tests"
        [ testCase "sitemap" $ assertEqual "inconcistent sitemap"
            mempty
            (pathsConsistencyCheck . treeToPaths . compile $ Cannon.API.sitemap)
        , D.tests
        ]
