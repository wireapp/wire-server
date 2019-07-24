module Main where

import Imports

import Data.Metrics.Test (sitemapConsistency)
import Network.Wai.Utilities.Server (compile)
import Test.Tasty

import qualified Bench            as B
import qualified Cannon.API
import qualified Test.Cannon.Dict as D

main :: IO ()
main = do
    B.benchmark
    defaultMain $ testGroup "Tests"
        [ sitemapConsistency . compile $ Cannon.API.sitemap
        , D.tests
        ]
