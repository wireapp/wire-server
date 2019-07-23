module Main (main) where

import Imports

import Data.Metrics.Test (sitemapConsistency)
import Network.Wai.Utilities.Server (compile)
import OpenSSL (withOpenSSL)
import Test.Tasty

import qualified DelayQueue
import qualified Gundeck.API
import qualified Json
import qualified Native
import qualified Push

main :: IO ()
main = withOpenSSL . defaultMain $
    testGroup "Main"
        [ sitemapConsistency . compile $ Gundeck.API.sitemap
        , DelayQueue.tests
        , Json.tests
        , Native.tests
        , Push.tests
        ]
