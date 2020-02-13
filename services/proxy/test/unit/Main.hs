module Main (main) where

import Data.Metrics (Metrics, metrics)
import Data.Metrics.Test (sitemapConsistency)
import Imports
import Network.Wai.Utilities.Server (compile)
import Proxy.API (sitemap)
import Proxy.Env (Env, createEnv)
import Proxy.Options (mockOpts)
import Test.Tasty

main :: IO ()
main = do
  mockEnv <- mkMockEnv =<< metrics
  defaultMain $
    testGroup
      "Main"
      [ sitemapConsistency . compile $ sitemap mockEnv
      ]

mkMockEnv :: Metrics -> IO Env
mkMockEnv mtrx = createEnv mtrx (mockOpts "doc/example.config")
