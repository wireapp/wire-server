module Main (main) where

import OpenSSL
import System.Environment
import Test.Tasty
import qualified API.V3

main :: IO ()
main = lookupEnv "INTEGRATION_TEST" >>= maybe (return ()) (const run)
  where
    run = withOpenSSL $ do
        cp <- getEnv "CARGOHOLD_WEB_PORT"
        v3 <- API.V3.tests cp
        defaultMain $ testGroup "API" [v3]
