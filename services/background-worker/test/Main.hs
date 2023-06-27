module Main
  ( main,
  )
where

import Data.Proxy
import Data.Yaml (decodeFileEither)
import Imports
import OpenSSL (withOpenSSL)
import Test.Tasty
import Test.Tasty.Options
import Test.Wire.BackendNotificationPusherSpec
import Test.Wire.Defederation
import Test.Wire.Util
import Util.Test

runTests :: (String -> String -> TestTree) -> IO ()
runTests run = defaultMainWithIngredients ings $
  askOption $
    \(ServiceConfigFile c) ->
      askOption $ \(IntegrationConfigFile i) -> run c i
  where
    ings =
      includingOptions
        [ Option (Proxy :: Proxy ServiceConfigFile),
          Option (Proxy :: Proxy IntegrationConfigFile)
        ]
        : defaultIngredients

main :: IO ()
main = withOpenSSL $ runTests go
  where
    go o i = withResource (getOpts o i) releaseOpts $ \setup -> do
      testGroup
        "background-worker"
        [ spec setup,
          deleteFederationDomainSpec setup
        ]
    getOpts oFile iFile = do
      opts <- handleParseError =<< decodeFileEither oFile
      iConf <- handleParseError =<< decodeFileEither iFile
      pure $ TestSetup opts iConf
    releaseOpts _ = pure ()
