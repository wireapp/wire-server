{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Main (main) where

import Bilge hiding (header, body)
import Control.Lens
import Data.Yaml hiding (Parser)
import GHC.Generics
import Network.HTTP.Client (responseTimeoutMicro)
import Network.HTTP.Client.TLS
import OpenSSL
import Util.Options
import Util.Options.Common
import Util.Test
import Test.Tasty

import qualified API.V3

data IntegrationConfig = IntegrationConfig
  -- internal endpoint
  { cargohold :: Endpoint
  } deriving (Show, Generic)

instance FromJSON IntegrationConfig

main :: IO ()
main = withOpenSSL $ runTests go
  where
    go c i = withResource (getOpts c i) releaseOpts $ \opts ->
                testGroup "Cargohold API Integration" [API.V3.tests opts]

    getOpts _ i = do
        -- TODO: It would actually be useful to read some
        -- values from cargohold (max bytes, for instance)
        -- so that tests do not need to keep those values
        -- in sync and the user _knows_ what they are
        m <- newManager tlsManagerSettings {
            managerResponseTimeout = responseTimeoutMicro 300000000
        }
        let local p = Endpoint { _epHost = "127.0.0.1", _epPort = p }
        iConf <- handleParseError =<< decodeFileEither i
        cp <- optOrEnv cargohold iConf (local . read) "CARGOHOLD_WEB_PORT"
        let cg = host "127.0.0.1" . port (cp^.epPort)
        return $ API.V3.TestSetup m cg

    releaseOpts _ = return ()
