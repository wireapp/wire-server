{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Main (main) where

import Bilge hiding (header, body)
import Control.Lens
import Cassandra.Util
import Data.Aeson
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml (decodeFileEither)
import GHC.Generics
import Network.HTTP.Client (responseTimeoutMicro)
import Network.HTTP.Client.TLS
import OpenSSL (withOpenSSL)
import Test.Tasty
import Types
import Util.Options
import Util.Options.Common
import Util.Test

import qualified API
import qualified Gundeck.Options as Opts
import qualified System.Logger   as Logger

data IntegrationConfig = IntegrationConfig
  -- internal endpoints
  { gundeck   :: Endpoint
  , cannon    :: Endpoint
  , brig      :: Endpoint
  } deriving (Show, Generic)

instance FromJSON IntegrationConfig

main :: IO ()
main = withOpenSSL $ runTests go
  where
    go g i = withResource (getOpts g i) releaseOpts $ \opts -> API.tests opts

    getOpts gFile iFile = do
        m <- newManager tlsManagerSettings {
            managerResponseTimeout = responseTimeoutMicro 300000000
        }
        let local p = Endpoint { _epHost = "127.0.0.1", _epPort = p }
        gConf <- handleParseError =<< decodeFileEither gFile
        iConf <- handleParseError =<< decodeFileEither iFile
        g <- Gundeck . mkRequest <$> optOrEnv gundeck iConf (local . read) "GUNDECK_WEB_PORT"
        c <- Cannon  . mkRequest <$> optOrEnv cannon iConf (local . read) "CANNON_WEB_PORT"
        b <- Brig    . mkRequest <$> optOrEnv brig iConf (local . read) "BRIG_WEB_PORT"
        ch <- optOrEnv (\v -> v^.Opts.cassandra.casEndpoint.epHost) gConf pack "GUNDECK_CASSANDRA_HOST"
        cp <- optOrEnv (\v -> v^.Opts.cassandra.casEndpoint.epPort) gConf read "GUNDECK_CASSANDRA_PORT"

        lg <- Logger.new Logger.defSettings
        db <- defInitCassandra "gundeck_test" ch cp lg

        return $ API.TestSetup m g c b db 

    releaseOpts _ = return ()
    
    mkRequest (Endpoint h p) = host (encodeUtf8 h) . port p
