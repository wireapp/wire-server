
module Main (main) where

import Imports hiding (local)
import Bilge hiding (header, body)
import Control.Lens
import Cassandra.Util
import Data.Aeson
import Data.Proxy
import Data.Tagged
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml (decodeFileEither)
import Gundeck.Options
import Network.HTTP.Client (responseTimeoutMicro)
import Network.HTTP.Client.TLS
import OpenSSL (withOpenSSL)
import Options.Applicative
import Test.Tasty
import Test.Tasty.Options
import Types
import Util.Options
import Util.Options.Common
import Util.Test

import qualified API
import qualified System.Logger   as Logger

data IntegrationConfig = IntegrationConfig
  -- internal endpoints
  { gundeck   :: Endpoint
  , cannon    :: Endpoint
  , cannon2   :: Endpoint
  , brig      :: Endpoint
  } deriving (Show, Generic)

instance FromJSON IntegrationConfig

newtype ServiceConfigFile = ServiceConfigFile String
    deriving (Eq, Ord, Typeable)

instance IsOption ServiceConfigFile where
    defaultValue = ServiceConfigFile "/etc/wire/gundeck/conf/gundeck.yaml"
    parseValue = fmap ServiceConfigFile . safeRead
    optionName = return "service-config"
    optionHelp = return "Service config file to read from"
    optionCLParser =
      fmap ServiceConfigFile $ strOption $
        (  short (untag (return 's' :: Tagged ServiceConfigFile Char))
        <> long  (untag (optionName :: Tagged ServiceConfigFile String))
        <> help  (untag (optionHelp :: Tagged ServiceConfigFile String))
        )

runTests :: (String -> String -> TestTree) -> IO ()
runTests run = defaultMainWithIngredients ings $
    askOption $ \(ServiceConfigFile c) ->
    askOption $ \(IntegrationConfigFile i) -> run c i
  where
    ings =
      includingOptions
        [Option (Proxy :: Proxy ServiceConfigFile)
        ,Option (Proxy :: Proxy IntegrationConfigFile)
        ]
      : defaultIngredients

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
        c2 <- Cannon  . mkRequest <$> optOrEnv cannon2 iConf (local . read) "CANNON2_WEB_PORT"
        b <- Brig    . mkRequest <$> optOrEnv brig iConf (local . read) "BRIG_WEB_PORT"
        ch <- optOrEnv (\v -> v^.optCassandra.casEndpoint.epHost) gConf pack "GUNDECK_CASSANDRA_HOST"
        cp <- optOrEnv (\v -> v^.optCassandra.casEndpoint.epPort) gConf read "GUNDECK_CASSANDRA_PORT"
        ck <- optOrEnv (\v -> v^.optCassandra.casKeyspace) gConf pack "GUNDECK_CASSANDRA_KEYSPACE"

        lg <- Logger.new Logger.defSettings
        db <- defInitCassandra ck ch cp lg

        return $ API.TestSetup m g c c2 b db

    releaseOpts _ = return ()

    mkRequest (Endpoint h p) = host (encodeUtf8 h) . port p
