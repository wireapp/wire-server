module Main (main) where

import Imports hiding (local)

import Bilge hiding (header, body)
import Data.Metrics.Test (pathsConsistencyCheck)
import Data.Metrics.WaiRoute (treeToPaths)
import Data.Proxy
import Data.Tagged
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml hiding (Parser)
import Network.HTTP.Client (responseTimeoutMicro)
import Network.HTTP.Client.TLS
import Network.Wai.Utilities.Server (compile)
import OpenSSL
import Options.Applicative
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Options
import Util.Options
import Util.Options.Common
import Util.Test

import TestSetup
import qualified API.V3
import qualified Metrics
import qualified CargoHold.API (sitemap)


data IntegrationConfig = IntegrationConfig
  -- internal endpoint
  { cargohold :: Endpoint
  } deriving (Show, Generic)

instance FromJSON IntegrationConfig

newtype ServiceConfigFile = ServiceConfigFile String
    deriving (Eq, Ord, Typeable)

instance IsOption ServiceConfigFile where
    defaultValue = ServiceConfigFile "/etc/wire/cargohold/conf/cargohold.yaml"
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
    go c i = withResource (getOpts c i) releaseOpts $ \opts ->
        testGroup "Cargohold"
            [ testCase "sitemap" $ assertEqual "inconcistent sitemap"
                mempty
                (pathsConsistencyCheck . treeToPaths . compile $ CargoHold.API.sitemap)
            , API.V3.tests opts
            , Metrics.tests opts
            ]

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
        cargo <- mkRequest <$> optOrEnv cargohold iConf (local . read) "CARGOHOLD_WEB_PORT"
        return $ TestSetup m cargo

    mkRequest (Endpoint h p) = host (encodeUtf8 h) . port p

    releaseOpts _ = return ()
