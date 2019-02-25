module Main (main) where

import Imports hiding (local)
import Bilge hiding (header, body)
import Control.Lens
import Data.Aeson
import Data.ByteString.Conversion
import Data.Proxy
import Data.Tagged
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml (decodeFileEither)
import Galley.Options
import Network.HTTP.Client (responseTimeoutMicro)
import Network.HTTP.Client.TLS
import OpenSSL (withOpenSSL)
import Options.Applicative
import Test.Tasty
import Test.Tasty.Options
import Util.Options
import Util.Options.Common
import Util.Test

import qualified API
import qualified API.Util               as Util
import qualified API.SQS                as SQS
import qualified Data.ByteString.Char8  as BS

data IntegrationConfig = IntegrationConfig
  -- internal endpoints
  { galley    :: Endpoint
  , brig      :: Endpoint
  , cannon    :: Endpoint
  } deriving (Show, Generic)

instance FromJSON IntegrationConfig

newtype ServiceConfigFile = ServiceConfigFile String
    deriving (Eq, Ord, Typeable)

instance IsOption ServiceConfigFile where
    defaultValue = ServiceConfigFile "/etc/wire/galley/conf/galley.yaml"
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
    go g i = withResource (getOpts g i) releaseOpts $ API.tests
    getOpts gFile iFile = do
        m <- newManager tlsManagerSettings {managerResponseTimeout = responseTimeoutMicro 300000000}
        let local p = Endpoint {_epHost = "127.0.0.1", _epPort = p}
        gConf <- handleParseError =<< decodeFileEither gFile
        iConf <- handleParseError =<< decodeFileEither iFile
        g <- mkRequest <$> optOrEnv galley iConf (local . read) "GALLEY_WEB_PORT"
        b <- mkRequest <$> optOrEnv brig iConf (local . read) "BRIG_WEB_PORT"
        c <- mkRequest <$> optOrEnv cannon iConf (local . read) "CANNON_WEB_PORT"
        -- unset this env variable in galley's config to disable testing SQS team events
        q <- join <$> optOrEnvSafe queueName gConf (Just . pack) "GALLEY_SQS_TEAM_EVENTS"
        e <- join <$> optOrEnvSafe endpoint gConf (fromByteString . BS.pack) "GALLEY_SQS_ENDPOINT"
        convMaxSize <- optOrEnv maxSize gConf read "CONV_MAX_SIZE"
        awsEnv <- initAwsEnv e q
        SQS.ensureQueueEmpty awsEnv
        return $ Util.TestSetup m g b c awsEnv convMaxSize

    queueName = fmap (view awsQueueName) . view optJournal
    endpoint = fmap (view awsEndpoint) . view optJournal
    maxSize = view (optSettings.setMaxConvSize)

    initAwsEnv (Just e) (Just q) = Just <$> SQS.mkAWSEnv (JournalOpts q e)
    initAwsEnv _ _               = return Nothing

    releaseOpts _ = return ()

    mkRequest (Endpoint h p) = host (encodeUtf8 h) . port p
