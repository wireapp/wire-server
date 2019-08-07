module Main (main) where

import Imports hiding (local)

import Bilge hiding (header, body)
import Cassandra.Util
import Control.Lens
import Data.ByteString.Conversion
import Data.Metrics.Test (pathsConsistencyCheck)
import Data.Metrics.WaiRoute (treeToPaths)
import Data.Proxy
import Data.Tagged
import Data.Text.Encoding (encodeUtf8)
import Data.Text (pack)
import Data.Yaml (decodeFileEither)
import Galley.API (sitemap)
import Galley.Options
import Network.HTTP.Client (responseTimeoutMicro)
import Network.HTTP.Client.TLS
import Network.Wai.Utilities.Server (compile)
import OpenSSL (withOpenSSL)
import Options.Applicative
import TestSetup
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Options
import Util.Options
import Util.Options.Common
import Util.Test

import qualified API
import qualified API.SQS                as SQS
import qualified Data.ByteString.Char8  as BS
import qualified System.Logger.Class    as Logger

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
    go g i = withResource (getOpts g i) releaseOpts $ \setup -> testGroup "galley"
        [ testCase "sitemap" $ assertEqual "inconcistent sitemap"
            mempty
            (pathsConsistencyCheck . treeToPaths . compile $ Galley.API.sitemap)
        , API.tests setup
        ]

    getOpts gFile iFile = do
        m <- newManager tlsManagerSettings {managerResponseTimeout = responseTimeoutMicro 300000000}
        let local p = Endpoint {_epHost = "127.0.0.1", _epPort = p}
        gConf <- handleParseError =<< decodeFileEither gFile
        iConf <- handleParseError =<< decodeFileEither iFile
        -- FUTUREWORK: we don't support process env setup any more, so both gconf and iConf
        -- must be 'Just'.  the following code could be simplified a lot, but this should
        -- probably happen after (or at least while) unifying the integration test suites into
        -- a single library.
        g <- mkRequest <$> optOrEnv galley iConf (local . read) "GALLEY_WEB_PORT"
        b <- mkRequest <$> optOrEnv brig iConf (local . read) "BRIG_WEB_PORT"
        c <- mkRequest <$> optOrEnv cannon iConf (local . read) "CANNON_WEB_PORT"
        -- unset this env variable in galley's config to disable testing SQS team events
        q <- join <$> optOrEnvSafe queueName gConf (Just . pack) "GALLEY_SQS_TEAM_EVENTS"
        e <- join <$> optOrEnvSafe endpoint gConf (fromByteString . BS.pack) "GALLEY_SQS_ENDPOINT"
        convMaxSize <- optOrEnv maxSize gConf read "CONV_MAX_SIZE"
        awsEnv <- initAwsEnv e q
        SQS.ensureQueueEmptyIO awsEnv

        -- Initialize cassandra
        let ch = fromJust gConf ^. optCassandra.casEndpoint.epHost
        let cp = fromJust gConf ^. optCassandra.casEndpoint.epPort
        let ck = fromJust gConf ^. optCassandra.casKeyspace

        lg <- Logger.new Logger.defSettings
        db <- defInitCassandra ck ch cp lg

        return $ TestSetup (fromJust gConf) (fromJust iConf) m g b c awsEnv convMaxSize db

    queueName = fmap (view awsQueueName) . view optJournal
    endpoint = fmap (view awsEndpoint) . view optJournal
    maxSize = view (optSettings.setMaxConvSize)

    initAwsEnv (Just e) (Just q) = Just <$> SQS.mkAWSEnv (JournalOpts q e)
    initAwsEnv _ _               = return Nothing

    releaseOpts _ = return ()

    mkRequest (Endpoint h p) = host (encodeUtf8 h) . port p
