module Main (main) where

import qualified API
import qualified API.SQS as SQS
import Bilge hiding (body, header)
import Cassandra.Util
import Control.Lens
import Data.Metrics.Test (pathsConsistencyCheck)
import Data.Metrics.WaiRoute (treeToPaths)
import Data.Proxy
import Data.Tagged
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml (decodeFileThrow)
import Galley.API (sitemap)
import Galley.Options
import Imports hiding (local)
import Network.HTTP.Client (responseTimeoutMicro)
import Network.HTTP.Client.TLS
import Network.Wai.Utilities.Server (compile)
import OpenSSL (withOpenSSL)
import Options.Applicative
import qualified System.Logger.Class as Logger
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Options
import TestSetup
import Util.Options
import Util.Test

newtype ServiceConfigFile = ServiceConfigFile String
  deriving (Eq, Ord, Typeable)

instance IsOption ServiceConfigFile where
  defaultValue = ServiceConfigFile "/etc/wire/galley/conf/galley.yaml"
  parseValue = fmap ServiceConfigFile . safeRead
  optionName = return "service-config"
  optionHelp = return "Service config file to read from"
  optionCLParser =
    fmap ServiceConfigFile $ strOption $
      ( short (untag (return 's' :: Tagged ServiceConfigFile Char))
          <> long (untag (optionName :: Tagged ServiceConfigFile String))
          <> help (untag (optionHelp :: Tagged ServiceConfigFile String))
      )

runTests :: (String -> String -> TestTree) -> IO ()
runTests run = defaultMainWithIngredients ings
  $ askOption
  $ \(ServiceConfigFile c) ->
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
    go g i = withResource (getOpts g i) releaseOpts $ \setup ->
      testGroup
        "galley"
        [ testCase "sitemap" $
            assertEqual
              "inconcistent sitemap"
              mempty
              (pathsConsistencyCheck . treeToPaths . compile $ Galley.API.sitemap),
          API.tests setup
        ]
    getOpts :: FilePath -> FilePath -> IO TestSetup
    getOpts gFile iFile = do
      m <- newManager tlsManagerSettings {managerResponseTimeout = responseTimeoutMicro 300000000}
      gConf :: Opts <- decodeFileThrow gFile
      iConf :: IntegrationConfig <- decodeFileThrow iFile
      let g = mkRequest (galley iConf)
      let b = mkRequest (brig iConf)
      let c = mkRequest (cannon iConf)
      let federatedBrig1' = mkRequest (federatedBrig1 iConf)
      let federatedBrig2' = mkRequest (federatedBrig2 iConf)
      let federatedBackend1' = mkRequest (federatedBackend1 iConf)
      let federatedBackend2' = mkRequest (federatedBackend2 iConf)
      let q = queueName gConf
      let e = endpoint gConf
      let convMaxSize = maxSize gConf
      awsEnv <- initAwsEnv e q
      SQS.ensureQueueEmptyIO awsEnv
      -- Initialize cassandra
      let ch = gConf ^. optCassandra . casEndpoint . epHost
      let cp = gConf ^. optCassandra . casEndpoint . epPort
      let ck = gConf ^. optCassandra . casKeyspace
      lg <- Logger.new Logger.defSettings
      db <- defInitCassandra ck ch cp lg
      return $ TestSetup gConf iConf m g b c awsEnv convMaxSize db federatedBrig1' federatedBrig2' federatedBackend1' federatedBackend2'
    queueName = fmap (view awsQueueName) . view optJournal
    endpoint = fmap (view awsEndpoint) . view optJournal
    maxSize = view (optSettings . setMaxConvSize)
    initAwsEnv (Just e) (Just q) = Just <$> SQS.mkAWSEnv (JournalOpts q e)
    initAwsEnv _ _ = return Nothing
    releaseOpts _ = return ()
    mkRequest (Endpoint h p) = host (encodeUtf8 h) . port p
