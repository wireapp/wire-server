{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Main (main) where

import Bilge hiding (header, body)
import Control.Lens
import Control.Monad (join)
import Data.Aeson
import Data.Monoid
import Data.Proxy
import Data.Tagged
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable
import Data.Yaml (decodeFileEither)
import GHC.Generics
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
import qualified API.Util as Util
import qualified API.SQS  as SQS
import qualified System.Posix.Env as Posix

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
        m <- newManager tlsManagerSettings {
            managerResponseTimeout = responseTimeoutMicro 300000000
        }
        let local p = Endpoint { _epHost = "127.0.0.1", _epPort = p }
        gConf <- handleParseError =<< decodeFileEither gFile
        iConf <- handleParseError =<< decodeFileEither iFile
        g <- mkRequest <$> optOrEnv galley iConf (local . read) "GALLEY_WEB_PORT"
        b <- mkRequest <$> optOrEnv brig iConf (local . read) "BRIG_WEB_PORT"
        c <- mkRequest <$> optOrEnv cannon iConf (local . read) "CANNON_WEB_PORT"
        sHost <- join <$> optOrEnvSafe (\v -> v^?optJournal.traverse.awsFakeSqs.traverse.sqsHost) gConf (Just . pack) "FAKE_SQS_HOST"
        sPort <- join <$> optOrEnvSafe (\v -> v^?optJournal.traverse.awsFakeSqs.traverse.sqsPort) gConf (Just . read) "FAKE_SQS_PORT"
        -- unset this env variable in galley's config to disable testing SQS team events
        q <- join <$> optOrEnvSafe handleJournal gConf (Just . pack) "GALLEY_SQS_TEAM_EVENTS"
        awsEnv <- maybe (return Nothing) (fmap Just . SQS.mkAWSEnv (sqsConf sHost sPort)) q

        return $ Util.TestSetup m g b c awsEnv

    -- if fake sqs host/port are passed as config/ENV variables, use fake sqs; otherwise use AWS SQS
    sqsConf (Just h) (Just p) = Just $ FakeSQSOpts h p
    sqsConf _ _               = Nothing

    handleJournal = maybe Nothing (Just . view awsQueueName) . view optJournal

    releaseOpts _ = return ()

    mkRequest (Endpoint h p) = host (encodeUtf8 h) . port p

-- similar to optOrEnv, except return None if an environment variable is not defined
optOrEnvSafe :: (a -> b) -> Maybe a -> (String -> b) -> String -> IO (Maybe b)
optOrEnvSafe getter conf reader var = case conf of
    Nothing -> fmap reader <$>Posix.getEnv var
    Just c  -> pure $ Just (getter c)
