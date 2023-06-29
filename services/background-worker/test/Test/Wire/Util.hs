{-# LANGUAGE RecordWildCards #-}

module Test.Wire.Util where

import Control.Concurrent.Chan
import Imports
import Network.HTTP.Client
import qualified System.Logger.Class as Logger
import Util.Options
import Wire.API.Routes.FederationDomainConfig
import Wire.BackgroundWorker.Env hiding (federatorInternal, galley)
import qualified Wire.BackgroundWorker.Env as E
import Wire.BackgroundWorker.Util

runTestAppT :: AppT IO a -> Int -> IO a
runTestAppT app port = do
  http2Manager <- initHttp2Manager
  httpManager <- newManager defaultManagerSettings
  logger <- Logger.new Logger.defSettings
  remoteDomains <- newIORef defFederationDomainConfigs
  remoteDomainsChan <- newChan
  let federatorInternal = Endpoint "localhost" (fromIntegral port)
      galley = Endpoint "localhost" undefined -- TODO
      brig = Endpoint "localhost" undefined -- TODO
      defederationTimeout = responseTimeoutNone
      rabbitmqAdminClient = undefined
      rabbitmqVHost = undefined
      env = Env {..}
  runAppT env app

data FakeEnvelope = FakeEnvelope
  { rejections :: IORef [Bool],
    acks :: IORef Int
  }

newFakeEnvelope :: IO FakeEnvelope
newFakeEnvelope =
  FakeEnvelope
    <$> newIORef []
    <*> newIORef 0

instance RabbitMQEnvelope FakeEnvelope where
  ack e = atomicModifyIORef' e.acks $ \a -> (a + 1, ())
  reject e requeueFlag = atomicModifyIORef' e.rejections $ \r -> (r <> [requeueFlag], ())
