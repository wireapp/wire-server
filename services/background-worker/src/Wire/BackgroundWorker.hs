{-# LANGUAGE DisambiguateRecordFields #-}

module Wire.BackgroundWorker where

import Imports
import Network.AMQP.Extended
import qualified Wire.BackendNotificationPusher as BackendNotificationPusher
import Wire.BackgroundWorker.Env
import Wire.BackgroundWorker.Options
import Wire.Defederation
import Control.Concurrent.Async

-- FUTUREWORK: Start an http service with status and metrics endpoints
run :: Opts -> IO ()
run opts = do
  env <- mkEnv opts
  threadsRef <- newIORef []
  let cancelThreads = do
        -- Kill all of the threads and clean up the IORef
        threads <- readIORef threadsRef
        traverse_ cancel threads
        atomicWriteIORef threadsRef []
  openConnectionWithRetries env.logger opts.rabbitmq.host opts.rabbitmq.port opts.rabbitmq.vHost $
    RabbitMqHooks
      { onNewChannel = \chan -> runAppT env $ do
          -- Channels are threadsafe: https://hackage.haskell.org/package/amqp-0.22.1/docs/Network-AMQP.html
          -- So we can async them for concurrency.
          pushThread <- BackendNotificationPusher.startWorker opts.remoteDomains chan
          deleteThread <- deleteWorker chan
          let threads = [pushThread, deleteThread]
          -- Write out the handles for the threads
          atomicWriteIORef threadsRef threads
          -- Wait for all the threads. This shouldn't occure
          -- as the threads all have `forever $ threadDelay ...`
          liftIO $ traverse_ wait threads
          -- clear the threadRef if the threads finish
          atomicWriteIORef threadsRef []
        -- FUTUREWORK: Use these for metrics
        --
        -- When the channel dies for whatever reason, kill all of the async
        -- threads and clean up the threadsRef state
        , onChannelException = const cancelThreads
        , onConnectionClose = cancelThreads
      }
  forever $ threadDelay maxBound
