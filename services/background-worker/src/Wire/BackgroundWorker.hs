{-# LANGUAGE DisambiguateRecordFields #-}

module Wire.BackgroundWorker where

import Control.Concurrent.Async
import Control.Concurrent.Chan
import Imports
import Network.AMQP.Extended
import Wire.API.Routes.FederationDomainConfig
import qualified Wire.BackendNotificationPusher as BackendNotificationPusher
import Wire.BackgroundWorker.Env
import Wire.BackgroundWorker.Options
import Wire.Defederation

-- FUTUREWORK: Start an http service with status and metrics endpoints
run :: Opts -> IO ()
run opts = do
  (env, syncThread) <- mkEnv opts
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
          deleteThread <- deleteWorker chan

          -- Since this is feeding off a Rabbit queue, it should
          -- be safe to kill and start these threads. At worst, we
          -- will double deliver some messages
          pushThread <- do
            -- get an initial list of domains from the IORef
            initRemotes <- liftIO $ readIORef env.remoteDomains
            -- Start the notification pusher using the initial domains
            thread <- BackendNotificationPusher.startWorker (domain <$> initRemotes.remotes) chan
            let go asyncThread = do
                  -- Wait for a new set of domains from the Chan
                  remotes <- liftIO $ readChan $ env.remoteDomainsChan
                  -- When we have new domains, kill the previous pusher thread
                  liftIO $ cancel asyncThread
                  -- Start a new pusher thread and then wait for new domains
                  -- TODO: There is a nicer way of doing this using
                  go =<< BackendNotificationPusher.startWorker (domain <$> remotes.remotes) chan
            go thread

          let threads = [pushThread, deleteThread]
          -- Write out the handles for the threads
          atomicWriteIORef threadsRef threads
          -- Wait for all the threads. This shouldn't occure
          -- as the threads all have `forever $ threadDelay ...`
          liftIO $ traverse_ wait threads
          -- clear the threadRef if the threads finish
          atomicWriteIORef threadsRef [],
        -- FUTUREWORK: Use these for metrics
        --
        -- When the channel dies for whatever reason, kill all of the async
        -- threads and clean up the threadsRef state
        onChannelException = const cancelThreads,
        onConnectionClose = cancelThreads
      }
  void $ forever $ threadDelay maxBound
  liftIO $ cancel syncThread
