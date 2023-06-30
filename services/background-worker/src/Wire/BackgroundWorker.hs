module Wire.BackgroundWorker where

import Control.Monad.Catch
import Imports
import Network.AMQP.Extended
import Network.Wai.Utilities.Server
import Servant
import Servant.Server.Generic
import UnliftIO.Async
import Util.Options
import qualified Wire.BackendNotificationPusher as BackendNotificationPusher
import Wire.BackgroundWorker.Env
import qualified Wire.BackgroundWorker.Health as Health
import Wire.BackgroundWorker.Options
import Wire.Defederation

-- FUTUREWORK: Start an http service with status and metrics endpoints
-- NOTE: Use atomic IORef writes to impose an ordering barrier on
--       reads and writes. This stops the CPU from being too clever
--       with its memory model and what it thinks it can get away with.
run :: Opts -> IO ()
run opts = do
  (env, syncThread) <- mkEnv opts
  -- We can fire and forget this thread because it keeps respawning itself using the
  -- 'onConnectionClosedHandler'
  void $ async $
    -- Don't leave the sync thread dangling
    handle (\e@AsyncCancelled -> cancel syncThread >> throwM e) $ do
      threadsRef <- newIORef []
      let cancelThreads = do
            -- Kill all of the threads and clean up the IORef
            -- The threads should handle the cleanup of their AMQP consumers.
            threads <- readIORef threadsRef
            traverse_ cancel threads
            atomicWriteIORef threadsRef []
          onChanClose = do
            runAppT env $ do
              markAsNotWorking BackendNotificationPusher
              markAsNotWorking DefederationWorker
            cancelThreads
      openConnectionWithRetries
        env.logger
        (demoteOpts opts.rabbitmq)
        $ RabbitMqHooks
          { -- If the function in onNewChannel throws an exception it will bubble up the stack as this is OUTSIDE of the
            -- connection and channel error handling. This will kill the pod, which should be restarted by kubernetes.
            onNewChannel = \chan -> runAppT env $ do
              -- Channels are threadsafe: https://hackage.haskell.org/package/amqp-0.22.1/docs/Network-AMQP.html
              -- So we can async them for concurrency.
              deleteThread <- deleteWorker chan

              -- Since this is feeding off a Rabbit queue, it should
              -- be safe to kill and start these threads. At worst, we
              -- will double deliver some messages
              pushThread <- BackendNotificationPusher.startWorker chan

              let threads = [pushThread, deleteThread]
              -- Write out the handles for the threads
              atomicWriteIORef threadsRef threads
              -- Wait for all the threads. This shouldn't occure
              -- as the threads all have `forever $ threadDelay ...`
              liftIO $ traverse_ wait threads,
            -- FUTUREWORK: Use these for metrics
            --
            -- When the channel dies for whatever reason, kill all of the async
            -- threads and clean up the threadsRef state
            onChannelException = const onChanClose,
            onConnectionClose = onChanClose
          }
  let server = defaultServer (cs $ opts.backgroundWorker._epHost) opts.backgroundWorker._epPort env.logger env.metrics
  settings <- newSettings server
  runSettingsWithShutdown settings (servantApp env) Nothing

servantApp :: Env -> Application
servantApp = genericServe . Health.api
