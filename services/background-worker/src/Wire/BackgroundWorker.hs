{-# LANGUAGE DisambiguateRecordFields #-}

module Wire.BackgroundWorker where

import Imports
import qualified Wire.BackendNotificationPusher as BackendNotificationPusher
import Wire.BackgroundWorker.Env
import Wire.BackgroundWorker.Options
import Wire.BackgroundWorker.RabbitMQ

-- TODO(elland): Start an http service with status and metrics endpoints
run :: Opts -> IO ()
run opts = do
  env <- mkEnv opts
  -- FUTUREWORK: Make some way to tracking all the workers, currently there is
  -- only one so we can just block on it.
  stopped <- newEmptyMVar
  runWithRabbitMq env.logger opts.rabbitmq $
    RabbitMqHooks
      { onNewChannel = runAppT env . BackendNotificationPusher.startWorker opts.remoteDomains,
        onGracefulStop = putMVar stopped (),
        onException = const $ pure ()
      }
  takeMVar stopped
