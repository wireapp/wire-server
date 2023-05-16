{-# LANGUAGE DisambiguateRecordFields #-}

module Wire.BackgroundWorker where

import Imports
import Network.AMQP.Extended
import qualified Wire.BackendNotificationPusher as BackendNotificationPusher
import Wire.BackgroundWorker.Env
import Wire.BackgroundWorker.Options

-- TODO(elland): Start an http service with status and metrics endpoints
run :: Opts -> IO ()
run opts = do
  env <- mkEnv opts
  -- FUTUREWORK: Make some way to tracking all the workers, currently there is
  -- only one so we can just block on it.
  openConnectionWithRetries env.logger opts.rabbitmq.host opts.rabbitmq.port opts.rabbitmq.vHost $
    RabbitMqHooks
      { onNewChannel = runAppT env . BackendNotificationPusher.startWorker opts.remoteDomains,
        -- TODO: Use these for metrics
        onChannelException = const $ pure (),
        onConnectionClose = pure ()
      }
  forever $ threadDelay maxBound
