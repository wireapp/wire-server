{-# LANGUAGE BlockArguments #-}

module Wire.BackgroundWorker where

import Data.Domain
import Data.Map.Strict qualified as Map
import Data.Metrics.Servant qualified as Metrics
import Data.Text qualified as T
import Imports
import Network.AMQP qualified as Q
import Network.AMQP.Extended (demoteOpts)
import Network.Wai.Utilities.Server
import Servant
import Servant.Server.Generic
import System.Logger qualified as Log
import Util.Options
import Wire.BackendNotificationPusher qualified as BackendNotificationPusher
import Wire.BackgroundWorker.Env
import Wire.BackgroundWorker.Health qualified as Health
import Wire.BackgroundWorker.Options

run :: Opts -> IO ()
run opts = do
  env <- mkEnv opts
  let amqpEP = either id demoteOpts opts.rabbitmq.unRabbitMqOpts
  (notifChanRef, notifConsumersRef) <- runAppT env $ BackendNotificationPusher.startWorker amqpEP
  let -- cleanup will run in a new thread when the signal is caught, so we need to use IORefs and
      -- specific exception types to message threads to clean up
      l = logger env
      cleanup = do
        -- Notification pusher thread
        Log.info l $ Log.msg (Log.val "Cancelling the notification pusher thread")
        readIORef notifChanRef >>= traverse_ \chan -> do
          Log.info l $ Log.msg (Log.val "Got channel")
          readIORef notifConsumersRef >>= \m -> for_ (Map.assocs m) \(domain, (consumer, runningFlag)) -> do
            Log.info l $ Log.msg (Log.val "Cancelling consumer") . Log.field "Domain" domain._domainText
            -- Remove the consumer from the channel so it isn't called again
            Q.cancelConsumer chan consumer
            -- Take from the mvar. This will only unblock when the consumer callback isn't running.
            -- This allows us to wait until the currently running tasks are completed, and new ones
            -- won't be scheduled because we've already removed the callback from the channel.
            -- If, for some reason, a consumer is invoked after us cancelling it, taking this MVar
            -- will block that thread from trying to push out the notification. At this point, we're
            -- relying on Rabbit to requeue the message for us as we won't be able to ACK or NACK it.
            -- This helps prevent message redelivery to endpoint services during the brief window between
            -- receiving a message from rabbit, and the signal handler shutting down the AMQP connection
            -- before notification delivery has finalised.
            Log.info l $ Log.msg $ Log.val "Taking MVar. Waiting for current operation to finish"
            takeMVar runningFlag
          -- Close the channel. `extended` will then close the connection, flushing messages to the server.
          Log.info l $ Log.msg $ Log.val "Closing RabbitMQ channel"
          Q.closeChannel chan
  let server = defaultServer (T.unpack $ opts.backgroundWorker._host) opts.backgroundWorker._port env.logger
  settings <- newSettings server
  -- Additional cleanup when shutting down via signals.
  runSettingsWithCleanup cleanup settings (servantApp env) Nothing

servantApp :: Env -> Application
servantApp env =
  Metrics.servantPrometheusMiddleware (Proxy @(ToServant Health.HealthAPI AsApi)) $
    genericServe $
      Health.api env
