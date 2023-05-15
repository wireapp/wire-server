module Wire.BackgroundWorker where

import qualified Data.Text as Text
import Imports
import qualified Network.AMQP as Q
import System.Logger.Extended (Logger)
import qualified System.Logger.Extended as Log
import qualified Wire.BackendNotificationPusher as BackendNotificationPusher
import Wire.BackgroundWorker.Env
import Wire.BackgroundWorker.Options

-- TODO(elland): Start an http service with status and metrics endpoints
run :: Opts -> IO ()
run opts = do
  env <- mkEnv opts
  -- FUTUREWORK: Make some way to tracking all the workers, currently there is
  -- only one so we can just block on it.
  withRabbitMqChannel env.logger opts.rabbitmq $
    RabbitMqHooks
      { onNewChannel = BackendNotificationPusher.startWorker env opts.remoteDomains,
        onGracefulStop = pure (),
        onException = const $ pure ()
      }

data RabbitMqHooks = RabbitMqHooks
  { onGracefulStop :: IO (),
    onException :: SomeException -> IO (),
    onNewChannel :: Q.Channel -> IO ()
  }

-- TODO(elland): Find out if there are benefits of having one channel for everything
-- or should we create more channels?
-- rabbitmqChannel :: IORef Q.Channel,
withRabbitMqChannel :: Logger -> RabbitMqOpts -> RabbitMqHooks -> IO ()
withRabbitMqChannel l opts hooks = do
  username <- Text.pack <$> getEnv "RABBITMQ_USERNAME"
  password <- Text.pack <$> getEnv "RABBITMQ_PASSWORD"
  connect username password
  forever $ threadDelay maxBound
  where
    connect username password = do
      conn <- Q.openConnection' opts.host (fromIntegral opts.port) opts.vHost username password
      Log.info l $ Log.msg (Log.val "RabbitMQ connection established")
      chan <- Q.openChannel conn
      -- TODO(elland): Q.addConnectionClosedHandler
      -- TODO(elland): Q.addConnectionBlockedHandler (Probably not required: https://www.rabbitmq.com/connection-blocked.html)
      Q.addChannelExceptionHandler chan (handler username password)
      Log.info l $ Log.msg (Log.val "RabbitMQ channel opened")
      hooks.onNewChannel chan
    handler username password e =
      if Q.isNormalChannelClose e
        then do
          Log.info l $ Log.msg (Log.val "RabbitMQ channel gracefully closed")
          hooks.onGracefulStop
        else do
          Log.err l $ Log.msg (Log.val "RabbitMQ channel closed with an exception") . Log.field "error" (displayException e)
          hooks.onException e
          connect username password
