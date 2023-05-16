{-# LANGUAGE DisambiguateRecordFields #-}

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
  stopped <- newEmptyMVar
  runWithRabbitMq env.logger opts.rabbitmq $
    RabbitMqHooks
      { onNewChannel = BackendNotificationPusher.startWorker env opts.remoteDomains,
        onGracefulStop = putMVar stopped (),
        onException = const $ pure ()
      }
  takeMVar stopped

data RabbitMqHooks = RabbitMqHooks
  { onGracefulStop :: IO (),
    onException :: SomeException -> IO (),
    onNewChannel :: Q.Channel -> IO ()
  }

-- TODO(elland): Find out if there are benefits of having one channel for everything
-- or should we create more channels?
runWithRabbitMq :: Logger -> RabbitMqOpts -> RabbitMqHooks -> IO ()
runWithRabbitMq l opts hooks = do
  username <- Text.pack <$> getEnv "RABBITMQ_USERNAME"
  password <- Text.pack <$> getEnv "RABBITMQ_PASSWORD"
  connect username password
  where
    connect username password = do
      conn <- Q.openConnection' opts.host (fromIntegral opts.port) opts.vHost username password
      Q.addConnectionClosedHandler conn True (connect username password)
      openChan conn

    openChan conn = do
      Log.info l $ Log.msg (Log.val "Opening channel with RabbitMQ")
      chan <- Q.openChannel conn
      Q.addChannelExceptionHandler chan (handler conn)
      Log.info l $ Log.msg (Log.val "RabbitMQ channel opened")
      hooks.onNewChannel chan
    handler conn e =
      if
          | Q.isNormalChannelClose e -> do
              Log.info l $ Log.msg (Log.val "RabbitMQ channel gracefully closed, not retrying to open channel")
              hooks.onGracefulStop
          | isConnectionClosed e -> do
              -- if ConnectionClosedException, do not reconnect channel
              Log.info l $ Log.msg (Log.val "RabbitMQ connection closed, not retrying to open channel")
          | otherwise -> do
              Log.err l $ Log.msg (Log.val "RabbitMQ channel closed with an exception") . Log.field "error" (displayException e)
              hooks.onException e
              openChan conn

isConnectionClosed :: SomeException -> Bool
isConnectionClosed (fromException -> Just (Q.ConnectionClosedException {})) = True
isConnectionClosed _ = False
