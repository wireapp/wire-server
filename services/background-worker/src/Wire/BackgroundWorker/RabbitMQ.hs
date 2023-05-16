{-# LANGUAGE NumericUnderscores #-}

module Wire.BackgroundWorker.RabbitMQ where

import Control.Monad.Catch
import Control.Monad.Trans.Control
import Control.Retry
import qualified Data.Text as Text
import Imports
import qualified Network.AMQP as Q
import System.Logger.Extended (Logger)
import qualified System.Logger.Extended as Log
import Wire.BackgroundWorker.Options

data RabbitMqHooks m = RabbitMqHooks
  { onGracefulStop :: m (),
    onException :: SomeException -> m (),
    onNewChannel :: Q.Channel -> m ()
  }

runWithRabbitMq ::
  forall m.
  (MonadIO m, MonadMask m, MonadBaseControl IO m) =>
  Logger ->
  RabbitMqOpts ->
  RabbitMqHooks m ->
  m ()
runWithRabbitMq l opts hooks = do
  username <- liftIO $ Text.pack <$> getEnv "RABBITMQ_USERNAME"
  password <- liftIO $ Text.pack <$> getEnv "RABBITMQ_PASSWORD"
  connectWithRetries username password
  where
    connectWithRetries :: Text -> Text -> m ()
    connectWithRetries username password = do
      -- Jittered exponential backoff with 1ms as starting delay and 5s as max
      -- delay.
      let policy = capDelay 5_000_000 $ fullJitterBackoff 1000
          logError willRetry e retryStatus = do
            Log.err l $
              Log.msg (Log.val "Failed to connect to RabbitMQ")
                . Log.field "error" (displayException @SomeException e)
                . Log.field "willRetry" willRetry
                . Log.field "retryCount" retryStatus.rsIterNumber
      recovering
        policy
        [logRetries (const $ pure True) logError]
        ( const $ do
            Log.info l $ Log.msg (Log.val "Trying to connect to RabbitMQ")
            connect username password
        )

    connect :: Text -> Text -> m ()
    connect username password = do
      conn <- liftIO $ Q.openConnection' opts.host (fromIntegral opts.port) opts.vHost username password
      liftBaseWith $ \runInIO ->
        Q.addConnectionClosedHandler conn True (void $ runInIO $ connectWithRetries username password)
      openChan conn

    openChan :: Q.Connection -> m ()
    openChan conn = do
      Log.info l $ Log.msg (Log.val "Opening channel with RabbitMQ")
      chan <- liftIO $ Q.openChannel conn
      liftBaseWith $ \runInIO ->
        Q.addChannelExceptionHandler chan (void . runInIO . chanExceptionHandler conn)
      Log.info l $ Log.msg (Log.val "RabbitMQ channel opened")
      hooks.onNewChannel chan

    chanExceptionHandler :: Q.Connection -> SomeException -> m ()
    chanExceptionHandler conn e =
      if
          | Q.isNormalChannelClose e -> do
              Log.info l $
                Log.msg (Log.val "RabbitMQ channel gracefully closed, not retrying to open channel")
                  . Log.field "error" (displayException e)
              hooks.onGracefulStop
          | isConnectionClosed e -> do
              Log.info l $
                Log.msg (Log.val "RabbitMQ connection closed, not retrying to open channel")
                  . Log.field "error" (displayException e)
          | otherwise -> do
              Log.err l $
                Log.msg (Log.val "RabbitMQ channel closed with an exception")
                  . Log.field "error" (displayException e)
              hooks.onException e
              openChan conn

isConnectionClosed :: SomeException -> Bool
isConnectionClosed (fromException -> Just (Q.ConnectionClosedException {})) = True
isConnectionClosed _ = False
