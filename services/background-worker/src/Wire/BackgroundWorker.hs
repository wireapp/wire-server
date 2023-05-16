{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NumericUnderscores #-}

module Wire.BackgroundWorker where

import Control.Monad.Catch
import Control.Monad.Trans.Control
import Control.Retry
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
      { onNewChannel = runAppT env . BackendNotificationPusher.startWorker opts.remoteDomains,
        onGracefulStop = putMVar stopped (),
        onException = const $ pure ()
      }
  takeMVar stopped

data RabbitMqHooks m = RabbitMqHooks
  { onGracefulStop :: m (),
    onException :: SomeException -> m (),
    onNewChannel :: Q.Channel -> m ()
  }

-- TODO(elland): Find out if there are benefits of having one channel for everything
-- or should we create more channels?
runWithRabbitMq :: forall m. (MonadIO m, MonadMask m, MonadBaseControl IO m) => Logger -> RabbitMqOpts -> RabbitMqHooks m -> m ()
runWithRabbitMq l opts hooks = do
  username <- liftIO $ Text.pack <$> getEnv "RABBITMQ_USERNAME"
  password <- liftIO $ Text.pack <$> getEnv "RABBITMQ_PASSWORD"
  connectWithRetries username password
  where
    connectWithRetries :: Text -> Text -> m ()
    connectWithRetries username password = do
      let policy = capDelay 5_000_000 $ fullJitterBackoff 1000
      recoverAll
        policy
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
        Q.addChannelExceptionHandler chan (void . runInIO . handler conn)
      Log.info l $ Log.msg (Log.val "RabbitMQ channel opened")
      hooks.onNewChannel chan
    handler conn e =
      if
          | Q.isNormalChannelClose e -> do
              Log.info l $ Log.msg (Log.val "RabbitMQ channel gracefully closed, not retrying to open channel")
              hooks.onGracefulStop
          | isConnectionClosed e -> do
              Log.info l $ Log.msg (Log.val "RabbitMQ connection closed, not retrying to open channel")
          | otherwise -> do
              Log.err l $ Log.msg (Log.val "RabbitMQ channel closed with an exception") . Log.field "error" (displayException e)
              hooks.onException e
              openChan conn

isConnectionClosed :: SomeException -> Bool
isConnectionClosed (fromException -> Just (Q.ConnectionClosedException {})) = True
isConnectionClosed _ = False
