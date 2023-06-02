module Network.AMQP.Extended where

import Control.Monad.Catch
import Control.Monad.Trans.Control
import Control.Retry
import qualified Data.Text as Text
import Imports
import qualified Network.AMQP as Q
import System.Logger (Logger)
import qualified System.Logger as Log

data RabbitMqHooks m = RabbitMqHooks
  { -- | Called whenever there is a new channel. At any time there should be at
    -- max 1 open channel. Perhaps this would need to change in future.
    onNewChannel :: Q.Channel -> m (),
    -- | Called when connection is closed. Any exceptions thrown by this would
    -- be logged and ignored.
    onConnectionClose :: m (),
    -- | Called when the channel is closed. Any exceptions thrown by this would
    -- be logged and ignored.
    onChannelException :: SomeException -> m ()
  }

-- | Connects with RabbitMQ and opens a channel. If the channel is closed for
-- some reasons, reopens the channel. If the connection is closed for some
-- reasons, keeps retrying to connect until it works.
openConnectionWithRetries ::
  forall m.
  (MonadIO m, MonadMask m, MonadBaseControl IO m) =>
  Logger ->
  String ->
  Int ->
  Text ->
  RabbitMqHooks m ->
  m ()
openConnectionWithRetries l host port vHost hooks = do
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
      conn <- liftIO $ Q.openConnection' host (fromIntegral port) vHost username password
      liftBaseWith $ \runInIO ->
        Q.addConnectionClosedHandler conn True $ void $ runInIO $ do
          hooks.onConnectionClose
            `catch` logException l "onConnectionClose hook threw an exception, reconnecting to RabbitMQ anyway"
          connectWithRetries username password
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
    chanExceptionHandler conn e = do
      logException l "RabbitMQ channel closed" e
      hooks.onChannelException e `catch` logException l "onChannelException hook threw an exception"
      case (Q.isNormalChannelClose e, fromException e) of
        (True, _) ->
          Log.info l $
            Log.msg (Log.val "RabbitMQ channel is closed normally, not attempting to reopen channel")
        (_, Just (Q.ConnectionClosedException {})) ->
          Log.info l $
            Log.msg (Log.val "RabbitMQ connection is closed, not attempting to reopen channel")
        _ -> openChan conn

logException :: (MonadIO m) => Logger -> String -> SomeException -> m ()
logException l m (SomeException e) = do
  Log.err l $
    Log.msg m
      . Log.field "error" (displayException e)
