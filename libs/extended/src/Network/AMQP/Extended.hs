{-# LANGUAGE RecordWildCards #-}

module Network.AMQP.Extended where

import Control.Exception (throwIO)
import Control.Monad.Catch
import Control.Monad.Trans.Control
import Control.Retry
import Data.Aeson
import Data.Proxy
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Imports
import qualified Network.AMQP as Q
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.OpenSSL as HTTP
import Network.RabbitMqAdmin
import OpenSSL.Session (SSLOption (..))
import qualified OpenSSL.Session as SSL
import Servant
import Servant.Client
import qualified Servant.Client as Servant
import System.Logger (Logger)
import qualified System.Logger as Log
import UnliftIO.Async

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

data RabbitMqAdminOpts = RabbitMqAdminOpts
  { host :: !String,
    port :: !Int,
    vHost :: !Text,
    adminPort :: !Int
  }
  deriving (Show, Generic)

instance FromJSON RabbitMqAdminOpts

mkRabbitMqAdminClientEnv :: RabbitMqAdminOpts -> IO (AdminAPI (AsClientT IO))
mkRabbitMqAdminClientEnv opts = do
  (username, password) <- readCredsFromEnv
  manager <- HTTP.newManager HTTP.defaultManagerSettings
  let basicAuthData = Servant.BasicAuthData (Text.encodeUtf8 username) (Text.encodeUtf8 password)
      clientEnv = Servant.mkClientEnv manager (Servant.BaseUrl Servant.Http opts.host opts.adminPort "")
  pure . fromServant $
    hoistClient
      (Proxy @(ToServant AdminAPI AsApi))
      (either throwM pure <=< flip runClientM clientEnv)
      (toServant $ adminClient basicAuthData)

-- | When admin opts are needed use `RabbitMqOpts Identity`, otherwise use
-- `RabbitMqOpts NoAdmin`.
data RabbitMqOpts = RabbitMqOpts
  { host :: !String,
    port :: !Int,
    vHost :: !Text
  }
  deriving (Show, Generic)

instance FromJSON RabbitMqOpts

demoteOpts :: RabbitMqAdminOpts -> RabbitMqOpts
demoteOpts RabbitMqAdminOpts {..} = RabbitMqOpts {..}

-- | Useful if the application only pushes into some queues.
mkRabbitMqChannelMVar :: Logger -> RabbitMqOpts -> IO (MVar Q.Channel)
mkRabbitMqChannelMVar l opts = do
  chanMVar <- newEmptyMVar
  connThread <-
    async . openConnectionWithRetries l opts $
      RabbitMqHooks
        { onNewChannel = \conn -> putMVar chanMVar conn >> forever (threadDelay maxBound),
          onChannelException = \_ -> void $ tryTakeMVar chanMVar,
          onConnectionClose = void $ tryTakeMVar chanMVar
        }
  waitForConnThread <- async $ withMVar chanMVar $ \_ -> pure ()
  waitEither connThread waitForConnThread >>= \case
    Left () -> throwIO $ RabbitMqConnectionFailed "connection thread finished before getting connection"
    Right () -> pure chanMVar

data RabbitMqConnectionError = RabbitMqConnectionFailed String
  deriving (Show)

instance Exception RabbitMqConnectionError

-- | Connects with RabbitMQ and opens a channel. If the channel is closed for
-- some reasons, reopens the channel. If the connection is closed for some
-- reasons, keeps retrying to connect until it works.
openConnectionWithRetries ::
  forall m.
  (MonadIO m, MonadMask m, MonadBaseControl IO m) =>
  Logger ->
  RabbitMqOpts ->
  RabbitMqHooks m ->
  m ()
openConnectionWithRetries l RabbitMqOpts {..} hooks = do
  (username, password) <- liftIO $ readCredsFromEnv
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
          getConn =
            recovering
              policy
              ( skipAsyncExceptions
                  <> [logRetries (const $ pure True) logError]
              )
              ( const $ do
                  Log.info l $ Log.msg (Log.val "Trying to connect to RabbitMQ")
                  liftIO $ Q.openConnection' host (fromIntegral port) vHost username password
              )
      bracket getConn (liftIO . Q.closeConnection) $ \conn -> do
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

readCredsFromEnv :: IO (Text, Text)
readCredsFromEnv =
  (,)
    <$> (Text.pack <$> getEnv "RABBITMQ_USERNAME")
    <*> (Text.pack <$> getEnv "RABBITMQ_PASSWORD")
