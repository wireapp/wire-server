{-# LANGUAGE RecordWildCards #-}

module Network.AMQP.Extended where

import Control.Monad.Catch
import Control.Monad.Trans.Control
import Control.Retry
import Data.Aeson
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Imports
import qualified Network.AMQP as Q
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.OpenSSL as HTTP
import Network.RabbitMqAdmin
import OpenSSL.Session (SSLOption (..))
import qualified OpenSSL.Session as SSL
import qualified Servant
import Servant.Client
import qualified Servant.Client as Servant
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

data RabbitMqAdminOpts = RabbitMqAdminOpts
  { host :: !String,
    port :: !Int,
    vHost :: !Text,
    adminPort :: !Int,
    adminEnableTLS :: !Bool,
    -- | When CA is not specified and TLS is enabled, system CA will be used.
    adminCA :: !(Maybe Text)
  }
  deriving (Show, Generic)

instance FromJSON RabbitMqAdminOpts

mkRabbitMqAdminClientEnv :: RabbitMqAdminOpts -> IO (Servant.ClientEnv, AdminAPI (AsClientT ClientM))
mkRabbitMqAdminClientEnv opts = do
  (username, password) <- readCredsFromEnv
  managerSettings <-
    if opts.adminEnableTLS
      then tlsManagerSettings
      else pure HTTP.defaultManagerSettings
  manager <- HTTP.newManager managerSettings
  let clientEnv = Servant.mkClientEnv manager (Servant.BaseUrl scheme opts.host opts.adminPort "")
      basicAuthData = Servant.BasicAuthData (Text.encodeUtf8 username) (Text.encodeUtf8 password)
      scheme = if opts.adminEnableTLS then Servant.Https else Servant.Http
  pure (clientEnv, adminClient basicAuthData)
  where
    tlsManagerSettings = do
      ctx <- SSL.context
      SSL.contextAddOption ctx SSL_OP_NO_SSLv2
      SSL.contextAddOption ctx SSL_OP_NO_SSLv3
      SSL.contextAddOption ctx SSL_OP_NO_TLSv1
      SSL.contextSetCiphers ctx "HIGH"
      SSL.contextSetVerificationMode ctx $
        SSL.VerifyPeer True True Nothing
      SSL.contextSetDefaultVerifyPaths ctx
      pure $ HTTP.opensslManagerSettings (pure ctx)

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
  chan <- newEmptyMVar
  openConnectionWithRetries l opts $
    RabbitMqHooks
      { onNewChannel = putMVar chan,
        onChannelException = \_ -> void $ tryTakeMVar chan,
        onConnectionClose = void $ tryTakeMVar chan
      }
  pure chan

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
      recovering
        policy
        ( skipAsyncExceptions
            <> [ logRetries (const $ pure True) logError
               ]
        )
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

readCredsFromEnv :: IO (Text, Text)
readCredsFromEnv = do
  username <- Text.pack <$> getEnv "RABBITMQ_USERNAME"
  password <- Text.pack <$> getEnv "RABBITMQ_PASSWORD"
  pure (username, password)
