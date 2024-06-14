{-# LANGUAGE RecordWildCards #-}

module Network.AMQP.Extended where

import Control.Exception (throwIO)
import Control.Monad.Catch
import Control.Monad.Trans.Control
import Control.Retry
import Data.Aeson
import Data.Default
import Data.Proxy
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.X509.CertificateStore qualified as X509
import Imports
import Network.AMQP qualified as Q
import Network.Connection as Conn
import Network.HTTP.Client qualified as HTTP
import Network.RabbitMqAdmin
import Network.TLS
import Network.TLS.Extra.Cipher
import Servant
import Servant.Client
import Servant.Client qualified as Servant
import System.Logger (Logger)
import System.Logger qualified as Log
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
    caCert :: !(Maybe FilePath),
    insecureSkipVerifyTls :: Bool,
    adminPort :: !Int
  }
  deriving (Show, Generic)

instance FromJSON RabbitMqAdminOpts where
  parseJSON = withObject "RabbitMqAdminOpts" $ \v ->
    RabbitMqAdminOpts
      <$> v .: "host"
      <*> v .: "port"
      <*> v .: "vHost"
      <*> v .:? "caCert"
      <*> v .:? "insecureSkipVerifyTls" .!= False
      <*> v .: "adminPort"

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
    vHost :: !Text,
    caCert :: !(Maybe FilePath),
    insecureSkipVerifyTls :: Bool
  }
  deriving (Show, Generic)

instance FromJSON RabbitMqOpts where
  parseJSON = withObject "RabbitMqAdminOpts" $ \v ->
    RabbitMqOpts
      <$> v .: "host"
      <*> v .: "port"
      <*> v .: "vHost"
      <*> v .:? "caCert"
      <*> v .:? "insecureSkipVerifyTls" .!= False

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
                  mTlsSettings <- traverse (liftIO . mkTLSSettings) caCert
                  liftIO $
                    Q.openConnection'' $
                      Q.defaultConnectionOpts
                        { Q.coServers = [(host, fromIntegral port)],
                          Q.coVHost = vHost,
                          Q.coAuth = [Q.plain username password],
                          Q.coTLSSettings = fmap Q.TLSCustom mTlsSettings
                        }
              )
      bracket getConn (liftIO . Q.closeConnection) $ \conn -> do
        liftBaseWith $ \runInIO ->
          Q.addConnectionClosedHandler conn True $ void $ runInIO $ do
            hooks.onConnectionClose
              `catch` logException l "onConnectionClose hook threw an exception, reconnecting to RabbitMQ anyway"
            connectWithRetries username password
        openChan conn

    mkTLSSettings :: FilePath -> IO TLSSettings
    mkTLSSettings path = do
      -- TODO: throw better exception here
      caStore <- fromJust <$> X509.readCertificateStore path
      pure $
        TLSSettings
          (defaultParamsClient host "rabbitmq")
            { clientShared =
                def
                  { sharedCAStore = caStore
                  },
              clientSupported =
                def
                  { supportedVersions = [TLS13, TLS12],
                    supportedCiphers = ciphersuite_strong
                  }
            }

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
      hooks.onChannelException e `catch` logException l "onChannelException hook threw an exception"
      case (Q.isNormalChannelClose e, fromException e) of
        (True, _) ->
          Log.info l $
            Log.msg (Log.val "RabbitMQ channel is closed normally, not attempting to reopen channel")
        (_, Just (Q.ConnectionClosedException {})) ->
          Log.info l $
            Log.msg (Log.val "RabbitMQ connection is closed, not attempting to reopen channel")
        _ -> do
          logException l "RabbitMQ channel closed" e
          openChan conn

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
