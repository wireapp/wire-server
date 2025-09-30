{-# LANGUAGE RecordWildCards #-}

-- | NATS Extended - Compatibility layer replacing Network.AMQP.Extended
-- 
-- This module provides a similar interface to Network.AMQP.Extended but uses NATS
-- instead of RabbitMQ for messaging.
module Network.NATS.Extended
  ( NatsHooks (..),
    NatsAdminOpts (..),
    NatsEndpoint (..),
    openConnectionWithRetries,
    mkNatsChannelMVar,
    defaultNatsOpts,
    demoteNatsOpts,
    readCredsFromEnv,
  )
where

import Control.Exception (AsyncException, throwIO, throw)
import Control.Monad.Catch
import Control.Monad.Trans.Control
import Control.Monad.Trans.Maybe
import Control.Retry
import Data.Aeson
import Data.Aeson.Types
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Imports
import Network.NATS.Client qualified as NATS
import System.Logger (Logger)
import System.Logger qualified as Log
import UnliftIO.Async

-- | Hooks for NATS connection lifecycle management
data NatsHooks m = NatsHooks
  { -- | Called whenever there is a new channel
    onNewChannel :: NATS.NatsChannel -> m (),
    -- | Called when connection is closed
    onConnectionClose :: m (),
    -- | Called when an error occurs
    onChannelException :: SomeException -> m ()
  }

-- | NATS endpoint configuration
data NatsEndpoint = NatsEndpoint
  { host :: !String,
    port :: !Int,
    -- | In NATS, this could map to a subject prefix
    namespace :: !Text
  }
  deriving (Eq, Show)

instance FromJSON NatsEndpoint where
  parseJSON = withObject "NatsEndpoint" $ \v ->
    NatsEndpoint
      <$> v .: "host"
      <*> v .: "port"
      <*> v .:? "namespace" .!= ""

-- | NATS admin options (for management API)
data NatsAdminOpts = NatsAdminOpts
  { host :: !String,
    port :: !Int,
    namespace :: !Text,
    adminHost :: !String,
    adminPort :: !Int
  }
  deriving (Eq, Show)

instance FromJSON NatsAdminOpts where
  parseJSON = withObject "NatsAdminOpts" $ \v ->
    NatsAdminOpts
      <$> v .: "host"
      <*> v .: "port"
      <*> v .:? "namespace" .!= ""
      <*> v .: "adminHost"
      <*> v .: "adminPort"

-- | Default NATS connection options
defaultNatsOpts :: NatsEndpoint
defaultNatsOpts =
  NatsEndpoint
    { host = "127.0.0.1",
      port = 4222,
      namespace = ""
    }

-- | Create a NATS channel MVar (similar to mkRabbitMqChannelMVar)
mkNatsChannelMVar :: Logger -> Maybe Text -> NatsEndpoint -> IO (MVar NATS.NatsChannel)
mkNatsChannelMVar l connName opts = do
  chanMVar <- newEmptyMVar
  connThread <-
    async . openConnectionWithRetries l opts connName $
      NatsHooks
        { onNewChannel = \conn -> putMVar chanMVar conn >> forever (threadDelay maxBound),
          onChannelException = \_ -> void $ tryTakeMVar chanMVar,
          onConnectionClose = void $ tryTakeMVar chanMVar
        }
  waitForConnThread <- async $ withMVar chanMVar $ \_ -> pure ()
  waitEither connThread waitForConnThread >>= \case
    Left () -> throwIO $ NatsConnectionFailed "connection thread finished before getting connection"
    Right () -> pure chanMVar

data NatsConnectionError = NatsConnectionFailed String
  deriving (Show)

instance Exception NatsConnectionError

-- | Open a NATS connection with automatic retries
openConnectionWithRetries ::
  forall m.
  (MonadIO m, MonadMask m, MonadBaseControl IO m) =>
  Logger ->
  NatsEndpoint ->
  Maybe Text ->
  NatsHooks m ->
  m ()
openConnectionWithRetries l NatsEndpoint {..} connName hooks = do
  (username, password) <- liftIO $ readCredsFromEnv
  connectWithRetries username password
  where
    connectWithRetries :: Text -> Text -> m ()
    connectWithRetries username password = do
      -- Jittered exponential backoff with 1ms as starting delay and 5s as max delay
      let policy = capDelay 5_000_000 $ fullJitterBackoff 1000
          logError willRetry e retryStatus = do
            Log.err l $
              Log.msg (Log.val "Failed to connect to NATS")
                . Log.field "error" (displayException @SomeException e)
                . Log.field "willRetry" willRetry
                . Log.field "retryCount" retryStatus.rsIterNumber
          getConn = do
            Log.info l $ Log.msg (Log.val "Trying to connect to NATS")
            conn <-
              recovering
                policy
                ( logAndSkipAsyncExceptions l
                    <> [logRetries (const $ pure True) logError]
                )
                ( const $ do
                    Log.info l $ Log.msg (Log.val "Opening NATS connection")
                    let connOpts =
                          NATS.defaultConnectionOpts
                            { NATS.natsServers = [(host, port)],
                              NATS.natsAuth = Just (username, password),
                              NATS.natsName = connName
                            }
                    liftIO $ NATS.openConnection connOpts
                )
            Log.info l $ Log.msg (Log.val "NATS connection established")
            pure conn
      bracket getConn (liftIO . NATS.closeConnection) $ \conn -> do
        chan <- liftIO $ NATS.createChannel conn
        -- Note: NATS doesn't have the same connection closed handler mechanism
        -- This is a simplified version
        hooks.onNewChannel chan
          `catch` \(e :: SomeException) -> do
            logException l "onNewChannel hook threw an exception" e
            hooks.onChannelException e
            throwM e

-- | List of pre-made handlers that will skip retries on AsyncException
logAndSkipAsyncExceptions :: (MonadIO m) => Logger -> [RetryStatus -> Control.Monad.Catch.Handler m Bool]
logAndSkipAsyncExceptions l = handlers
  where
    asyncH _ = Handler $ \(e :: AsyncException) -> do
      logException l "AsyncException caught" (SomeException e)
      pure False
    someAsyncH _ = Handler $ \(e :: SomeAsyncException) -> do
      logException l "SomeAsyncException caught" (SomeException e)
      pure False
    handlers = [asyncH, someAsyncH]

logException :: (MonadIO m) => Logger -> String -> SomeException -> m ()
logException l m (SomeException e) = do
  Log.err l $
    Log.msg m
      . Log.field "error" (displayException e)

-- | Read NATS credentials from environment
-- Note: Using same env vars as RabbitMQ for compatibility during migration
readCredsFromEnv :: IO (Text, Text)
readCredsFromEnv =
  (,)
    <$> (Text.pack <$> lookupEnv "NATS_USERNAME" >>= maybe (pure "guest") pure)
    <*> (Text.pack <$> lookupEnv "NATS_PASSWORD" >>= maybe (pure "guest") pure)
  where
    lookupEnv name = try @IOException (getEnv name) >>= \case
      Left _ -> pure Nothing
      Right v -> pure (Just v)

-- | Demote NatsAdminOpts to NatsEndpoint (for compatibility with RabbitMQ demoteOpts)
demoteNatsOpts :: NatsAdminOpts -> NatsEndpoint
demoteNatsOpts NatsAdminOpts {..} = NatsEndpoint {..}
