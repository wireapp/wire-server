{-# OPTIONS -Wwarn #-}
{-# LANGUAGE RecordWildCards #-}

module Cannon.RabbitMq
  ( RabbitMqPoolException,
    RabbitMqPoolOptions (..),
    RabbitMqPool,
    createRabbitMqPool,
    RabbitMqChannel (..),
    createChannel,
    getMessage,
    ackMessage,
  )
where

import Control.Concurrent.Async
import Control.Exception
import Control.Monad.Codensity
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Retry
import Data.List.Extra
import Data.Map qualified as Map
import Imports
import Network.AMQP qualified as Q
import Network.AMQP.Extended
import System.Logger (Logger)
import System.Logger qualified as Log

data RabbitMqPoolException
  = TooManyChannels
  | ChannelClosed
  deriving (Eq, Show)

instance Exception RabbitMqPoolException

data PooledConnection key = PooledConnection
  { connId :: Word64,
    inner :: Q.Connection,
    channels :: !(Map key Q.Channel)
  }

data RabbitMqPool key = RabbitMqPool
  { opts :: RabbitMqPoolOptions,
    nextId :: TVar Word64,
    connections :: TVar [PooledConnection key],
    logger :: Logger,
    deadVar :: MVar ()
  }

data RabbitMqPoolOptions = RabbitMqPoolOptions
  { maxConnections :: Int,
    maxChannels :: Int,
    endpoint :: AmqpEndpoint
  }

createRabbitMqPool :: (Ord key) => RabbitMqPoolOptions -> Logger -> Codensity IO (RabbitMqPool key)
createRabbitMqPool opts logger = Codensity $ bracket create destroy
  where
    create = do
      deadVar <- newEmptyMVar
      (nextId, connections) <-
        atomically $
          (,) <$> newTVar 0 <*> newTVar []
      let pool = RabbitMqPool {..}
      -- create one connection
      void $ createConnection pool
      pure pool
    destroy pool = putMVar pool.deadVar ()

createConnection :: (Ord key) => RabbitMqPool key -> IO (PooledConnection key)
createConnection pool = mask_ $ do
  conn <- openConnection pool
  pconn <- atomically $ do
    connId <- readTVar pool.nextId
    writeTVar pool.nextId $! succ connId
    let c =
          PooledConnection
            { connId = connId,
              channels = mempty,
              inner = conn
            }
    modifyTVar pool.connections (c :)
    pure c

  closedVar <- newEmptyMVar
  -- Fire and forget: the thread will terminate by itself as soon as the
  -- connection is closed (or if the pool is destroyed).
  -- Asynchronous exception safety is guaranteed because exceptions are masked
  -- in this whole block.
  void . async $ do
    v <- race (takeMVar closedVar) (readMVar pool.deadVar)
    when (isRight v) $
      -- close connection and ignore exceptions
      catch @SomeException (Q.closeConnection conn) $
        \_ -> pure ()
    atomically $ do
      conns <- readTVar pool.connections
      writeTVar pool.connections $
        filter (\c -> c.connId /= pconn.connId) conns
  Q.addConnectionClosedHandler conn True $ do
    putMVar closedVar ()
  pure pconn

openConnection :: RabbitMqPool key -> IO Q.Connection
openConnection pool = do
  (username, password) <- readCredsFromEnv
  recovering
    rabbitMqRetryPolicy
    ( skipAsyncExceptions
        <> [logRetries (const $ pure True) (logConnectionError pool.logger)]
    )
    ( const $ do
        Log.info pool.logger $
          Log.msg (Log.val "Trying to connect to RabbitMQ")
        mTlsSettings <-
          traverse
            (liftIO . (mkTLSSettings pool.opts.endpoint.host))
            pool.opts.endpoint.tls
        liftIO $
          Q.openConnection'' $
            Q.defaultConnectionOpts
              { Q.coServers =
                  [ ( pool.opts.endpoint.host,
                      fromIntegral pool.opts.endpoint.port
                    )
                  ],
                Q.coVHost = pool.opts.endpoint.vHost,
                Q.coAuth = [Q.plain username password],
                Q.coTLSSettings = fmap Q.TLSCustom mTlsSettings
              }
    )

data RabbitMqChannel = RabbitMqChannel
  { -- | The current channel. The var is empty while the channel is being
    -- re-established.
    inner :: MVar Q.Channel,
    msgVar :: MVar (Maybe (Q.Message, Q.Envelope))
  }

getMessage :: RabbitMqChannel -> IO (Q.Message, Q.Envelope)
getMessage chan = takeMVar chan.msgVar >>= maybe (throwIO ChannelClosed) pure

ackMessage :: RabbitMqChannel -> Word64 -> Bool -> IO ()
ackMessage chan deliveryTag multiple = do
  inner <- readMVar chan.inner
  Q.ackMsg inner deliveryTag multiple

createChannel :: (Ord key) => RabbitMqPool key -> Text -> key -> Codensity IO RabbitMqChannel
createChannel pool queue key = do
  closedVar <- lift newEmptyMVar
  inner <- lift newEmptyMVar
  msgVar <- lift newEmptyMVar

  let handleException e = do
        retry <- case (Q.isNormalChannelClose e, fromException e) of
          (True, _) -> do
            Log.info pool.logger $
              Log.msg (Log.val "RabbitMQ channel is closed normally, not attempting to reopen channel")
            pure False
          (_, Just (Q.ConnectionClosedException {})) -> do
            Log.info pool.logger $
              Log.msg (Log.val "RabbitMQ connection was closed unexpectedly")
            pure True
          _ -> do
            unless (fromException e == Just AsyncCancelled) $
              logException pool.logger "RabbitMQ channel closed" e
            pure True
        putMVar closedVar retry

  let manageChannel = do
        retry <- lowerCodensity $ do
          conn <- Codensity $ bracket (acquireConnection pool) (releaseConnection pool key)
          chan <- Codensity $ bracket (Q.openChannel conn.inner) $ \c ->
            catch (Q.closeChannel c) $ \(_ :: SomeException) -> pure ()
          connSize <- atomically $ do
            let conn' = conn {channels = Map.insert key chan conn.channels}
            conns <- readTVar pool.connections
            writeTVar pool.connections $!
              map (\c -> if c.connId == conn'.connId then conn' else c) conns
            pure $ Map.size conn'.channels
          if connSize > pool.opts.maxChannels
            then pure True
            else do
              liftIO $ Q.addChannelExceptionHandler chan handleException
              putMVar inner chan
              void $ liftIO $ Q.consumeMsgs chan queue Q.Ack $ \(message, envelope) -> do
                putMVar msgVar (Just (message, envelope))
              takeMVar closedVar

        when retry manageChannel

  void $
    Codensity $
      withAsync $
        catch manageChannel handleException
          `finally` putMVar msgVar Nothing
  pure RabbitMqChannel {inner = inner, msgVar = msgVar}

acquireConnection :: (Ord key) => RabbitMqPool key -> IO (PooledConnection key)
acquireConnection pool = do
  findConnection pool >>= \case
    Nothing -> do
      bracketOnError
        ( do
            conn <- createConnection pool
            -- if we have too many connections at this point, give up
            numConnections <- -- TODO should be moved to the body
              atomically $
                length <$> readTVar pool.connections
            when (numConnections > pool.opts.maxConnections) $
              throw TooManyChannels
            pure conn
        )
        (\conn -> Q.closeConnection conn.inner)
        pure
    Just conn -> pure conn

findConnection :: RabbitMqPool key -> IO (Maybe (PooledConnection key))
findConnection pool = (either throwIO pure <=< (atomically . runExceptT . runMaybeT)) $ do
  conns <- lift . lift $ readTVar pool.connections
  guard (notNull conns)

  let pconn = minimumOn (Map.size . (.channels)) $ conns
  when (Map.size pconn.channels >= pool.opts.maxChannels) $
    if length conns >= pool.opts.maxConnections
      then lift $ throwE TooManyChannels
      else mzero
  pure pconn

releaseConnection :: (Ord key) => RabbitMqPool key -> key -> PooledConnection key -> IO ()
releaseConnection pool key conn = atomically $ do
  modifyTVar pool.connections $ map $ \c ->
    if c.connId == conn.connId
      then c {channels = Map.delete key c.channels}
      else c

logConnectionError :: Logger -> Bool -> SomeException -> RetryStatus -> IO ()
logConnectionError l willRetry e retryStatus = do
  Log.err l $
    Log.msg (Log.val "Failed to connect to RabbitMQ")
      . Log.field "error" (displayException @SomeException e)
      . Log.field "willRetry" willRetry
      . Log.field "retryCount" retryStatus.rsIterNumber

logException :: (MonadIO m) => Logger -> String -> SomeException -> m ()
logException l m (SomeException e) = do
  Log.err l $
    Log.msg m
      . Log.field "error" (displayException e)

rabbitMqRetryPolicy :: RetryPolicyM IO
rabbitMqRetryPolicy = limitRetriesByCumulativeDelay 1_000_000 $ fullJitterBackoff 1000
