{-# LANGUAGE RecordWildCards #-}

module Cannon.RabbitMq
  ( RabbitMqPoolException,
    RabbitMqPoolOptions (..),
    RabbitMqPool,
    createRabbitMqPool,
    drainRabbitMqPool,
    RabbitMqChannel (..),
    createChannel,
    getMessage,
    ackMessage,
  )
where

import Cannon.Options
import Control.Concurrent.Async
import Control.Concurrent.Timeout
import Control.Exception
import Control.Lens ((^.))
import Control.Monad.Codensity
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Retry
import Data.ByteString.Conversion
import Data.List.Extra
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Timeout
import Imports hiding (threadDelay)
import Network.AMQP qualified as Q
import Network.AMQP.Extended
import System.Logger (Logger)
import System.Logger qualified as Log
import UnliftIO (pooledMapConcurrentlyN_)

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
    -- | draining mode
    draining :: TVar Bool,
    logger :: Logger,
    deadVar :: MVar ()
  }

data RabbitMqPoolOptions = RabbitMqPoolOptions
  { maxConnections :: Int,
    maxChannels :: Int,
    endpoint :: AmqpEndpoint,
    retryEnabled :: Bool
  }

createRabbitMqPool :: (Ord key) => RabbitMqPoolOptions -> Logger -> Codensity IO (RabbitMqPool key)
createRabbitMqPool opts logger = Codensity $ bracket create destroy
  where
    create = do
      deadVar <- newEmptyMVar
      (nextId, connections, draining) <-
        atomically $
          (,,) <$> newTVar 0 <*> newTVar [] <*> newTVar False
      let pool = RabbitMqPool {..}
      -- create one connection
      void $ createConnection pool
      pure pool
    destroy pool = putMVar pool.deadVar ()

drainRabbitMqPool :: (ToByteString key) => RabbitMqPool key -> DrainOpts -> IO ()
drainRabbitMqPool pool opts = do
  atomically $ writeTVar pool.draining True

  channels <- atomically $ do
    conns <- readTVar pool.connections
    pure $ concat [Map.assocs c.channels | c <- conns]
  let numberOfChannels = fromIntegral (length channels)

  let maxNumberOfBatches =
        (opts ^. gracePeriodSeconds * 1000)
          `div` (opts ^. millisecondsBetweenBatches)
      computedBatchSize = numberOfChannels `div` maxNumberOfBatches
      batchSize = max (opts ^. minBatchSize) computedBatchSize

  logDraining
    pool.logger
    numberOfChannels
    batchSize
    (opts ^. minBatchSize)
    computedBatchSize
    maxNumberOfBatches

  -- Sleep for the grace period + 1 second. If the sleep completes, it means
  -- that draining didn't finish, and we should log that.
  withAsync
    ( do
        -- Allocate 1 second more than the grace period to allow for overhead of
        -- spawning threads.
        liftIO $ threadDelay $ ((opts ^. gracePeriodSeconds) # Second + 1 # Second)
        logExpired pool.logger (opts ^. gracePeriodSeconds)
    )
    $ \_ -> do
      for_ (chunksOf (fromIntegral batchSize) channels) $ \batch -> do
        -- 16 was chosen with a roll of a fair dice.
        concurrently
          (pooledMapConcurrentlyN_ 16 (closeChannel pool.logger) batch)
          (liftIO $ threadDelay ((opts ^. millisecondsBetweenBatches) # MilliSecond))
  Log.info pool.logger $ Log.msg (Log.val "Draining complete")
  where
    closeChannel :: (ToByteString key) => Log.Logger -> (key, Q.Channel) -> IO ()
    closeChannel l (key, chan) = do
      Log.info l $
        Log.msg (Log.val "closing rabbitmq channel")
          . Log.field "key" (toByteString' key)
      Q.closeChannel chan

    logExpired :: Log.Logger -> Word64 -> IO ()
    logExpired l period = do
      Log.err l $ Log.msg (Log.val "Drain grace period expired") . Log.field "gracePeriodSeconds" period

    logDraining :: Log.Logger -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> IO ()
    logDraining l count b minB batchSize m = do
      Log.info l $
        Log.msg (Log.val "draining all rabbitmq channels")
          . Log.field "numberOfChannels" count
          . Log.field "computedBatchSize" b
          . Log.field "minBatchSize" minB
          . Log.field "batchSize" batchSize
          . Log.field "maxNumberOfBatches" m

createConnection :: (Ord key) => RabbitMqPool key -> IO (PooledConnection key)
createConnection pool = mask_ $ do
  conn <- openConnection pool
  mpconn <- runMaybeT . atomically $ do
    -- do not create new connections when in draining mode
    readTVar pool.draining >>= guard . not
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
  pconn <- maybe (throwIO TooManyChannels) pure mpconn

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
  -- This might not be the correct connection ID that will eventually be
  -- assigned to this connection, since there are potential races with other
  -- connections being opened at the same time. However, this is only used to
  -- name the connection, and we only rely on names for tests, so it is fine.
  connId <- readTVarIO pool.nextId
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
                Q.coTLSSettings = fmap Q.TLSCustom mTlsSettings,
                -- the name is used by tests to identify pool connections
                Q.coName = Just ("pool " <> T.pack (show connId))
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
            pure pool.opts.retryEnabled
          _ -> do
            unless (fromException e == Just AsyncCancelled) $
              logException pool.logger "RabbitMQ channel closed" e
            pure pool.opts.retryEnabled
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
              retry <- takeMVar closedVar
              void $ takeMVar inner
              pure retry

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
        (createConnection pool)
        (Q.closeConnection . (.inner))
        $ \conn -> do
          -- if we have too many connections at this point, give up
          numConnections <- atomically $ length <$> readTVar pool.connections
          when (numConnections > pool.opts.maxConnections) $
            throw TooManyChannels
          pure conn
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
