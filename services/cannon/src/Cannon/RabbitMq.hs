{-# LANGUAGE RecordWildCards #-}

module Cannon.RabbitMq
  ( RabbitMqPoolException (..),
    RabbitMqChannelException (..),
    RabbitMqPoolOptions (..),
    RabbitMqPool,
    QueueInfo (..),
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
import Data.Id
import Data.List.Extra
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Timeout
import Data.Unique
import Imports hiding (threadDelay)
import Network.AMQP qualified as Q
import Network.AMQP.Extended
import System.Logger (Logger)
import System.Logger qualified as Log
import UnliftIO (pooledMapConcurrentlyN_)

data RabbitMqPoolException
  = TooManyChannels
  deriving (Eq, Show)

instance Exception RabbitMqPoolException

data RabbitMqChannelException = ChannelClosed
  deriving (Eq, Show)

instance Exception RabbitMqChannelException

data PooledConnection = PooledConnection
  { connId :: Word64,
    inner :: Q.Connection,
    channels :: !(Map Unique Q.Channel)
  }

data RabbitMqPool = RabbitMqPool
  { opts :: RabbitMqPoolOptions,
    nextId :: TVar Word64,
    connections :: TVar [PooledConnection],
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

createRabbitMqPool :: RabbitMqPoolOptions -> Logger -> Codensity IO RabbitMqPool
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

drainRabbitMqPool :: RabbitMqPool -> DrainOpts -> IO ()
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
    closeChannel :: Log.Logger -> (Unique, Q.Channel) -> IO ()
    closeChannel l (key, chan) = do
      Log.debug l $
        Log.msg (Log.val "closing rabbitmq channel")
          . Log.field "key_hash" (toByteString' $ hashUnique key)
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

createConnection :: RabbitMqPool -> IO PooledConnection
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

openConnection :: RabbitMqPool -> IO Q.Connection
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
  { inner :: Q.Channel,
    msgVar :: MVar (Maybe (Q.Message, Q.Envelope))
  }

getMessage :: RabbitMqChannel -> IO (Q.Message, Q.Envelope)
getMessage chan = takeMVar chan.msgVar >>= maybe (throwIO ChannelClosed) pure

ackMessage :: RabbitMqChannel -> Word64 -> Bool -> IO ()
ackMessage chan deliveryTag multiple = do
  Q.ackMsg chan.inner deliveryTag multiple

data QueueInfo = QueueInfo
  { queueName :: Text,
    messageCount :: Int
  }

type CreateQueue = Q.Channel -> Codensity IO QueueInfo

createChannel :: UserId -> Maybe ClientId -> RabbitMqPool -> CreateQueue -> Codensity IO (RabbitMqChannel, QueueInfo)
createChannel uid mcid pool createQueue = do
  msgVar <- lift newEmptyMVar
  key <- lift newUnique

  let logContext =
        Log.field "user" (idToText uid)
          . Log.field "client" (maybe "<temporary>" clientToText mcid)
      handleException e = do
        case (Q.isNormalChannelClose e, fromException e) of
          (True, _) -> do
            Log.info pool.logger $
              Log.msg (Log.val "RabbitMQ channel is closed normally, not attempting to reopen channel")
                . logContext
          (_, Just (Q.ConnectionClosedException {})) -> do
            Log.info pool.logger $
              Log.msg (Log.val "RabbitMQ connection was closed unexpectedly")
                . logContext
          _ -> do
            unless (fromException e == Just AsyncCancelled) $
              Log.err pool.logger $
                Log.msg (Log.val "RabbitMQ channel closed")
                  . (Log.field "error" (displayException e))
                  . logContext
        putMVar msgVar Nothing

  let tryCreateChannel = do
        conn <- Codensity $ bracket (acquireConnection pool) (releaseConnection pool key)
        chan <- Codensity $ bracket (Q.openChannel conn.inner) $ \c ->
          catch (Q.closeChannel c) $ \(_ :: SomeException) -> pure ()
        -- Limit the amount of unacknowledged message a consumer receives
        -- (`true` means limit per consumer, `false` per channel.) This
        -- prevents overloading the consumer with new messages.
        liftIO $ Q.qos chan 0 500 True

        connSize <- atomically $ do
          let conn' = conn {channels = Map.insert key chan conn.channels}
          conns <- readTVar pool.connections
          writeTVar pool.connections $!
            map (\c -> if c.connId == conn'.connId then conn' else c) conns
          pure $ Map.size conn'.channels
        if connSize > pool.opts.maxChannels
          then pure Nothing
          else do
            liftIO $ Q.addChannelExceptionHandler chan handleException
            pure . Just $ RabbitMqChannel {inner = chan, msgVar = msgVar}

  mChan <-
    retrying
      -- For this to reach 10 times, we'd have to create a _lot_ of channels
      -- concurrently and have this thread lose all races for creating a
      -- connection
      (constantDelay 10000 <> limitRetries 10)
      (const $ pure . isNothing)
      (const $ tryCreateChannel)
  chan <- maybe (throw TooManyChannels) pure mChan
  queueInfo <- createQueue chan.inner
  void $ liftIO $ Q.consumeMsgs (chan.inner) queueInfo.queueName Q.Ack $ \(message, envelope) -> do
    putMVar msgVar (Just (message, envelope))
  pure (chan, queueInfo)

acquireConnection :: RabbitMqPool -> IO PooledConnection
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

findConnection :: RabbitMqPool -> IO (Maybe PooledConnection)
findConnection pool = (either throwIO pure <=< (atomically . runExceptT . runMaybeT)) $ do
  conns <- lift . lift $ readTVar pool.connections
  guard (notNull conns)

  let pconn = minimumOn (Map.size . (.channels)) $ conns
  when (Map.size pconn.channels >= pool.opts.maxChannels) $
    if length conns >= pool.opts.maxConnections
      then lift $ throwE TooManyChannels
      else mzero
  pure pconn

releaseConnection :: RabbitMqPool -> Unique -> PooledConnection -> IO ()
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

rabbitMqRetryPolicy :: RetryPolicyM IO
rabbitMqRetryPolicy = limitRetriesByCumulativeDelay 1_000_000 $ fullJitterBackoff 1000
