{-# OPTIONS -Wwarn #-}
{-# LANGUAGE RecordWildCards #-}

module Cannon.RabbitMq
  ( RabbitMqPoolException,
    RabbitMqPoolOptions (..),
    RabbitMqPool,
    createRabbitMqPool,
    RabbitMqChannel,
    createChannel,
    getMessage,
  )
where

import Control.Concurrent.Async
import Control.Exception
import Control.Monad.Codensity
import Control.Monad.Trans.Except
import Control.Retry
import Data.List.Extra
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

data PooledConnection = PooledConnection
  { connId :: Word64,
    inner :: Q.Connection,
    numChannels :: !Int
  }

data RabbitMqPool = RabbitMqPool
  { opts :: RabbitMqPoolOptions,
    nextId :: TVar Word64,
    -- TODO: use a priority queue
    connections :: TVar [PooledConnection],
    logger :: Logger,
    deadVar :: MVar ()
  }

data RabbitMqPoolOptions = RabbitMqPoolOptions
  { maxConnections :: Int,
    maxChannels :: Int,
    endpoint :: AmqpEndpoint
  }

createRabbitMqPool :: RabbitMqPoolOptions -> Logger -> Codensity IO RabbitMqPool
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

createConnection :: RabbitMqPool -> IO Q.Connection
createConnection pool = mask_ $ do
  conn <- openConnection pool
  pconn <- atomically $ do
    connId <- readTVar pool.nextId
    writeTVar pool.nextId $! succ connId
    let c =
          PooledConnection
            { connId = connId,
              numChannels = 0,
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
  pure conn

openConnection :: RabbitMqPool -> IO Q.Connection
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
  { inner :: MVar Q.Channel,
    msgVar :: MVar (Maybe (Q.Message, Q.Envelope))
  }

getMessage :: RabbitMqChannel -> IO (Q.Message, Q.Envelope)
getMessage chan = takeMVar chan.msgVar >>= maybe (throwIO ChannelClosed) pure

createChannel :: RabbitMqPool -> Text -> IO RabbitMqChannel
createChannel pool queue = do
  closedVar <- newEmptyMVar
  inner <- newEmptyMVar
  msgVar <- newEmptyMVar
  let manageChannel = do
        conn <- acquireConnection pool
        chan <- Q.openChannel conn.inner
        void $ Q.consumeMsgs chan queue Q.Ack $ \(message, envelope) -> do
          putMVar msgVar (Just (message, envelope))

        Q.addChannelExceptionHandler chan $ \e -> do
          releaseConnection pool conn

          retry <- case (Q.isNormalChannelClose e, fromException e) of
            (True, _) -> do
              Log.info pool.logger $
                Log.msg (Log.val "RabbitMQ channel is closed normally, not attempting to reopen channel")
              pure False
            (_, Just (Q.ConnectionClosedException {})) -> do
              Log.info pool.logger $
                Log.msg (Log.val "RabbitMQ connection is closed, not attempting to reopen channel")
              pure False
            _ -> do
              logException pool.logger "RabbitMQ channel closed" e
              pure True

          putMVar closedVar retry

        retry <- takeMVar closedVar
        if retry
          then manageChannel
          else putMVar msgVar Nothing

  -- TODO: leaking async
  void . mask_ $ async manageChannel
  pure RabbitMqChannel {inner = inner, msgVar = msgVar}

acquireConnection :: RabbitMqPool -> IO PooledConnection
acquireConnection pool = (>>= either throwIO pure) . atomically . runExceptT $ do
  conns <- lift $ readTVar pool.connections
  let pconn = minimumOn (.numChannels) conns
  -- TODO: spawn new connection if possible
  when (pconn.numChannels >= pool.opts.maxChannels) $
    throwE TooManyChannels

  let pconn' =
        pconn'
          { numChannels =
              max pool.opts.maxChannels (succ (numChannels pconn))
          }
  lift $
    writeTVar pool.connections $!
      map (\c -> if c.connId == pconn'.connId then pconn' else c) conns
  pure pconn'

releaseConnection :: RabbitMqPool -> PooledConnection -> IO ()
releaseConnection pool conn = atomically $ do
  modifyTVar pool.connections $ map $ \c ->
    if c.connId == conn.connId
      then c {numChannels = pred (numChannels c)}
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
