{-# LANGUAGE RecordWildCards #-}

module Cannon.RabbitMqConsumerApp where

import Cannon.App (rejectOnError)
import Cannon.Dict qualified as D
import Cannon.Options
import Cannon.RabbitMq
import Cannon.WS hiding (env)
import Cassandra as C hiding (batch)
import Control.Concurrent.Async
import Control.Concurrent.Timeout
import Control.Exception (Handler (..), bracket, catch, catches, throwIO, try)
import Control.Lens hiding ((#))
import Control.Monad.Codensity
import Data.Aeson hiding (Key)
import Data.Id
import Data.List.Extra hiding (delete)
import Data.Timeout (TimeoutUnit (..), (#))
import Imports hiding (min, threadDelay)
import Network.AMQP qualified as Q
import Network.WebSockets
import Network.WebSockets qualified as WS
import System.Logger qualified as Log
import UnliftIO.Async (pooledMapConcurrentlyN_)
import Wire.API.Event.WebSocketProtocol
import Wire.API.Notification

drainRabbitQueues :: Env -> IO ()
drainRabbitQueues e = do
  conns <- D.toList e.rabbitConnections
  numberOfConns <- fromIntegral <$> D.size e.rabbitConnections

  let opts = e.drainOpts
      maxNumberOfBatches = (opts ^. gracePeriodSeconds * 1000) `div` (opts ^. millisecondsBetweenBatches)
      computedBatchSize = numberOfConns `div` maxNumberOfBatches
      batchSize = max (opts ^. minBatchSize) computedBatchSize

  logDraining e.logg numberOfConns batchSize (opts ^. minBatchSize) computedBatchSize maxNumberOfBatches

  -- Sleeps for the grace period + 1 second. If the sleep completes, it means
  -- that draining didn't finish, and we should log that.
  timeoutAction <- async $ do
    -- Allocate 1 second more than the grace period to allow for overhead of
    -- spawning threads.
    liftIO $ threadDelay $ ((opts ^. gracePeriodSeconds) # Second + 1 # Second)
    logExpired e.logg (opts ^. gracePeriodSeconds)

  for_ (chunksOf (fromIntegral batchSize) conns) $ \batch -> do
    -- 16 was chosen with a roll of a fair dice.
    void . async $ pooledMapConcurrentlyN_ 16 (uncurry (closeConn e.logg)) batch
    liftIO $ threadDelay ((opts ^. millisecondsBetweenBatches) # MilliSecond)
  cancel timeoutAction
  Log.info e.logg $ Log.msg (Log.val "Draining complete")
  where
    closeConn :: Log.Logger -> Key -> Q.Connection -> IO ()
    closeConn l key conn = do
      Log.info l $
        Log.msg (Log.val "closing rabbitmq connection")
          . Log.field "key" (show key)
      Q.closeConnection conn
      void $ D.remove key e.rabbitConnections

    logExpired :: Log.Logger -> Word64 -> IO ()
    logExpired l period = do
      Log.err l $ Log.msg (Log.val "Drain grace period expired") . Log.field "gracePeriodSeconds" period

    logDraining :: Log.Logger -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> IO ()
    logDraining l count b min batchSize m = do
      Log.info l $
        Log.msg (Log.val "draining all rabbitmq connections")
          . Log.field "numberOfConns" count
          . Log.field "computedBatchSize" b
          . Log.field "minBatchSize" min
          . Log.field "batchSize" batchSize
          . Log.field "maxNumberOfBatches" m

rabbitMQWebSocketApp :: UserId -> ClientId -> Env -> ServerApp
rabbitMQWebSocketApp uid cid e pendingConn = do
  wsVar <- newEmptyMVar

  bracket (openWebSocket wsVar) closeWebSocket $ \(wsConn, _) ->
    ( do
        sendFullSyncMessageIfNeeded wsVar wsConn uid cid e
        sendNotifications wsConn wsVar
    )
      `catches` [ handleClientMisbehaving wsConn,
                  handleWebSocketExceptions wsConn
                ]
  where
    logClient =
      Log.field "user" (idToText uid)
        . Log.field "client" (clientToText cid)

    openWebSocket wsVar = do
      wsConn <-
        acceptRequest pendingConn
          `catch` rejectOnError pendingConn
      -- start a reader thread for client messages
      -- this needs to run asynchronously in order to promptly react to
      -- client-side connection termination
      a <- async $ forever $ do
        catch
          ( do
              msg <- getClientMessage wsConn
              putMVar wsVar (Right msg)
          )
          $ \err -> putMVar wsVar (Left err)
      pure (wsConn, a)

    -- this is only needed in case of asynchronous exceptions
    closeWebSocket (wsConn, a) = do
      cancel a
      logCloseWebsocket
      -- ignore any exceptions when sending the close message
      void . try @SomeException $ WS.sendClose wsConn ("" :: ByteString)

    getEventData :: RabbitMqChannel -> IO EventData
    getEventData chan = do
      (msg, envelope) <- getMessage chan
      case eitherDecode @QueuedNotification msg.msgBody of
        Left err -> do
          logParseError err
          -- This message cannot be parsed, make sure it doesn't requeue. There
          -- is no need to throw an error and kill the websocket as this is
          -- probably caused by a bug or someone messing with RabbitMQ.
          --
          -- The bug case is slightly dangerous as it could drop a lot of events
          -- en masse, if at some point we decide that Events should not be
          -- pushed as JSONs, hopefully we think of the parsing side if/when
          -- that happens.
          Q.rejectEnv envelope False
          -- try again
          getEventData chan
        Right notif -> do
          logEvent notif
          pure $ EventData notif envelope.envDeliveryTag

    handleWebSocketExceptions wsConn =
      Handler $
        \(err :: WS.ConnectionException) -> do
          case err of
            CloseRequest code reason ->
              Log.debug e.logg $
                Log.msg (Log.val "Client requested to close connection")
                  . Log.field "status_code" code
                  . Log.field "reason" reason
                  . logClient
            ConnectionClosed ->
              Log.info e.logg $
                Log.msg (Log.val "Client closed tcp connection abruptly")
                  . logClient
            _ -> do
              Log.info e.logg $
                Log.msg (Log.val "Failed to receive message, closing websocket")
                  . Log.field "error" (displayException err)
                  . logClient
              WS.sendCloseCode wsConn 1003 ("websocket-failure" :: ByteString)

    handleClientMisbehaving wsConn =
      Handler $ \(err :: WebSocketServerError) -> do
        case err of
          FailedToParseClientMessage _ -> do
            Log.info e.logg $
              Log.msg (Log.val "Failed to parse received message, closing websocket")
                . logClient
            WS.sendCloseCode wsConn 1003 ("failed-to-parse" :: ByteString)
          UnexpectedAck -> do
            Log.info e.logg $
              Log.msg (Log.val "Client sent unexpected ack message")
                . logClient
            WS.sendCloseCode wsConn 1003 ("unexpected-ack" :: ByteString)

    sendNotifications ::
      WS.Connection ->
      MVar (Either ConnectionException MessageClientToServer) ->
      IO ()
    sendNotifications wsConn wsVar = lowerCodensity $ do
      chan <- createChannel e.pool (clientNotificationQueueName uid cid)

      let consumeRabbitMq = forever $ do
            eventData <- getEventData chan
            catch (WS.sendBinaryData wsConn (encode (EventMessage eventData))) $
              \(err :: SomeException) -> do
                logSendFailure err
                throwIO err

      -- get ack from wsVar and forward to rabbitmq
      let consumeWebsocket = forever $ do
            v <- takeMVar wsVar
            either throwIO pure v >>= \case
              AckFullSync -> throwIO UnexpectedAck
              AckMessage ackData -> do
                logAckReceived ackData
                void $ ackMessage chan ackData.deliveryTag ackData.multiple

      -- run both loops concurrently, so that
      --  - notifications are delivered without having to wait for acks
      --  - exceptions on either side do not cause a deadlock
      lift $ concurrently_ consumeRabbitMq consumeWebsocket

    logParseError :: String -> IO ()
    logParseError err =
      Log.err e.logg $
        Log.msg (Log.val "failed to decode event from the queue as a JSON")
          . logClient
          . Log.field "parse_error" err

    logEvent :: QueuedNotification -> IO ()
    logEvent event =
      Log.debug e.logg $
        Log.msg (Log.val "got event")
          . logClient
          . Log.field "event" (encode event)

    logSendFailure :: SomeException -> IO ()
    logSendFailure err =
      Log.err e.logg $
        Log.msg (Log.val "Pushing to WS failed, closing connection")
          . Log.field "error" (displayException err)
          . logClient

    logAckReceived :: AckData -> IO ()
    logAckReceived ackData =
      Log.debug e.logg $
        Log.msg (Log.val "Received ACK")
          . Log.field "delivery_tag" ackData.deliveryTag
          . Log.field "multiple" ackData.multiple
          . logClient

    logCloseWebsocket :: IO ()
    logCloseWebsocket =
      Log.debug e.logg $
        Log.msg (Log.val "Closing the websocket")
          . logClient

-- | Check if client has missed messages. If so, send a full synchronisation
-- message and wait for the corresponding ack.
sendFullSyncMessageIfNeeded ::
  MVar (Either ConnectionException MessageClientToServer) ->
  WS.Connection ->
  UserId ->
  ClientId ->
  Env ->
  IO ()
sendFullSyncMessageIfNeeded wsVar wsConn uid cid env = do
  row <- C.runClient env.cassandra do
    retry x5 $ query1 q (params LocalQuorum (uid, cid))
  for_ row $ \_ -> sendFullSyncMessage uid cid wsVar wsConn env
  where
    q :: PrepQuery R (UserId, ClientId) (Identity (Maybe UserId))
    q =
      [sql| SELECT user_id FROM missed_notifications 
            WHERE user_id = ? and client_id = ?
        |]

sendFullSyncMessage ::
  UserId ->
  ClientId ->
  MVar (Either ConnectionException MessageClientToServer) ->
  WS.Connection ->
  Env ->
  IO ()
sendFullSyncMessage uid cid wsVar wsConn env = do
  let event = encode EventFullSync
  WS.sendBinaryData wsConn event
  res <- takeMVar wsVar >>= either throwIO pure
  case res of
    AckMessage _ -> throwIO UnexpectedAck
    AckFullSync ->
      C.runClient env.cassandra do
        retry x1 $ write delete (params LocalQuorum (uid, cid))
  where
    delete :: PrepQuery W (UserId, ClientId) ()
    delete =
      [sql|
          DELETE FROM missed_notifications
          WHERE user_id = ? and client_id = ?
        |]

getClientMessage :: WS.Connection -> IO MessageClientToServer
getClientMessage wsConn = do
  msg <- WS.receiveData wsConn
  case eitherDecode msg of
    Left err -> throwIO (FailedToParseClientMessage err)
    Right m -> pure m

data WebSocketServerError
  = FailedToParseClientMessage String
  | UnexpectedAck
  deriving (Show)

instance Exception WebSocketServerError
