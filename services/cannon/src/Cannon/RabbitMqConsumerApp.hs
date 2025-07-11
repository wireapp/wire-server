{-# LANGUAGE RecordWildCards #-}

module Cannon.RabbitMqConsumerApp (rabbitMQWebSocketApp) where

import Cannon.App (rejectOnError)
import Cannon.Options
import Cannon.RabbitMq
import Cannon.WS hiding (env)
import Cassandra as C hiding (batch)
import Control.Concurrent.Async
import Control.Exception (Handler (..), bracket, catch, catches, handle, throwIO, try)
import Control.Lens hiding ((#))
import Control.Monad.Codensity
import Data.Aeson hiding (Key)
import Data.Id
import Data.Text
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Imports hiding (min, threadDelay)
import Network.AMQP (newQueue)
import Network.AMQP qualified as Q
import Network.WebSockets
import Network.WebSockets qualified as WS
import Network.WebSockets.Connection
import System.Logger qualified as Log
import System.Timeout
import Wire.API.Event.WebSocketProtocol
import Wire.API.Notification

data InactivityTimeout = InactivityTimeout
  deriving (Show)

instance Exception InactivityTimeout

rabbitMQWebSocketApp :: UserId -> Maybe ClientId -> Maybe Text -> Env -> ServerApp
rabbitMQWebSocketApp uid mcid mSyncMarkerId e pendingConn =
  handle handleTooManyChannels . lowerCodensity $
    do
      (chan, queueInfo) <- createChannel uid mcid e.pool createQueue
      conn <- Codensity $ bracket openWebSocket closeWebSocket
      activity <- liftIO newEmptyMVar
      let wsConn =
            WSConnection
              { inner = conn,
                activity,
                activityTimeout = e.wsOpts.activityTimeout,
                pongTimeout = e.wsOpts.pongTimeout
              }

      main <- Codensity
        $ withAsync
        $ flip
          catches
          [ handleClientMisbehaving conn,
            handleWebSocketExceptions conn,
            handleRabbitMqChannelException conn,
            handleInactivity conn,
            handleOtherExceptions conn
          ]
        $ do
          traverse_ (sendFullSyncMessageIfNeeded wsConn uid e) mcid
          traverse_ (Q.publishMsg chan.inner "" queueInfo.queueName . mkSynchronizationMessage e.notificationTTL) (mcid *> mSyncMarkerId)
          sendNotifications chan wsConn

      let monitor = do
            timeout wsConn.activityTimeout (takeMVar wsConn.activity) >>= \case
              Just _ -> monitor
              Nothing -> do
                WS.sendPing wsConn.inner ("ping" :: Text)
                timeout wsConn.pongTimeout (takeMVar wsConn.inner.connectionHeartbeat) >>= \case
                  Just _ -> monitor
                  Nothing -> cancelWith main InactivityTimeout

      _ <- Codensity $ withAsync monitor

      liftIO $ wait main
  where
    logClient =
      Log.field "user" (idToText uid)
        . Log.field "client" (maybe "<temporary>" clientToText mcid)

    openWebSocket =
      acceptRequest pendingConn
        `catch` rejectOnError pendingConn

    closeWebSocket wsConn = do
      logCloseWebsocket
      -- ignore any exceptions when sending the close message
      void . try @SomeException $ WS.sendClose wsConn ("" :: ByteString)

    getEventData :: RabbitMqChannel -> IO (Either EventData SynchronizationData)
    getEventData chan = do
      (msg, envelope) <- getMessage chan
      case msg.msgType of
        Just "synchronization" -> do
          let syncData =
                SynchronizationData
                  { markerId = TL.toStrict $ TLE.decodeUtf8 msg.msgBody,
                    deliveryTag = envelope.envDeliveryTag
                  }
          pure $ Right syncData
        _ -> do
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
              pure $
                Left $
                  EventData
                    { event = notif,
                      deliveryTag = envelope.envDeliveryTag
                    }

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

    handleInactivity wsConn =
      Handler $ \(_ :: InactivityTimeout) -> do
        Log.info e.logg $
          Log.msg (Log.val "Closing websocket due to inactivity")
            . logClient
        WS.sendCloseCode wsConn 1002 ("inactivity" :: ByteString)

    handleClientMisbehaving wsConn =
      Handler $ \(err :: WebSocketServerError) -> do
        case err of
          FailedToParseClientMessage parseError -> do
            Log.info e.logg $
              Log.msg (Log.val "Failed to parse received message, closing websocket")
                . Log.field "parse_error" parseError
                . logClient
            WS.sendCloseCode wsConn 1003 ("failed-to-parse" :: ByteString)
          UnexpectedAck -> do
            Log.info e.logg $
              Log.msg (Log.val "Client sent unexpected ack message")
                . logClient
            WS.sendCloseCode wsConn 1003 ("unexpected-ack" :: ByteString)

    handleRabbitMqChannelException wsConn = do
      Handler $ \ChannelClosed -> do
        Log.debug e.logg $ Log.msg (Log.val "RabbitMQ channel closed") . logClient
        WS.sendCloseCode wsConn 1001 ("" :: ByteString)

    handleOtherExceptions wsConn = Handler $
      \(err :: SomeException) -> do
        WS.sendCloseCode wsConn 1003 ("internal-error" :: ByteString)
        throwIO err

    handleTooManyChannels TooManyChannels =
      rejectRequestWith pendingConn $
        RejectRequest
          { rejectCode = 503,
            rejectMessage = "Service Unavailable",
            rejectHeaders = [],
            rejectBody = ""
          }

    createQueue chan = case mcid of
      Nothing -> Codensity $ \k -> do
        (queueName, messageCount, _) <-
          Q.declareQueue chan $
            newQueue
              { Q.queueExclusive = True,
                Q.queueAutoDelete = True
              }
        for_ [userRoutingKey uid, temporaryRoutingKey uid] $
          Q.bindQueue chan queueName userNotificationExchangeName
        k $ QueueInfo {queueName = queueName, messageCount = messageCount}
      Just cid -> Codensity $ \k -> do
        (queueName, messageCount, _) <- Q.declareQueue chan $ queueOpts (clientNotificationQueueName uid cid)
        k $ QueueInfo queueName messageCount

    mkSynchronizationMessage ttl markerId =
      Q.newMsg
        { Q.msgBody = TLE.encodeUtf8 (TL.fromStrict markerId),
          Q.msgContentType = Just "text/plain; charset=utf-8",
          Q.msgDeliveryMode = Just Q.Persistent,
          Q.msgExpiration = Just (Text.pack $ show ttl),
          Q.msgType = Just "synchronization"
        }

    sendNotifications :: RabbitMqChannel -> WSConnection -> IO ()
    sendNotifications chan wsConn = do
      let consumeRabbitMq = forever $ do
            eventData <- getEventData chan
            let msg = case eventData of
                  Left event -> EventMessage event
                  Right sync -> EventSyncMessage sync

            catch (WS.sendBinaryData wsConn.inner (encode msg)) $
              \(err :: SomeException) -> do
                logSendFailure err
                throwIO err

      -- get ack from websocket and forward to rabbitmq
      let consumeWebsocket = forever $ do
            getClientMessage wsConn >>= \case
              AckFullSync -> throwIO UnexpectedAck
              AckMessage ackData -> do
                logAckReceived ackData
                void $ ackMessage chan ackData.deliveryTag ackData.multiple

      -- run both loops concurrently, so that
      --  - notifications are delivered without having to wait for acks
      --  - exceptions on either side do not cause a deadlock
      concurrently_ consumeRabbitMq consumeWebsocket

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
  WSConnection ->
  UserId ->
  Env ->
  ClientId ->
  IO ()
sendFullSyncMessageIfNeeded wsConn uid env cid = do
  row <- C.runClient env.cassandra do
    retry x5 $ query1 q (params LocalQuorum (uid, cid))
  for_ row $ \_ -> sendFullSyncMessage uid cid wsConn env
  where
    q :: PrepQuery R (UserId, ClientId) (Identity (Maybe UserId))
    q =
      [sql| SELECT user_id FROM missed_notifications
            WHERE user_id = ? and client_id = ?
        |]

sendFullSyncMessage ::
  UserId ->
  ClientId ->
  WSConnection ->
  Env ->
  IO ()
sendFullSyncMessage uid cid wsConn env = do
  let event = encode EventFullSync
  WS.sendBinaryData wsConn.inner event
  getClientMessage wsConn >>= \case
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

data WSConnection = WSConnection
  { inner :: WS.Connection,
    activity :: MVar (),
    activityTimeout :: Int,
    pongTimeout :: Int
  }

getClientMessage :: WSConnection -> IO MessageClientToServer
getClientMessage wsConn = do
  msg <- WS.fromDataMessage <$> receiveDataMessageWithTimeout wsConn
  case eitherDecode msg of
    Left err -> throwIO (FailedToParseClientMessage err)
    Right m -> pure m

-- | A modified copy of 'WS.receiveDataMessage' which can detect client
-- inactivity.
receiveDataMessageWithTimeout :: WSConnection -> IO DataMessage
receiveDataMessageWithTimeout wsConn = do
  msg <- WS.receive wsConn.inner
  case msg of
    DataMessage _ _ _ am -> pure am
    ControlMessage cm -> case cm of
      Close i closeMsg -> do
        hasSentClose <- readIORef $ connectionSentClose wsConn.inner
        unless hasSentClose $ send wsConn.inner msg
        throwIO $ CloseRequest i closeMsg
      Pong _ -> do
        _ <- tryPutMVar (connectionHeartbeat wsConn.inner) ()
        receiveDataMessageWithTimeout wsConn
      Ping pl -> do
        _ <- tryPutMVar wsConn.activity ()
        send wsConn.inner (ControlMessage (Pong pl))
        receiveDataMessageWithTimeout wsConn

data WebSocketServerError
  = FailedToParseClientMessage String
  | UnexpectedAck
  deriving (Show)

instance Exception WebSocketServerError
