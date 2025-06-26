{-# LANGUAGE RecordWildCards #-}

module Cannon.RabbitMqConsumerApp where

import Cannon.App (rejectOnError)
import Cannon.RabbitMq
import Cannon.WS hiding (env)
import Cassandra as C hiding (batch)
import Control.Concurrent.Async
import Control.Exception (Handler (..), bracket, catch, catches, throwIO, try)
import Control.Lens hiding ((#))
import Control.Monad.Codensity
import Data.Aeson hiding (Key)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Id
import Data.List.NonEmpty qualified as NonEmpty
import Imports hiding (min, threadDelay)
import Network.AMQP (newQueue)
import Network.AMQP qualified as Q
import Network.WebSockets
import Network.WebSockets qualified as WS
import System.Logger qualified as Log
import Wire.API.Event.WebSocketProtocol
import Wire.API.Notification

rabbitMQWebSocketApp :: UserId -> Maybe ClientId -> Env -> ServerApp
rabbitMQWebSocketApp uid mcid e pendingConn = do
  runCodensity (createChannel uid mcid e.pool createQueue) runWithChannel
    `catches` [handleTooManyChannels]
  where
    logClient =
      Log.field "user" (idToText uid)
        . Log.field "client" (maybe "<temporary>" clientToText mcid)

    runWithChannel (chan, queueInfo) = bracket openWebSocket closeWebSocket $ \wsConn ->
      ( do
          traverse_ (sendFullSyncMessageIfNeeded wsConn uid e) mcid
          sendNotifications chan queueInfo wsConn
      )
        `catches` [ handleClientMisbehaving wsConn,
                    handleWebSocketExceptions wsConn,
                    handleRabbitMqChannelException wsConn,
                    handleOtherExceptions wsConn
                  ]

    openWebSocket =
      acceptRequest pendingConn
        `catch` rejectOnError pendingConn

    closeWebSocket wsConn = do
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

    handleTooManyChannels = Handler $
      \TooManyChannels ->
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

    endOfInitialSyncNotifType :: Text
    endOfInitialSyncNotifType = "notification.end-of-initial-sync"

    endOfInitialSyncMsg notificationId =
      Q.newMsg
        { Q.msgBody =
            Aeson.encode . queuedNotification notificationId . NonEmpty.singleton $
              KM.fromList ["type" Aeson..= endOfInitialSyncNotifType],
          Q.msgContentType = Just "application/json",
          Q.msgDeliveryMode = Just Q.NonPersistent,
          Q.msgExpiration = Just "0"
        }

    sendNotifications :: RabbitMqChannel -> QueueInfo -> WS.Connection -> IO ()
    sendNotifications chan queueInfo wsConn = do
      -- Empty when pending, full when done
      initialSync <- newEmptyMVar
      unackedMessages <- newIORef (0 :: Int)

      let publishEndOfInitialSync = do
            notificationId <- mkNotificationId
            void $ Q.publishMsg chan.inner "" queueInfo.queueName (endOfInitialSyncMsg notificationId)

      when (queueInfo.messageCount == 0) $ do
        publishEndOfInitialSync

      let consumeRabbitMq = forever $ do
            eventData <- getEventData chan

            whenM (isEmptyMVar initialSync) $ do
              let notif = NonEmpty.head (eventData.event ^. queuedNotificationPayload)
              case KM.lookup "type" notif of
                Just (Aeson.String typ)
                  | typ == endOfInitialSyncNotifType ->
                      void $ tryPutMVar initialSync ()
                _ -> pure ()

            -- This looks like a duplicate check, but the previous whenM can
            -- modify this value, so we should check again. This avoids sending
            -- duplicate end-of-initial-sync messages.
            whenM (isEmptyMVar initialSync) $ do
              atomicModifyIORef' unackedMessages (\x -> (x + 1, ()))

            catch (WS.sendBinaryData wsConn (encode (EventMessage eventData))) $
              \(err :: SomeException) -> do
                logSendFailure err
                throwIO err

      -- get ack from websocket and forward to rabbitmq
      let consumeWebsocket = forever $ do
            getClientMessage wsConn >>= \case
              AckFullSync -> throwIO UnexpectedAck
              AckMessage ackData -> do
                logAckReceived ackData
                isInitialSyncPending <- isEmptyMVar initialSync
                when isInitialSyncPending $ do
                  unackedCount <- atomicModifyIORef' unackedMessages (\x -> (x - 1, x - 1))

                  when (unackedCount == 0 && isInitialSyncPending) $ do
                    publishEndOfInitialSync

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
  WS.Connection ->
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
  WS.Connection ->
  Env ->
  IO ()
sendFullSyncMessage uid cid wsConn env = do
  let event = encode EventFullSync
  WS.sendBinaryData wsConn event
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
