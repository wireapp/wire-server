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
import Data.Id
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
  bracket openWebSocket closeWebSocket $ \wsConn ->
    ( do
        traverse_ (sendFullSyncMessageIfNeeded wsConn uid e) mcid
        sendNotifications wsConn
    )
      `catches` [ handleClientMisbehaving wsConn,
                  handleWebSocketExceptions wsConn,
                  handleOtherExceptions wsConn
                ]
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

    handleOtherExceptions wsConn = Handler $
      \(err :: SomeException) -> do
        WS.sendCloseCode wsConn 1003 ("internal-error" :: ByteString)
        throwIO err

    sendNotifications :: WS.Connection -> IO ()
    sendNotifications wsConn = lowerCodensity $ do
      let createQueue chan = case mcid of
            Nothing -> Codensity $ \k -> do
              (queueName, _, _) <-
                Q.declareQueue chan $
                  newQueue
                    { Q.queueExclusive = True,
                      Q.queueAutoDelete = True
                    }
              for_ [userRoutingKey uid, temporaryRoutingKey uid] $
                Q.bindQueue chan queueName userNotificationExchangeName
              k queueName
            Just cid -> Codensity $ \k -> k $ clientNotificationQueueName uid cid

      chan <- createChannel e.pool createQueue

      let consumeRabbitMq = forever $ do
            eventData <- getEventData chan
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
