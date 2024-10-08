module Cannon.RabbitMqConsumerApp where

import Cannon.App (rejectOnError)
import Cannon.WS
import Control.Concurrent.Async (race)
import Control.Exception (Handler (..), catch, catches, throwIO)
import Data.Aeson
import Data.Id
import Imports
import Network.AMQP qualified as Amqp
import Network.AMQP.Extended (withConnection)
import Network.WebSockets
import Network.WebSockets qualified as WS
import System.Logger qualified as Log
import Wire.API.Notification
import Wire.API.WebSocket

rabbitMQWebSocketApp :: UserId -> ClientId -> Env -> ServerApp
rabbitMQWebSocketApp uid cid e pendingConn = do
  wsConn <- liftIO (acceptRequest pendingConn `catch` rejectOnError pendingConn)
  -- TODO: Don't create new conns for every client, this will definitely kill rabbit
  withConnection e.logg e.rabbitmq $ \conn -> do
    chan <- Amqp.openChannel conn -- TODO: should we open a channel for every request? or have a pool of them?
    closeWS <- newEmptyMVar
    amqpConsumerTag <- startWsSender wsConn chan closeWS
    wsReceiverLoop wsConn chan closeWS amqpConsumerTag
  where
    logClient =
      Log.field "user" (idToText uid)
        . Log.field "client" (clientToText cid)

    pushEventsToWS :: WS.Connection -> (Amqp.Message, Amqp.Envelope) -> IO ()
    pushEventsToWS wsConn (msg, envelope) =
      case eitherDecode @Value msg.msgBody of
        Left err -> do
          Log.err e.logg $
            Log.msg (Log.val "failed to decode event from the queue as a JSON")
              . logClient
              . Log.field "parse_error" err
          -- TODO: Make it a nicer exception
          error err
        Right payload -> do
          WS.sendBinaryData wsConn . encode $
            object
              [ "payload" .= payload,
                "delivery_tag" .= envelope.envDeliveryTag
              ]

    startWsSender :: Connection -> Amqp.Channel -> MVar () -> IO Amqp.ConsumerTag
    startWsSender wsConn chan closeWS = do
      let handleException :: (Exception e) => e -> IO ()
          handleException err = do
            Log.err e.logg $
              Log.msg (Log.val "Pushing to WS failed, closing connection")
                . Log.field "error" (displayException err)
                . logClient
            void $ tryPutMVar closeWS ()
            throwIO err

          exceptionHandlers =
            [ Handler $ handleException @SomeException,
              Handler $ handleException @SomeAsyncException
            ]

          qName = clientNotificationQueueName uid cid

      Amqp.consumeMsgs chan qName Amqp.Ack $ \msg ->
        pushEventsToWS wsConn msg `catches` exceptionHandlers

    wsReceiverLoop :: Connection -> Amqp.Channel -> MVar () -> Amqp.ConsumerTag -> IO ()
    wsReceiverLoop wsConn chan closeWS amqpConsumerTag = do
      let handleConnectionClosed :: ConnectionException -> IO ()
          handleConnectionClosed err = do
            Log.info e.logg $
              Log.msg (Log.val "Websocket connection closed")
                . Log.field "error" (displayException err)
                . logClient
            Amqp.cancelConsumer chan amqpConsumerTag
          handleException :: (Exception e) => e -> IO ()
          handleException err = do
            Log.info e.logg $
              Log.msg (Log.val "Unexpected exception in receive loop")
                . Log.field "error" (displayException err)
                . logClient
            Amqp.cancelConsumer chan amqpConsumerTag
            throwIO err
          exceptionHandlers =
            [ Handler $ handleConnectionClosed,
              Handler $ handleException @SomeException,
              Handler $ handleException @SomeAsyncException
            ]
      let loop = do
            -- no timeout necessary here, we want to keep running forever.
            eitherData <- race (takeMVar closeWS) (WS.receiveData wsConn)
            case eitherData of
              Left () -> do
                Log.info e.logg $
                  Log.msg (Log.val "gracefully closing websocket")
                    . logClient
                -- Look at the status codes: https://datatracker.ietf.org/doc/html/rfc6455#section-7.4
                WS.sendClose wsConn ("goaway" :: ByteString)
              Right dat -> case eitherDecode @MessageClientToServer dat of
                Left err -> do
                  Log.info e.logg $
                    Log.msg (Log.val "log failed to parse received message, gracefully closing websocket")
                      . logClient
                  WS.sendClose wsConn ("invalid-message" :: ByteString)
                  throwIO $ FailedToParseClientMessage err
                Right (AckMessage ackData) -> do
                  Log.debug e.logg $
                    Log.msg (Log.val "Received ACK")
                      . Log.field "delivery_tag" ackData.deliveryTag
                      . Log.field "multiple" ackData.multiple
                      . logClient
                  void $ Amqp.ackMsg chan ackData.deliveryTag ackData.multiple
                  loop
      loop `catches` exceptionHandlers

data WebSocketServerError
  = FailedToParseClientMessage String
  | ClientSentAnEvent EventData
  deriving (Show)

instance Exception WebSocketServerError
