module Cannon.RabbitMqConsumerApp where

import Cannon.App (rejectOnError)
import Cannon.WS
import Control.Concurrent.Async (race)
import Control.Exception (Handler (..), bracket, catch, catches, throwIO)
import Data.Aeson
import Data.Id
import Imports
import Network.AMQP qualified as Amqp
import Network.AMQP.Extended (withConnection)
import Network.WebSockets
import Network.WebSockets qualified as WS
import System.Logger qualified as Log
import Wire.API.Event.WebSocketProtocol
import Wire.API.Notification

rabbitMQWebSocketApp :: UserId -> ClientId -> Env -> ServerApp
rabbitMQWebSocketApp uid cid e pendingConn = do
  wsConn <- liftIO (acceptRequest pendingConn `catch` rejectOnError pendingConn)
  -- FUTUREWORK: Pool connections
  withConnection e.logg e.rabbitmq $ \conn -> do
    bracket (Amqp.openChannel conn) (Amqp.closeChannel) $ \chan -> do
      closeWS <- newEmptyMVar
      bracket (startWsSender wsConn chan closeWS) (Amqp.cancelConsumer chan) $ \_ -> do
        wsReceiverLoop wsConn chan closeWS
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
          -- This message cannot be parsed, make sure it doesn't requeue. There
          -- is no need to throw an error and kill the websocket as this is
          -- probably caused by a bug or someone messing with RabbitMQ.
          --
          -- The bug case is slightly dangerous as it could drop a lot of events
          -- en masse, if at some point we decide that Events should not be
          -- pushed as JSONs, hopefully we think of the parsing side if/when
          -- that happens.
          Amqp.rejectEnv envelope False
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

      Amqp.consumeMsgs chan qName Amqp.Ack $ \msgWithEnv ->
        pushEventsToWS wsConn msgWithEnv `catches` exceptionHandlers

    wsReceiverLoop :: Connection -> Amqp.Channel -> MVar () -> IO ()
    wsReceiverLoop wsConn chan closeWS = do
      let loop = do
            -- no timeout necessary here, we want to keep running forever.
            eitherData <- race (takeMVar closeWS) (WS.receiveData wsConn)
            case eitherData of
              Left () -> do
                Log.debug e.logg $
                  Log.msg (Log.val "Closing the websocket")
                    . logClient
                WS.sendClose wsConn ("" :: ByteString)
              Right dat -> case eitherDecode @MessageClientToServer dat of
                Left err -> do
                  Log.info e.logg $
                    Log.msg (Log.val "Failed to parse received message, closing websocket")
                      . logClient
                  WS.sendCloseCode wsConn 1003 ("failed-to-parse" :: ByteString)
                  throwIO $ FailedToParseClientMessage err
                Right (AckMessage ackData) -> do
                  Log.debug e.logg $
                    Log.msg (Log.val "Received ACK")
                      . Log.field "delivery_tag" ackData.deliveryTag
                      . Log.field "multiple" ackData.multiple
                      . logClient
                  void $ Amqp.ackMsg chan ackData.deliveryTag ackData.multiple
                  loop

          handleConnectionClosed :: ConnectionException -> IO ()
          handleConnectionClosed connException = do
            case connException of
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
                    . Log.field "error" (displayException connException)
                    . logClient
                WS.send wsConn (WS.ControlMessage $ WS.Close 1003 "failed-to-parse")
          handleException :: (Exception e) => e -> IO ()
          handleException err = do
            Log.err e.logg $
              Log.msg (Log.val "Unexpected exception in receive loop")
                . Log.field "error" (displayException err)
                . logClient
            throwIO err
      loop
        `catches` [ Handler $ handleConnectionClosed,
                    Handler $ handleException @SomeException,
                    Handler $ handleException @SomeAsyncException
                  ]

data WebSocketServerError
  = FailedToParseClientMessage String
  deriving (Show)

instance Exception WebSocketServerError
