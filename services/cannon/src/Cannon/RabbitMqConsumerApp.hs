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
  closeWS <- newEmptyMVar
  -- TODO: Don't create new conns for every client, this will definitely kill rabbit
  withConnection e.logg e.rabbitmq $ \conn -> do
    chan <- Amqp.openChannel conn -- TODO: should we open a channel for every request? or have a pool of them?
    let handleConsumerError :: (Exception e) => e -> IO ()
        handleConsumerError err = do
          Log.err e.logg $
            Log.msg (Log.val "Pushing to WS failed, closing connection")
              . Log.field "error" (displayException err)
              . Log.field "user" (idToText uid)
              . Log.field "client" (clientToText cid)
          void $ tryPutMVar closeWS ()
          throwIO err

        handleConnectionClosed :: ConnectionException -> IO ()
        handleConnectionClosed err = do
          Log.info e.logg $
            Log.msg (Log.val "Pushing to WS failed, closing connection")
              . Log.field "error" (displayException err)
              . Log.field "user" (idToText uid)
              . Log.field "client" (clientToText cid)
          void $ tryPutMVar closeWS ()

        handlers =
          [ Handler $ handleConnectionClosed,
            Handler $ handleConsumerError @SomeException,
            Handler $ handleConsumerError @SomeAsyncException
          ]
        qName = clientNotificationQueueName uid cid

    _consumerTag <-
      Amqp.consumeMsgs chan qName Amqp.Ack (\msg -> pushEventsToWS wsConn msg `catches` handlers)

    let wsReceiverLoop = do
          eitherData <- race (takeMVar closeWS) (WS.receiveData wsConn) -- no timeout necessary here, we want to keep running forever.
          case eitherData of
            Left () -> do
              Log.info e.logg $
                Log.msg (Log.val "gracefully closing websocket")
                  . Log.field "user" (idToText uid)
                  . Log.field "client" (clientToText cid)
              WS.sendClose wsConn ("goaway" :: ByteString)
            Right dat -> case eitherDecode @MessageClientToServer dat of
              Left err -> do
                WS.sendClose wsConn ("invalid-message" :: ByteString)
                throwIO $ FailedToParseClientMessage err
              Right (AckMessage ackData) -> do
                void $ Amqp.ackMsg chan ackData.deliveryTag ackData.multiple
                wsReceiverLoop
    wsReceiverLoop `catches` handlers

data WebSocketServerError
  = FailedToParseClientMessage String
  | ClientSentAnEvent EventData
  deriving (Show)

instance Exception WebSocketServerError

pushEventsToWS :: WS.Connection -> (Amqp.Message, Amqp.Envelope) -> IO ()
pushEventsToWS wsConn (msg, envelope) =
  case eitherDecode @Value msg.msgBody of
    Left e -> error e
    Right payload -> WS.sendBinaryData wsConn (encode $ object ["payload" .= payload, "delivery_tag" .= envelope.envDeliveryTag])
