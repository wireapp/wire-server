module Cannon.RabbitMqConsumerApp where

import Cannon.App (rejectOnError)
import Cannon.WS
import Control.Concurrent.Async (race)
import Control.Exception (Handler (..), catch, catches, throwIO)
import Data.Aeson
import Data.Id
import Imports
import Network.AMQP qualified as Amqp
import Network.AMQP.Extended (getStableRabbitmqConn)
import Network.WebSockets
import Network.WebSockets qualified as WS
import System.Logger qualified as Log
import Wire.API.Notification
import Wire.API.WebSocket

rabbitMQWebSocketApp :: UserId -> ClientId -> MVar (Maybe Amqp.Connection) -> Env -> ServerApp
rabbitMQWebSocketApp uid cid rConn e pendingConn = do
  wsConn <- liftIO (acceptRequest pendingConn `catch` rejectOnError pendingConn)
  closeWS <- newEmptyMVar

  do
    -- FUTUREWORK: we pool connections, but not channels.  however, channel pooling is also a
    -- thing!  we should generate some performance data using otel and decide whether we want
    -- to do it.
    -- https://stackoverflow.com/questions/10365867/how-can-i-pool-channels-in-rabbitmq
    mConn <- getStableRabbitmqConn rConn
    chan <- maybe (throwIO ConnectionClosed) Amqp.openChannel mConn

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
          -- TODO: extract "Log.msg ..." into helper function.  don't say "pushing" in pulling exceptions.  make everything nicer.
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
