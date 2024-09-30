module Cannon.RabbitMqConsumerApp where

import Cannon.App (rejectOnError)
import Cannon.WS
import Control.Exception (catch)
import Data.Id
import Imports
import Network.AMQP qualified as Amqp
import Network.AMQP.Extended (RabbitMqHooks (..), openConnectionWithRetries)
import Network.AMQP.Lifted qualified as AmqpL
import Network.WebSockets
import Network.WebSockets qualified as WS
import Wire.NotificationSubsystem.Interpreter

rabbitMQWebSocketApp :: UserId -> ClientId -> Env -> ServerApp
rabbitMQWebSocketApp uid cid e pendingConn = do
  wsConn <- liftIO (acceptRequest pendingConn `catch` rejectOnError pendingConn)
  openConnectionWithRetries e.logg e.rabbitmq $
    RabbitMqHooks
      { onNewChannel = \chan -> do
          -- declareQueue for the client
          -- TODO: Don't use  the interpreter
          qName <- setUpUserNotificationQueuesImpl chan uid cid
          void $ AmqpL.consumeMsgs chan qName Amqp.Ack (pushEventsToWS wsConn),
        -- subscribe to the queue
        onChannelException = \_ -> WS.sendClose wsConn ("channel-exception" :: ByteString),
        onConnectionClose = WS.sendClose wsConn ("rabbitmq-conn-close" :: ByteString)
      }

pushEventsToWS :: WS.Connection -> (Amqp.Message, Amqp.Envelope) -> IO ()
pushEventsToWS wsConn (msg, _envelope) =
  WS.sendBinaryData wsConn msg.msgBody
