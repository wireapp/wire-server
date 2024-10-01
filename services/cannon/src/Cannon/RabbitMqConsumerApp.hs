module Cannon.RabbitMqConsumerApp where

import Cannon.App (rejectOnError)
import Cannon.WS
import Control.Exception (catch)
import Control.Monad.Catch (Handler (..), MonadThrow, throwM)
import Data.Aeson
import Data.Id
import Imports
import Network.AMQP qualified as Amqp
import Network.AMQP.Extended (withConnection)
import Network.AMQP.Lifted qualified as AmqpL
import Network.WebSockets
import Network.WebSockets qualified as WS
import System.Logger qualified as Log
import UnliftIO (catches)
import Wire.NotificationSubsystem.Interpreter

rabbitMQWebSocketApp :: UserId -> ClientId -> Env -> ServerApp
rabbitMQWebSocketApp uid cid e pendingConn = do
  wsConn <- liftIO (acceptRequest pendingConn `catch` rejectOnError pendingConn)
  withConnection e.logg e.rabbitmq $ \conn -> do
    chan <- liftIO $ Amqp.openChannel conn
    -- TODO: Don't use  the interpreter
    qName <- setUpUserNotificationQueuesImpl chan uid cid
    let cleanup :: (Exception e, MonadThrow m, MonadIO m) => e -> m ()
        cleanup err = do
          Log.err e.logg $ Log.msg (Log.val "Pushing to WS failed") . Log.field "error" (displayException err)
          throwM err
        handlers = [Handler $ cleanup @SomeException, Handler $ cleanup @SomeAsyncException]
    _consumerTag <-
      AmqpL.consumeMsgs chan qName Amqp.Ack (\msg -> pushEventsToWS wsConn msg `catches` handlers)
    forever $ do
      eitherMsg :: Either String ClientMessage <- eitherDecode <$> WS.receiveData wsConn
      case eitherMsg of
        Left err -> error err
        Right msg -> do
          void $ Amqp.ackMsg chan msg.ack False

data ClientMessage = ClientMessage {ack :: Word64}

instance FromJSON ClientMessage where
  parseJSON = withObject "ClientMessage" $ \o -> ClientMessage <$> o .: "ack"

pushEventsToWS :: WS.Connection -> (Amqp.Message, Amqp.Envelope) -> IO ()
pushEventsToWS wsConn (msg, envelope) =
  case eitherDecode @Value msg.msgBody of
    Left e -> error e
    Right payload -> WS.sendBinaryData wsConn (encode $ object ["payload" .= payload, "delivery_tag" .= envelope.envDeliveryTag])
