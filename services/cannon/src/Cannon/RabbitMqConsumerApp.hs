{-# LANGUAGE RecordWildCards #-}

module Cannon.RabbitMqConsumerApp where

import Cannon.App (rejectOnError)
import Cannon.WS
import Cassandra as C
import Control.Exception (Handler (..), bracket, catch, catches, throwIO, try)
import Control.Monad.Codensity
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
  bracket openWebSocket closeWebSocket $ \wsConn ->
    catches
      ( do
          sendFullSyncMessageIfNeeded wsConn uid cid e
          sendNotifications wsConn
      )
      [ -- handle client misbehaving
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
              WS.sendCloseCode wsConn 1003 ("unexpected-ack" :: ByteString),
        -- handle web socket exception
        Handler $
          \err -> case err of
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
      ]
  where
    logClient =
      Log.field "user" (idToText uid)
        . Log.field "client" (clientToText cid)

    openWebSocket =
      acceptRequest pendingConn
        `catch` rejectOnError pendingConn

    -- this is only needed in case of asynchronous exceptions
    closeWebSocket wsConn = do
      logCloseWebsocket
      -- ignore any exceptions when sending the close message
      void . try @SomeException $ WS.sendClose wsConn ("" :: ByteString)

    -- Create a rabbitmq consumer that receives messages and saves them into an MVar
    createConsumer :: Amqp.Channel -> MVar EventData -> IO Amqp.ConsumerTag
    createConsumer chan msgVar = do
      Amqp.consumeMsgs chan (clientNotificationQueueName uid cid) Amqp.Ack $
        \(msg, envelope) -> case eitherDecode @QueuedNotification msg.msgBody of
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
            Amqp.rejectEnv envelope False
          Right notif ->
            putMVar msgVar $
              EventData notif envelope.envDeliveryTag

    sendNotifications :: WS.Connection -> IO ()
    sendNotifications wsConn = lowerCodensity $ do
      conn <- Codensity $ withConnection e.logg e.rabbitmq
      amqpChan <- Codensity $ bracket (Amqp.openChannel conn) Amqp.closeChannel
      msgVar <- lift newEmptyMVar

      void $
        Codensity $
          bracket
            (createConsumer amqpChan msgVar)
            (Amqp.cancelConsumer amqpChan)

      lift $ forever $ do
        eventData <- takeMVar msgVar
        logEvent eventData.event
        catch (WS.sendBinaryData wsConn (encode (EventMessage eventData))) $
          \(err :: SomeException) -> do
            logSendFailure err
            throwIO err
        getClientMessage wsConn >>= \case
          AckFullSync -> throwIO UnexpectedAck
          AckMessage ackData -> do
            logAckReceived ackData
            void $ Amqp.ackMsg amqpChan ackData.deliveryTag ackData.multiple

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
sendFullSyncMessageIfNeeded :: WS.Connection -> UserId -> ClientId -> Env -> IO ()
sendFullSyncMessageIfNeeded wsConn uid cid e = do
  row <- C.runClient e.cassandra $ do
    retry x5 $ query1 q (params LocalQuorum (uid, cid))
  for_ row $ \_ -> sendFullSyncMessage wsConn
  where
    q :: PrepQuery R (UserId, ClientId) (Identity (Maybe UserId))
    q =
      [sql| SELECT user_id FROM missed_notifications 
            WHERE user_id = ? and client_id = ?
        |]

sendFullSyncMessage :: WS.Connection -> IO ()
sendFullSyncMessage wsConn = do
  WS.sendBinaryData wsConn $ encode EventFullSync
  getClientMessage wsConn >>= \case
    AckMessage _ -> throwIO UnexpectedAck
    AckFullSync -> pure ()

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
