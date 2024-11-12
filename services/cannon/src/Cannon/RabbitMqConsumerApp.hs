{-# LANGUAGE RecordWildCards #-}

module Cannon.RabbitMqConsumerApp where

import Cannon.App (rejectOnError)
import Cannon.WS hiding (env)
import Cassandra as C
import Control.Concurrent.Async
import Control.Exception (Handler (..), bracket, catch, catches, throwIO, try)
import Control.Monad.Codensity
import Data.Aeson
import Data.Id
import Imports
import Network.AMQP qualified as Q
import Network.AMQP.Extended (withConnection)
import Network.WebSockets
import Network.WebSockets qualified as WS
import System.Logger qualified as Log
import Wire.API.Event.WebSocketProtocol
import Wire.API.Notification

rabbitMQWebSocketApp :: UserId -> ClientId -> Env -> ServerApp
rabbitMQWebSocketApp uid cid e pendingConn = do
  wsVar <- newEmptyMVar
  msgVar <- newEmptyMVar

  bracket (openWebSocket wsVar) closeWebSocket $ \(wsConn, _) ->
    ( do
        sendFullSyncMessageIfNeeded wsVar wsConn uid cid e
        sendNotifications wsConn msgVar wsVar
    )
      `catches` [ handleClientMisbehaving wsConn,
                  handleWebSocketExceptions wsConn
                ]
  where
    logClient =
      Log.field "user" (idToText uid)
        . Log.field "client" (clientToText cid)

    openWebSocket wsVar = do
      wsConn <-
        acceptRequest pendingConn
          `catch` rejectOnError pendingConn
      -- start a reader thread for client messages
      -- this needs to run asynchronously in order to promptly react to
      -- client-side connection termination
      a <- async $ forever $ do
        catch
          ( do
              msg <- getClientMessage wsConn
              putMVar wsVar (Right msg)
          )
          $ \err -> putMVar wsVar (Left err)
      pure (wsConn, a)

    -- this is only needed in case of asynchronous exceptions
    closeWebSocket (wsConn, a) = do
      cancel a
      logCloseWebsocket
      -- ignore any exceptions when sending the close message
      void . try @SomeException $ WS.sendClose wsConn ("" :: ByteString)

    -- Create a rabbitmq consumer that receives messages and saves them into an MVar
    createConsumer ::
      Q.Channel ->
      MVar (Either Q.AMQPException EventData) ->
      IO Q.ConsumerTag
    createConsumer chan msgVar = do
      Q.consumeMsgs chan (clientNotificationQueueName uid cid) Q.Ack $
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
            Q.rejectEnv envelope False
          Right notif ->
            putMVar msgVar . Right $
              EventData notif envelope.envDeliveryTag

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
    sendNotifications ::
      WS.Connection ->
      MVar (Either Q.AMQPException EventData) ->
      MVar (Either ConnectionException MessageClientToServer) ->
      IO ()
    sendNotifications wsConn msgVar wsVar = lowerCodensity $ do
      -- create rabbitmq connection
      conn <- Codensity $ withConnection e.logg e.rabbitmq

      -- create rabbitmq channel
      amqpChan <- Codensity $ bracket (Q.openChannel conn) Q.closeChannel

      -- propagate rabbitmq connection failure
      lift $ Q.addConnectionClosedHandler conn True $ do
        putMVar msgVar $
          Left (Q.ConnectionClosedException Q.Normal "")

      -- register consumer that pushes rabbitmq messages into msgVar
      void $
        Codensity $
          bracket
            (createConsumer amqpChan msgVar)
            (Q.cancelConsumer amqpChan)

      -- get data from msgVar and push to client
      let consumeRabbitMq = forever $ do
            eventData' <- takeMVar msgVar
            either throwIO pure eventData' >>= \eventData -> do
              logEvent eventData.event
              catch (WS.sendBinaryData wsConn (encode (EventMessage eventData))) $
                \(err :: SomeException) -> do
                  logSendFailure err
                  throwIO err

      -- get ack from wsVar and forward to rabbitmq
      let consumeWebsocket = forever $ do
            v <- takeMVar wsVar
            either throwIO pure v >>= \case
              AckFullSync -> throwIO UnexpectedAck
              AckMessage ackData -> do
                logAckReceived ackData
                void $ Q.ackMsg amqpChan ackData.deliveryTag ackData.multiple

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
  MVar (Either ConnectionException MessageClientToServer) ->
  WS.Connection ->
  UserId ->
  ClientId ->
  Env ->
  IO ()
sendFullSyncMessageIfNeeded wsVar wsConn uid cid env = do
  row <- C.runClient env.cassandra do
    retry x5 $ query1 q (params LocalQuorum (uid, cid))
  for_ row $ \_ -> sendFullSyncMessage uid cid wsVar wsConn env
  where
    q :: PrepQuery R (UserId, ClientId) (Identity (Maybe UserId))
    q =
      [sql| SELECT user_id FROM missed_notifications 
            WHERE user_id = ? and client_id = ?
        |]

sendFullSyncMessage ::
  UserId ->
  ClientId ->
  MVar (Either ConnectionException MessageClientToServer) ->
  WS.Connection ->
  Env ->
  IO ()
sendFullSyncMessage uid cid wsVar wsConn env = do
  let event = encode EventFullSync
  WS.sendBinaryData wsConn event
  res <- takeMVar wsVar >>= either throwIO pure
  case res of
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
