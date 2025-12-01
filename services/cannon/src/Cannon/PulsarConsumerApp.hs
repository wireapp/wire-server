{-# LANGUAGE RecordWildCards #-}

module Cannon.PulsarConsumerApp (pulsarWebSocketApp) where

import Cannon.App (rejectOnError)
import Cannon.Options
import Cannon.WS hiding (env)
import Cassandra as C hiding (batch)
import Conduit (runResourceT)
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Exception (Handler (..), catches)
import Control.Exception.Base
import Control.Lens hiding ((#))
import Control.Monad.Codensity
import Control.Monad.STM qualified as STM
import Data.Aeson hiding (Key)
import Data.Aeson qualified as A
import Data.Base64.Types
import Data.ByteString qualified as BS
import Data.ByteString.Base64
import Data.ByteString.UTF8
import Data.ByteString.UTF8 qualified as BSUTF8
import Data.Id
import Data.Text
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Debug.Trace
import Imports hiding (min, threadDelay)
import Network.WebSockets
import Network.WebSockets qualified as WS
import Network.WebSockets.Connection
import Pulsar.Client qualified as Pulsar
import Pulsar.Client.Logging
import System.Logger qualified as Log
import System.Timeout
import UnliftIO qualified
import Wire.API.Event.WebSocketProtocol
import Wire.API.Notification

data InactivityTimeout = InactivityTimeout
  deriving (Show)

instance Exception InactivityTimeout

-- TODO: The name is a misleading. However, while developing, it's useful to keep the analogies with RabbitMQ.
data PulsarChannel = PulsarChannel
  { -- TODO: Rename: msgChannel
    msgVar :: Chan (PulsarMsgId, ByteString),
    closeSignal :: MVar (),
    acknowledgeMessages :: Chan PulsarMsgId,
    rejectMessages :: Chan PulsarMsgId
  }

newtype PulsarMsgId = PulsarMsgId {unPulsarMsgId :: ByteString}

createPulsarChannel :: UserId -> Maybe ClientId -> Env -> Codensity IO PulsarChannel
createPulsarChannel uid mCid env = do
  msgChannel :: Chan (PulsarMsgId, ByteString) <- lift newChan
  acknowledgeMessages :: Chan PulsarMsgId <- lift newChan
  rejectMessages :: Chan PulsarMsgId <- lift newChan
  closeSignal :: MVar () <- lift $ newEmptyMVar
  unackedMsgsCounter :: TVar Int <- newTVarIO 0
  let subscription = case mCid of
        Nothing -> temporaryRoutingKey uid
        Just cid -> clientNotificationQueueName uid cid
      subscriptionType = case mCid of
        Nothing -> Pulsar.Latest
        Just _cid -> Pulsar.Earliest
  liftIO $
    do
      traceM $ "Connecting ..."
      void . async $ Pulsar.withClient (Pulsar.defaultClientConfiguration {Pulsar.clientLogger = Just (pulsarClientLogger "createPulsarChannel" env.logg)}) env.pulsarUrl $ do
        let topic = Pulsar.Topic . Pulsar.TopicName $ "persistent://wire/user-notifications/" ++ unpack (userRoutingKey uid)
        traceM $ "newConsumer " ++ show topic
        Pulsar.withConsumerNoUnsubscribe
          ( Pulsar.defaultConsumerConfiguration
              { Pulsar.consumerType = Just Pulsar.ConsumerExclusive,
                Pulsar.consumerSubscriptionInitialPosition = Just subscriptionType
              }
          )
          ("cannon-websocket-" ++ unpack subscription)
          topic
          (onPulsarError "createPulsarChannel consumer" env.logg)
          $ do
            traceM $ "Ready"
            receiveMsgsAsync :: Async () <- receiveMsgs msgChannel unackedMsgsCounter
            blockOnCloseSignalAsync :: Async () <- blockOnCloseSignal closeSignal
            acknowledgeMsgsAsync <- acknowledgeMsgs acknowledgeMessages unackedMsgsCounter
            rejectMsgsAsync <- rejectMsgs rejectMessages unackedMsgsCounter
            void $ UnliftIO.waitAnyCancel [receiveMsgsAsync, blockOnCloseSignalAsync, acknowledgeMsgsAsync, rejectMsgsAsync]
      pure ()
  traceM "createPulsarChannel: Done"
  pure $
    ( PulsarChannel
        { msgVar = msgChannel,
          closeSignal = closeSignal,
          acknowledgeMessages = acknowledgeMessages,
          rejectMessages = rejectMessages
        },
      PulsarQueueInfo subscription
    )
  where
    receiveMsgs :: (UnliftIO.MonadUnliftIO m) => Chan (PulsarMsgId, ByteString) -> TVar Int -> ReaderT Pulsar.Consumer m (Async ())
    receiveMsgs msgChannel unackedMsgsCounter = UnliftIO.async . forever $ do
      liftIO $ waitUntilCounterBelow unackedMsgsCounter 500
      Pulsar.receiveMessage (liftIO . onPulsarError "receiveMessage" env.logg) $ do
        content <- Pulsar.messageContent
        traceM $ "XXX - received message with content " ++ BSUTF8.toString content
        msgId <- Pulsar.messageId Pulsar.messageIdSerialize
        liftIO $ writeChan msgChannel (PulsarMsgId msgId, content)
        liftIO $ incCounter unackedMsgsCounter
        traceM $ "XXX - wrote message to channel:" ++ BSUTF8.toString content

    blockOnCloseSignal :: (UnliftIO.MonadUnliftIO m) => MVar () -> m (Async ())
    blockOnCloseSignal = UnliftIO.async . readMVar

    acknowledgeMsgs :: (UnliftIO.MonadUnliftIO m) => Chan PulsarMsgId -> TVar Int -> ReaderT Pulsar.Consumer m (Async ())
    acknowledgeMsgs chan unackedMsgsCounter =
      UnliftIO.async . forever $ do
        PulsarMsgId msgId <- UnliftIO.readChan chan
        consumer :: Pulsar.Consumer <- ask
        traceM $ "acknowledgeMsgs"
        result <- liftIO $ Pulsar.withDeserializedMessageId consumer msgId Pulsar.acknowledgeMessageId
        liftIO $ logPulsarResult "createPulsarChannel - acknowledge message result: " env.logg result
        liftIO $ decCounter unackedMsgsCounter

    rejectMsgs :: (UnliftIO.MonadUnliftIO m) => Chan PulsarMsgId -> TVar Int -> ReaderT Pulsar.Consumer m (Async ())
    rejectMsgs chan unackedMsgsCounter =
      UnliftIO.async . forever $ do
        PulsarMsgId msgId <- UnliftIO.readChan chan
        consumer :: Pulsar.Consumer <- ask
        Pulsar.withDeserializedMessageId consumer msgId Pulsar.acknowledgeNegativeMessageId
        liftIO $ decCounter unackedMsgsCounter

    incCounter :: TVar Int -> IO ()
    incCounter tv = atomically $ modifyTVar' tv (+ 1)

    decCounter :: TVar Int -> IO ()
    decCounter tv = atomically $ modifyTVar' tv (subtract 1)

    waitUntilCounterBelow :: TVar Int -> Int -> IO ()
    waitUntilCounterBelow tv threshold = atomically $ do
      v <- readTVar tv
      STM.check (v < threshold) -- blocks (retry) until v < threshold

pulsarWebSocketApp :: UserId -> Maybe ClientId -> Maybe Text -> Env -> ServerApp
pulsarWebSocketApp uid mcid mSyncMarkerId e pendingConn =
  lowerCodensity $
    do
      chan <- createPulsarChannel uid mcid e
      conn <- Codensity $ bracket openWebSocket closeWebSocket
      activity <- liftIO newEmptyMVar
      let wsConn =
            WSConnection
              { inner = conn,
                activity,
                activityTimeout = e.wsOpts.activityTimeout,
                pongTimeout = e.wsOpts.pongTimeout
              }

      main <- Codensity
        $ withAsync
        $ flip
          catches
          [ -- TODO: Review exceptions. pulsar-hs and amqp exceptions surely differ.
            handleClientMisbehaving conn,
            handleWebSocketExceptions conn,
            handleInactivity conn,
            handleOtherExceptions conn
          ]
        $ do
          traverse_ (sendFullSyncMessageIfNeeded wsConn uid e) mcid
          traverse_ (publishSyncMessage uid . mkSynchronizationMessage) mSyncMarkerId
          sendNotifications chan wsConn

      let monitor = do
            timeout wsConn.activityTimeout (takeMVar wsConn.activity) >>= \case
              Just _ -> monitor
              Nothing -> do
                WS.sendPing wsConn.inner ("ping" :: Text)
                timeout wsConn.pongTimeout (takeMVar wsConn.inner.connectionHeartbeat) >>= \case
                  Just _ -> monitor
                  Nothing -> cancelWith main InactivityTimeout

      _ <- Codensity $ withAsync monitor

      liftIO $ wait main
      -- TODO: This probably needs more exception handling... However, I'd like to see the principle working first.
      putMVar chan.closeSignal ()
  where
    publishSyncMessage :: UserId -> ByteString -> IO ()
    publishSyncMessage userId message =
      Pulsar.withClient (Pulsar.defaultClientConfiguration {Pulsar.clientLogger = Just (pulsarClientLogger "publishSyncMessage" e.logg)}) e.pulsarUrl $ do
        let topic = Pulsar.TopicName $ "persistent://wire/user-notifications/" ++ unpack (userRoutingKey userId)
        Pulsar.withProducer Pulsar.defaultProducerConfiguration topic (onPulsarError "publishSyncMessage producer" e.logg) $ do
          result <- runResourceT $ do
            (_, message') <- Pulsar.buildMessage $ Pulsar.defaultMessageBuilder {Pulsar.content = Just $ message}
            lift $ Pulsar.sendMessage message'
          liftIO $ logPulsarResult "consumeWebsocket" e.logg result
          pure ()

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

    getEventData :: PulsarChannel -> IO (Either EventData SynchronizationData)
    getEventData chan = do
      traceM $ "getEventData called"
      (msgId, msg) <- readChan chan.msgVar
      traceM $ "getEventData received message" <> show (toString msg)
      decMsg :: PulsarMessage <- either (\err -> logParseError err >> error "Unexpected parse error") pure $ A.eitherDecode (BS.fromStrict msg)
      case decMsg.msgType of
        Just "synchronization" -> do
          let syncData =
                SynchronizationData
                  { markerId = decMsg.msgBody,
                    deliveryTag = encodeMsgId msgId
                  }
          pure $ Right syncData
        _ -> do
          case eitherDecode @QueuedNotification ((BS.fromStrict . TE.encodeUtf8) decMsg.msgBody) of
            Left err ->
              do
                logParseError err
                writeChan chan.rejectMessages msgId
                -- TODO: Deadlettering hasn't been configured, yet. See e.g. https://pulsar.apache.org/docs/4.1.x/client-libraries-websocket/#negatively-acknowledge-messages
                getEventData chan
            Right notif -> do
              logEvent notif
              pure $
                Left $
                  EventData
                    { event = notif,
                      deliveryTag = encodeMsgId msgId
                    }

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

    handleInactivity wsConn =
      Handler $ \(_ :: InactivityTimeout) -> do
        Log.info e.logg $
          Log.msg (Log.val "Closing websocket due to inactivity")
            . logClient
        WS.sendCloseCode wsConn 1002 ("inactivity" :: ByteString)

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

    handleOtherExceptions wsConn = Handler $
      \(err :: SomeException) -> do
        WS.sendCloseCode wsConn 1003 ("internal-error" :: ByteString)
        throwIO err

    mkSynchronizationMessage :: StrictText -> ByteString
    mkSynchronizationMessage markerId =
      -- TODO: Check all fromStrict/toStrict calls: It makes no sense to be "sometimes lazy".
      BS.toStrict . encode $
        PulsarMessage
          { msgBody = markerId,
            msgContentType = "text/plain; charset=utf-8",
            msgType = Just "synchronization"
          }

    sendNotifications :: PulsarChannel -> WSConnection -> IO ()
    sendNotifications chan wsConn = do
      traceM $ "XXX - sendNotifications called "
      let consumeRabbitMq = forever $ do
            traceM $ "XXX - sendNotifications consumeRabbitMq called "
            eventData <- getEventData chan
            let msg = case eventData of
                  Left event -> EventMessage event
                  Right sync -> EventSyncMessage sync
            traceM $ "XXX - sendNotifications sending ... " <> show msg
            catch (WS.sendBinaryData wsConn.inner (encode msg)) $
              \(err :: SomeException) -> do
                logSendFailure err
                throwIO err

      -- get ack from websocket and forward to rabbitmq
      let consumeWebsocket = forever $ do
            getClientMessage wsConn >>= \case
              AckFullSync -> throwIO UnexpectedAck
              AckMessage ackData -> do
                logAckReceived ackData
                writeChan chan.acknowledgeMessages $ decodeMsgId ackData.deliveryTag

      -- run both loops concurrently, so that
      --  - notifications are delivered without having to wait for acks
      --  - exceptions on either side do not cause a deadlock
      concurrently_ consumeRabbitMq consumeWebsocket

    decodeMsgId :: String -> PulsarMsgId
    decodeMsgId = either (error . ("decodeMsgId: " ++) . unpack) PulsarMsgId . decodeBase64Untyped . BSUTF8.fromString

    encodeMsgId :: PulsarMsgId -> String
    encodeMsgId = T.unpack . extractBase64 . encodeBase64 . unPulsarMsgId

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
  WSConnection ->
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
  WSConnection ->
  Env ->
  IO ()
sendFullSyncMessage uid cid wsConn env = do
  let event = encode EventFullSync
  WS.sendBinaryData wsConn.inner event
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

data WSConnection = WSConnection
  { inner :: WS.Connection,
    activity :: MVar (),
    activityTimeout :: Int,
    pongTimeout :: Int
  }

getClientMessage :: WSConnection -> IO MessageClientToServer
getClientMessage wsConn = do
  msg <- WS.fromDataMessage <$> receiveDataMessageWithTimeout wsConn
  case eitherDecode msg of
    Left err -> throwIO (FailedToParseClientMessage err)
    Right m -> pure m

-- | A modified copy of 'WS.receiveDataMessage' which can detect client
-- inactivity.
receiveDataMessageWithTimeout :: WSConnection -> IO DataMessage
receiveDataMessageWithTimeout wsConn = do
  msg <- WS.receive wsConn.inner
  case msg of
    DataMessage _ _ _ am -> pure am
    ControlMessage cm -> case cm of
      Close i closeMsg -> do
        hasSentClose <- readIORef $ connectionSentClose wsConn.inner
        unless hasSentClose $ send wsConn.inner msg
        throwIO $ CloseRequest i closeMsg
      Pong _ -> do
        _ <- tryPutMVar (connectionHeartbeat wsConn.inner) ()
        receiveDataMessageWithTimeout wsConn
      Ping pl -> do
        _ <- tryPutMVar wsConn.activity ()
        send wsConn.inner (ControlMessage (Pong pl))
        receiveDataMessageWithTimeout wsConn

data WebSocketServerError
  = FailedToParseClientMessage String
  | UnexpectedAck
  deriving (Show)

instance Exception WebSocketServerError
