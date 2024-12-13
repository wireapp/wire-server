module Test.Events where

import API.Brig
import API.BrigCommon
import API.Common
import API.Galley
import API.Gundeck
import qualified Control.Concurrent.Timeout as Timeout
import Control.Monad.Codensity
import Control.Monad.RWS (asks)
import Control.Monad.Trans.Class
import Control.Retry
import Data.ByteString.Conversion (toByteString')
import qualified Data.Text as Text
import Data.Timeout
import Network.AMQP.Extended
import Network.RabbitMqAdmin
import qualified Network.WebSockets as WS
import Notifications
import SetupHelpers
import Testlib.Prelude hiding (assertNoEvent)
import Testlib.ResourcePool (acquireResources)
import UnliftIO hiding (handle)

testConsumeEventsOneWebSocket :: (HasCallStack) => App ()
testConsumeEventsOneWebSocket = do
  alice <- randomUser OwnDomain def

  lastNotifResp <-
    retrying
      (constantDelay 10_000 <> limitRetries 10)
      (\_ resp -> pure $ resp.status == 404)
      (\_ -> getLastNotification alice def)
  lastNotifId <- lastNotifResp.json %. "id" & asString

  client <- addClient alice def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
  clientId <- objId client

  runCodensity (createEventsWebSocket alice clientId) $ \ws -> do
    deliveryTag <- assertEvent ws $ \e -> do
      e %. "type" `shouldMatch` "event"
      e %. "data.event.payload.0.type" `shouldMatch` "user.client-add"
      e %. "data.event.payload.0.client.id" `shouldMatch` clientId
      e %. "data.delivery_tag"
    assertNoEvent_ ws

    sendAck ws deliveryTag False
    assertNoEvent_ ws

    handle <- randomHandle
    putHandle alice handle >>= assertSuccess

    assertEvent ws $ \e -> do
      e %. "type" `shouldMatch` "event"
      e %. "data.event.payload.0.type" `shouldMatch` "user.update"
      e %. "data.event.payload.0.user.handle" `shouldMatch` handle

  -- No new notifications should be stored in Cassandra as the user doesn't have
  -- any legacy clients
  getNotifications alice def {since = Just lastNotifId} `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    shouldBeEmpty $ resp.json %. "notifications"

testConsumeEventsForDifferentUsers :: (HasCallStack) => App ()
testConsumeEventsForDifferentUsers = do
  alice <- randomUser OwnDomain def
  bob <- randomUser OwnDomain def

  aliceClient <- addClient alice def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
  aliceClientId <- objId aliceClient

  bobClient <- addClient bob def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
  bobClientId <- objId bobClient

  lowerCodensity $ do
    aliceWS <- createEventsWebSocket alice aliceClientId
    bobWS <- createEventsWebSocket bob bobClientId
    lift $ assertClientAdd aliceClientId aliceWS
    lift $ assertClientAdd bobClientId bobWS
  where
    assertClientAdd :: (HasCallStack) => String -> EventWebSocket -> App ()
    assertClientAdd clientId ws = do
      deliveryTag <- assertEvent ws $ \e -> do
        e %. "data.event.payload.0.type" `shouldMatch` "user.client-add"
        e %. "data.event.payload.0.client.id" `shouldMatch` clientId
        e %. "data.delivery_tag"
      assertNoEvent_ ws
      sendAck ws deliveryTag False

testConsumeEventsWhileHavingLegacyClients :: (HasCallStack) => App ()
testConsumeEventsWhileHavingLegacyClients = do
  alice <- randomUser OwnDomain def

  -- Even if alice has no clients, the notifications should still be persisted
  -- in Cassandra. This choice is kinda arbitrary as these notifications
  -- probably don't mean much, however, it ensures backwards compatibility.
  lastNotifId <-
    awaitNotification alice noValue (const $ pure True) >>= \notif -> do
      notif %. "payload.0.type" `shouldMatch` "user.activate"
      -- There is only one notification (at the time of writing), so we assume
      -- it to be the last one.
      notif %. "id" & asString

  oldClient <- addClient alice def {acapabilities = Just []} >>= getJSON 201

  withWebSocket (alice, "anything-but-conn", oldClient %. "id") $ \oldWS -> do
    newClient <- addClient alice def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
    newClientId <- newClient %. "id" & asString

    oldNotif <- awaitMatch isUserClientAddNotif oldWS
    oldNotif %. "payload.0.client.id" `shouldMatch` newClientId

    runCodensity (createEventsWebSocket alice newClientId) $ \ws ->
      assertEvent ws $ \e -> do
        e %. "data.event.payload.0.type" `shouldMatch` "user.client-add"
        e %. "data.event.payload.0.client.id" `shouldMatch` newClientId

  -- All notifs are also in Cassandra because of the legacy client
  getNotifications alice def {since = Just lastNotifId} `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "notifications.0.payload.0.type" `shouldMatch` "user.client-add"
    resp.json %. "notifications.1.payload.0.type" `shouldMatch` "user.client-add"

testConsumeEventsAcks :: (HasCallStack) => App ()
testConsumeEventsAcks = do
  alice <- randomUser OwnDomain def
  client <- addClient alice def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
  clientId <- objId client

  runCodensity (createEventsWebSocket alice clientId) $ \ws -> do
    assertEvent ws $ \e -> do
      e %. "data.event.payload.0.type" `shouldMatch` "user.client-add"
      e %. "data.event.payload.0.client.id" `shouldMatch` clientId

  -- without ack, we receive the same event again
  runCodensity (createEventsWebSocket alice clientId) $ \ws -> do
    deliveryTag <- assertEvent ws $ \e -> do
      e %. "data.event.payload.0.type" `shouldMatch` "user.client-add"
      e %. "data.event.payload.0.client.id" `shouldMatch` clientId
      e %. "data.delivery_tag"
    sendAck ws deliveryTag False

  runCodensity (createEventsWebSocket alice clientId) $ \ws -> do
    assertNoEvent_ ws

testConsumeEventsMultipleAcks :: (HasCallStack) => App ()
testConsumeEventsMultipleAcks = do
  alice <- randomUser OwnDomain def
  client <- addClient alice def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
  clientId <- objId client

  handle <- randomHandle
  putHandle alice handle >>= assertSuccess

  runCodensity (createEventsWebSocket alice clientId) $ \ws -> do
    assertEvent ws $ \e -> do
      e %. "data.event.payload.0.type" `shouldMatch` "user.client-add"
      e %. "data.event.payload.0.client.id" `shouldMatch` clientId

    deliveryTag <- assertEvent ws $ \e -> do
      e %. "data.event.payload.0.type" `shouldMatch` "user.update"
      e %. "data.event.payload.0.user.handle" `shouldMatch` handle
      e %. "data.delivery_tag"

    sendAck ws deliveryTag True

  runCodensity (createEventsWebSocket alice clientId) $ \ws -> do
    assertNoEvent_ ws

testConsumeEventsAckNewEventWithoutAckingOldOne :: (HasCallStack) => App ()
testConsumeEventsAckNewEventWithoutAckingOldOne = do
  alice <- randomUser OwnDomain def
  client <- addClient alice def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
  clientId <- objId client

  handle <- randomHandle
  putHandle alice handle >>= assertSuccess

  runCodensity (createEventsWebSocket alice clientId) $ \ws -> do
    assertEvent ws $ \e -> do
      e %. "data.event.payload.0.type" `shouldMatch` "user.client-add"
      e %. "data.event.payload.0.client.id" `shouldMatch` clientId

    deliveryTagHandleAdd <- assertEvent ws $ \e -> do
      e %. "data.event.payload.0.type" `shouldMatch` "user.update"
      e %. "data.event.payload.0.user.handle" `shouldMatch` handle
      e %. "data.delivery_tag"

    -- Only ack the handle add delivery tag
    sendAck ws deliveryTagHandleAdd False

  -- Expect client-add event to be delivered again.
  runCodensity (createEventsWebSocket alice clientId) $ \ws -> do
    deliveryTagClientAdd <- assertEvent ws $ \e -> do
      e %. "data.event.payload.0.type" `shouldMatch` "user.client-add"
      e %. "data.event.payload.0.client.id" `shouldMatch` clientId
      e %. "data.delivery_tag"

    sendAck ws deliveryTagClientAdd False

  runCodensity (createEventsWebSocket alice clientId) $ \ws -> do
    assertNoEvent_ ws

testEventsDeadLettered :: (HasCallStack) => App ()
testEventsDeadLettered = do
  let notifTTL = 1 # Second
  withModifiedBackend (def {gundeckCfg = setField "settings.notificationTTL" (notifTTL #> Second)}) $ \domain -> do
    alice <- randomUser domain def

    -- This generates an event
    client <- addClient alice def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
    clientId <- objId client

    -- We expire the add client event by waiting it out
    Timeout.threadDelay (notifTTL + 500 # MilliSecond)

    -- Generate a second event
    handle1 <- randomHandle
    putHandle alice handle1 >>= assertSuccess

    runCodensity (createEventsWebSocket alice clientId) $ \ws -> do
      assertEvent ws $ \e -> do
        e %. "type" `shouldMatch` "notifications.missed"

      -- Until we ack the full sync, we can't get new events
      ackFullSync ws

      -- withEventsWebSocket alice clientId $ \eventsChan ackChan -> do
      -- Now we can see the next event
      assertEvent ws $ \e -> do
        e %. "data.event.payload.0.type" `shouldMatch` "user.update"
        e %. "data.event.payload.0.user.handle" `shouldMatch` handle1
        ackEvent ws e

      -- We've consumed the whole queue.
      assertNoEvent_ ws

testTransientEventsDoNotTriggerDeadLetters :: (HasCallStack) => App ()
testTransientEventsDoNotTriggerDeadLetters = do
  let notifTTL = 1 # Second
  withModifiedBackend (def {gundeckCfg = setField "settings.notificationTTL" (notifTTL #> Second)}) $ \domain -> do
    alice <- randomUser domain def
    -- Creates a non-transient event
    client <- addClient alice def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
    clientId <- objId client

    -- consume it
    runCodensity (createEventsWebSocket alice clientId) $ \ws -> do
      assertEvent ws $ \e -> do
        e %. "data.event.payload.0.type" `shouldMatch` "user.client-add"
        e %. "type" `shouldMatch` "event"
        e %. "data.event.payload.0.type" `shouldMatch` "user.client-add"
        e %. "data.event.payload.0.client.id" `shouldMatch` clientId
        deliveryTag <- e %. "data.delivery_tag"
        sendAck ws deliveryTag False

    -- Self conv ID is same as user's ID, we'll use this to send typing
    -- indicators, so we don't have to create another conv.
    selfConvId <- objQidObject alice
    -- Typing status is transient, currently no one is listening.
    sendTypingStatus alice selfConvId "started" >>= assertSuccess

    runCodensity (createEventsWebSocket alice clientId) $ \ws -> do
      assertNoEvent_ ws

testTransientEvents :: (HasCallStack) => App ()
testTransientEvents = do
  alice <- randomUser OwnDomain def
  client <- addClient alice def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
  clientId <- objId client

  -- Self conv ID is same as user's ID, we'll use this to send typing
  -- indicators, so we don't have to create another conv.
  selfConvId <- objQidObject alice

  runCodensity (createEventsWebSocket alice clientId) $ \ws -> do
    consumeAllEvents ws
    sendTypingStatus alice selfConvId "started" >>= assertSuccess
    assertEvent ws $ \e -> do
      e %. "data.event.payload.0.type" `shouldMatch` "conversation.typing"
      e %. "data.event.payload.0.qualified_conversation" `shouldMatch` selfConvId
      deliveryTag <- e %. "data.delivery_tag"
      sendAck ws deliveryTag False

  handle1 <- randomHandle
  putHandle alice handle1 >>= assertSuccess

  sendTypingStatus alice selfConvId "stopped" >>= assertSuccess

  handle2 <- randomHandle
  putHandle alice handle2 >>= assertSuccess

  -- We shouldn't see the stopped typing status because we were not connected to
  -- the websocket when it was sent. The other events should still show up in
  -- order.
  runCodensity (createEventsWebSocket alice clientId) $ \ws -> do
    for_ [handle1, handle2] $ \handle ->
      assertEvent ws $ \e -> do
        e %. "data.event.payload.0.type" `shouldMatch` "user.update"
        e %. "data.event.payload.0.user.handle" `shouldMatch` handle
        ackEvent ws e

    assertNoEvent_ ws

testChannelLimit :: (HasCallStack) => App ()
testChannelLimit = withModifiedBackend
  ( def
      { cannonCfg =
          setField "rabbitMqMaxChannels" (2 :: Int)
            >=> setField "rabbitMqMaxConnections" (1 :: Int)
      }
  )
  $ \domain -> do
    alice <- randomUser domain def
    (client0 : clients) <-
      replicateM 3
        $ addClient alice def {acapabilities = Just ["consumable-notifications"]}
        >>= getJSON 201
        >>= (%. "id")
        >>= asString

    lowerCodensity $ do
      for_ clients $ \c -> do
        ws <- createEventsWebSocket alice c
        lift $ assertEvent ws $ \e -> do
          e %. "data.event.payload.0.type" `shouldMatch` "user.client-add"
          e %. "data.event.payload.0.client.id" `shouldMatch` c

      -- the first client fails to connect because the server runs out of channels
      do
        ws <- createEventsWebSocket alice client0
        lift $ assertNoEvent_ ws

testChannelKilled :: (HasCallStack) => App ()
testChannelKilled = lowerCodensity $ do
  pool <- lift $ asks (.resourcePool)
  [backend] <- acquireResources 1 pool
  domain <- startDynamicBackend backend mempty
  alice <- lift $ randomUser domain def
  [c1, c2] <-
    lift
      $ replicateM 2
      $ addClient alice def {acapabilities = Just ["consumable-notifications"]}
      >>= getJSON 201
      >>= (%. "id")
      >>= asString

  ws <- createEventsWebSocket alice c1
  lift $ do
    assertEvent ws $ \e -> do
      e %. "data.event.payload.0.type" `shouldMatch` "user.client-add"
      e %. "data.event.payload.0.client.id" `shouldMatch` c1
      ackEvent ws e

    assertEvent ws $ \e -> do
      e %. "data.event.payload.0.type" `shouldMatch` "user.client-add"
      e %. "data.event.payload.0.client.id" `shouldMatch` c2

      recoverAll
        (constantDelay 500_000 <> limitRetries 10)
        (const (killConnection backend))

    noEvent <- assertNoEvent ws
    noEvent `shouldMatch` WebSocketDied

----------------------------------------------------------------------
-- helpers

data EventWebSocket = EventWebSocket
  { events :: Chan (Either WS.ConnectionException Value),
    ack :: MVar (Maybe Value)
  }

createEventsWebSocket ::
  (HasCallStack, MakesValue uid) =>
  uid ->
  String ->
  Codensity App EventWebSocket
createEventsWebSocket user cid = do
  eventsChan <- liftIO newChan
  ackChan <- liftIO newEmptyMVar
  serviceMap <- lift $ getServiceMap =<< objDomain user
  apiVersion <- lift $ getAPIVersionFor $ objDomain user
  let minAPIVersion = 8
  lift
    . when (apiVersion < minAPIVersion)
    $ assertFailure ("Events websocket can only be created when APIVersion is at least " <> show minAPIVersion)

  uid <- lift $ objId =<< objQidObject user
  let HostPort caHost caPort = serviceHostPort serviceMap Cannon
      path = "/v" <> show apiVersion <> "/events?client=" <> cid
      caHdrs = [(fromString "Z-User", toByteString' uid)]
      app conn =
        race_
          (wsRead conn `catch` (writeChan eventsChan . Left))
          (wsWrite conn)

      wsRead conn = forever $ do
        bs <- WS.receiveData conn
        case decodeStrict' bs of
          Just n -> writeChan eventsChan (Right n)
          Nothing ->
            error $ "Failed to decode events: " ++ show bs

      wsWrite conn = do
        mAck <- takeMVar ackChan
        case mAck of
          Nothing -> WS.sendClose conn (Text.pack "")
          Just ack ->
            WS.sendBinaryData conn (encode ack)
              >> wsWrite conn

  wsThread <- Codensity $ \k -> do
    withAsync
      ( liftIO
          $ WS.runClientWith
            caHost
            (fromIntegral caPort)
            path
            WS.defaultConnectionOptions
            caHdrs
            app
      )
      k

  Codensity $ \k ->
    k (EventWebSocket eventsChan ackChan) `finally` do
      putMVar ackChan Nothing
      liftIO $ wait wsThread

ackFullSync :: (HasCallStack) => EventWebSocket -> App ()
ackFullSync ws =
  putMVar ws.ack
    $ Just (object ["type" .= "ack_full_sync"])

ackEvent :: (HasCallStack) => EventWebSocket -> Value -> App ()
ackEvent ws event = do
  deliveryTag <- event %. "data.delivery_tag"
  sendAck ws deliveryTag False

sendAck :: (HasCallStack) => EventWebSocket -> Value -> Bool -> App ()
sendAck ws deliveryTag multiple =
  do
    putMVar $ ws.ack
    $ Just
    $ object
      [ "type" .= "ack",
        "data"
          .= object
            [ "delivery_tag" .= deliveryTag,
              "multiple" .= multiple
            ]
      ]

assertEvent :: (HasCallStack) => EventWebSocket -> ((HasCallStack) => Value -> App a) -> App a
assertEvent ws expectations = do
  timeout 10_000_000 (readChan ws.events) >>= \case
    Nothing -> assertFailure "No event received for 1s"
    Just (Left _) -> assertFailure "Websocket closed when waiting for more events"
    Just (Right e) -> do
      pretty <- prettyJSON e
      addFailureContext ("event:\n" <> pretty)
        $ expectations e

data NoEvent = NoEvent | WebSocketDied

instance ToJSON NoEvent where
  toJSON NoEvent = toJSON "no-event"
  toJSON WebSocketDied = toJSON "web-socket-died"

assertNoEvent :: (HasCallStack) => EventWebSocket -> App NoEvent
assertNoEvent ws = do
  timeout 1_000_000 (readChan ws.events) >>= \case
    Nothing -> pure NoEvent
    Just (Left _) -> pure WebSocketDied
    Just (Right e) -> do
      eventJSON <- prettyJSON e
      assertFailure $ "Did not expect event: \n" <> eventJSON

assertNoEvent_ :: (HasCallStack) => EventWebSocket -> App ()
assertNoEvent_ = void . assertNoEvent

consumeAllEvents :: EventWebSocket -> App ()
consumeAllEvents ws = do
  timeout 1_000_000 (readChan ws.events) >>= \case
    Nothing -> pure ()
    Just (Left e) ->
      assertFailure
        $ "Websocket closed while consuming all events: "
        <> displayException e
    Just (Right e) -> do
      ackEvent ws e
      consumeAllEvents ws

killConnection :: (HasCallStack) => BackendResource -> App ()
killConnection backend = do
  rc <- asks (.rabbitMQConfig)
  let opts =
        RabbitMqAdminOpts
          { host = rc.host,
            port = 0,
            adminPort = fromIntegral rc.adminPort,
            vHost = Text.pack backend.berVHost,
            tls = Just $ RabbitMqTlsOpts Nothing True
          }
  servantClient <- liftIO $ mkRabbitMqAdminClientEnv opts
  name <- do
    connections <- liftIO $ listConnectionsByVHost servantClient opts.vHost
    connection <-
      assertOne
        [ c | c <- connections, c.userProvidedName == Just (Text.pack "pool 0")
        ]
    pure connection.name

  void $ liftIO $ deleteConnection servantClient name
