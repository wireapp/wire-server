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
import Data.Proxy (Proxy (..))
import qualified Data.Text as Text
import Data.Timeout
import MLS.Util
import Network.AMQP.Extended
import Network.RabbitMqAdmin
import qualified Network.WebSockets as WS
import Notifications
import Servant.API (AsApi, ToServant, toServant)
import Servant.API.Generic (fromServant)
import Servant.Client (AsClientT)
import qualified Servant.Client as Servant
import SetupHelpers
import Testlib.Prelude
import Testlib.ResourcePool
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

  runCodensity (createEventsWebSocket alice (Just clientId)) $ \ws -> do
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

testConsumeTempEvents :: (HasCallStack) => App ()
testConsumeTempEvents = do
  alice <- randomUser OwnDomain def

  client0 <- addClient alice def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
  clientId0 <- objId client0

  lowerCodensity $ do
    ws0 <- createEventsWebSocket alice (Just clientId0)

    -- Ensure there is no race between event for this client being pushed and temp
    -- consumer being created
    lift $ do
      expectAndAckNewClientEvent ws0 clientId0
      assertNoEvent_ ws0

    wsTemp <- createEventsWebSocket alice Nothing

    lift $ do
      client1 <- addClient alice def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
      clientId1 <- objId client1

      -- Temp client gets this event as it happens after temp client has started
      -- listening
      void $ expectAndAckNewClientEvent wsTemp clientId1

      -- Client0 should also be notified even if there is a temp client
      void $ expectAndAckNewClientEvent ws0 clientId1

      assertNoEvent_ wsTemp
      assertNoEvent_ ws0
  where
    expectAndAckNewClientEvent :: EventWebSocket -> String -> App ()
    expectAndAckNewClientEvent ws cid =
      assertEvent ws $ \e -> do
        e %. "type" `shouldMatch` "event"
        e %. "data.event.payload.0.type" `shouldMatch` "user.client-add"
        e %. "data.event.payload.0.client.id" `shouldMatch` cid

        ackEvent ws e

testConsumeTempEventsWithoutOwnClient :: (HasCallStack) => App ()
testConsumeTempEventsWithoutOwnClient = do
  [alice, bob] <- createAndConnectUsers [OwnDomain, OwnDomain]

  runCodensity (createEventsWebSocket alice Nothing) $ \ws -> do
    handle <- randomHandle
    putHandle bob handle >>= assertSuccess

    -- We cannot use 'assertEvent' here because there is a race between the temp
    -- queue being created and rabbitmq fanning out the previous events.
    void $ assertFindsEvent ws $ \e -> do
      e %. "type" `shouldMatch` "event"
      e %. "data.event.payload.0.type" `shouldMatch` "user.update"
      e %. "data.event.payload.0.user.id" `shouldMatch` objId bob
      e %. "data.event.payload.0.user.handle" `shouldMatch` handle

      ackEvent ws e

testTemporaryQueuesAreDeletedAfterUse :: (HasCallStack) => App ()
testTemporaryQueuesAreDeletedAfterUse = do
  startDynamicBackendsReturnResources [def] $ \[beResource] -> do
    let domain = beResource.berDomain
    rabbitmqAdmin <- mkRabbitMqAdminClientForResource beResource
    queuesBeforeWS <- rabbitmqAdmin.listQueuesByVHost (fromString beResource.berVHost) (fromString "") True 100 1
    let deadNotifsQueue = Queue {name = fromString "dead-user-notifications", vhost = fromString beResource.berVHost}
    queuesBeforeWS.items `shouldMatch` [deadNotifsQueue]

    [alice, bob] <- createAndConnectUsers [domain, domain]

    runCodensity (createEventsWebSocket alice Nothing) $ \ws -> do
      handle <- randomHandle
      putHandle bob handle >>= assertSuccess

      queuesDuringWS <- rabbitmqAdmin.listQueuesByVHost (fromString beResource.berVHost) (fromString "") True 100 1
      addJSONToFailureContext "queuesDuringWS" queuesDuringWS $ do
        length queuesDuringWS.items `shouldMatchInt` 2

      -- We cannot use 'assertEvent' here because there is a race between the temp
      -- queue being created and rabbitmq fanning out the previous events.
      void $ assertFindsEvent ws $ \e -> do
        e %. "type" `shouldMatch` "event"
        e %. "data.event.payload.0.type" `shouldMatch` "user.update"
        e %. "data.event.payload.0.user.id" `shouldMatch` objId bob
        e %. "data.event.payload.0.user.handle" `shouldMatch` handle

        ackEvent ws e

    -- Use let binding here so 'shouldMatchEventually' retries the whole request
    let queuesAfterWSM = rabbitmqAdmin.listQueuesByVHost (fromString beResource.berVHost) (fromString "") True 100 1
    fmap (.items) queuesAfterWSM `shouldEventuallyMatch` ([deadNotifsQueue])

testMLSTempEvents :: (HasCallStack) => App ()
testMLSTempEvents = do
  [alice, bob] <- createAndConnectUsers [OwnDomain, OwnDomain]
  clients@[alice1, _, _] <-
    traverse
      ( createMLSClient
          def
          def
            { clientArgs =
                def
                  { acapabilities = Just ["consumable-notifications"]
                  }
            }
      )
      [alice, bob, bob]

  traverse_ (uploadNewKeyPackage def) clients
  convId <- createNewGroup def alice1

  runCodensity (createEventsWebSocket bob Nothing) $ \ws -> do
    commit <- createAddCommit alice1 convId [bob]
    void $ postMLSCommitBundle commit.sender (mkBundle commit) >>= getJSON 201

    -- FUTUREWORK: we should not rely on events arriving in this particular order

    -- We cannot use 'assertEvent' here because there is a race between the temp
    -- queue being created and rabbitmq fanning out the previous events.
    void $ assertFindsEvent ws $ \e -> do
      e %. "type" `shouldMatch` "event"
      e %. "data.event.payload.0.type" `shouldMatch` "conversation.member-join"
      user <- assertOne =<< (e %. "data.event.payload.0.data.users" & asList)
      user %. "qualified_id" `shouldMatch` (bob %. "qualified_id")
      ackEvent ws e

    void $ assertEvent ws $ \e -> do
      e %. "type" `shouldMatch` "event"
      e %. "data.event.payload.0.type" `shouldMatch` "conversation.mls-welcome"
      ackEvent ws e

    assertNoEvent_ ws

testConsumeEventsForDifferentUsers :: (HasCallStack) => App ()
testConsumeEventsForDifferentUsers = do
  alice <- randomUser OwnDomain def
  bob <- randomUser OwnDomain def

  aliceClient <- addClient alice def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
  aliceClientId <- objId aliceClient

  bobClient <- addClient bob def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
  bobClientId <- objId bobClient

  lowerCodensity $ do
    aliceWS <- createEventsWebSocket alice (Just aliceClientId)
    bobWS <- createEventsWebSocket bob (Just bobClientId)
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

    runCodensity (createEventsWebSocket alice (Just newClientId)) $ \ws ->
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

  runCodensity (createEventsWebSocket alice (Just clientId)) $ \ws -> do
    assertEvent ws $ \e -> do
      e %. "data.event.payload.0.type" `shouldMatch` "user.client-add"
      e %. "data.event.payload.0.client.id" `shouldMatch` clientId

  -- without ack, we receive the same event again
  runCodensity (createEventsWebSocket alice (Just clientId)) $ \ws -> do
    deliveryTag <- assertEvent ws $ \e -> do
      e %. "data.event.payload.0.type" `shouldMatch` "user.client-add"
      e %. "data.event.payload.0.client.id" `shouldMatch` clientId
      e %. "data.delivery_tag"
    sendAck ws deliveryTag False

  runCodensity (createEventsWebSocket alice (Just clientId)) $ \ws -> do
    assertNoEvent_ ws

testConsumeEventsMultipleAcks :: (HasCallStack) => App ()
testConsumeEventsMultipleAcks = do
  alice <- randomUser OwnDomain def
  client <- addClient alice def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
  clientId <- objId client

  handle <- randomHandle
  putHandle alice handle >>= assertSuccess

  runCodensity (createEventsWebSocket alice (Just clientId)) $ \ws -> do
    assertEvent ws $ \e -> do
      e %. "data.event.payload.0.type" `shouldMatch` "user.client-add"
      e %. "data.event.payload.0.client.id" `shouldMatch` clientId

    deliveryTag <- assertEvent ws $ \e -> do
      e %. "data.event.payload.0.type" `shouldMatch` "user.update"
      e %. "data.event.payload.0.user.handle" `shouldMatch` handle
      e %. "data.delivery_tag"

    sendAck ws deliveryTag True

  runCodensity (createEventsWebSocket alice (Just clientId)) $ \ws -> do
    assertNoEvent_ ws

testConsumeEventsAckNewEventWithoutAckingOldOne :: (HasCallStack) => App ()
testConsumeEventsAckNewEventWithoutAckingOldOne = do
  alice <- randomUser OwnDomain def
  client <- addClient alice def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
  clientId <- objId client

  handle <- randomHandle
  putHandle alice handle >>= assertSuccess

  runCodensity (createEventsWebSocket alice (Just clientId)) $ \ws -> do
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
  runCodensity (createEventsWebSocket alice (Just clientId)) $ \ws -> do
    deliveryTagClientAdd <- assertEvent ws $ \e -> do
      e %. "data.event.payload.0.type" `shouldMatch` "user.client-add"
      e %. "data.event.payload.0.client.id" `shouldMatch` clientId
      e %. "data.delivery_tag"

    sendAck ws deliveryTagClientAdd False

  runCodensity (createEventsWebSocket alice (Just clientId)) $ \ws -> do
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

    runCodensity (createEventsWebSocket alice (Just clientId)) $ \ws -> do
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
    runCodensity (createEventsWebSocket alice (Just clientId)) $ \ws -> do
      assertFindsEvent ws $ \e -> do
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

    runCodensity (createEventsWebSocket alice (Just clientId)) $ \ws -> do
      assertNoEvent_ ws

testTransientEvents :: (HasCallStack) => App ()
testTransientEvents = do
  alice <- randomUser OwnDomain def
  client <- addClient alice def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
  clientId <- objId client

  -- Self conv ID is same as user's ID, we'll use this to send typing
  -- indicators, so we don't have to create another conv.
  selfConvId <- objQidObject alice

  runCodensity (createEventsWebSocket alice (Just clientId)) $ \ws -> do
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
  runCodensity (createEventsWebSocket alice (Just clientId)) $ \ws -> do
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
        ws <- createEventsWebSocket alice (Just c)
        lift $ assertEvent ws $ \e -> do
          e %. "data.event.payload.0.type" `shouldMatch` "user.client-add"
          e %. "data.event.payload.0.client.id" `shouldMatch` c

      -- the first client fails to connect because the server runs out of channels
      do
        eithWS <- createEventsWebSocketEither alice (Just client0)
        case eithWS of
          Left (WS.MalformedResponse respHead _) ->
            lift $ respHead.responseCode `shouldMatchInt` 503
          Left e ->
            lift $ assertFailure $ "Expected websocket to fail with response code 503, got some other handshake exception: " <> displayException e
          Right _ -> lift $ assertFailure "Expected websocket hanshake to fail, but it didn't"

testChannelKilled :: (HasCallStack) => App ()
testChannelKilled = do
  pool <- asks (.resourcePool)
  runCodensity (acquireResources 1 pool) $ \[backend] -> do
    -- Some times RabbitMQ still remembers connections from previous uses of the
    -- dynamic backend. So we wait to ensure that we kill connection only for our
    -- current.
    void $ killAllRabbitMqConns backend
    waitUntilNoRabbitMqConns backend

    runCodensity (startDynamicBackend backend def) $ \_ -> do
      let domain = backend.berDomain
      alice <- randomUser domain def
      [c1, c2] <-
        replicateM 2
          $ addClient alice def {acapabilities = Just ["consumable-notifications"]}
          >>= getJSON 201
          >>= (%. "id")
          >>= asString

      runCodensity (createEventsWebSocket alice (Just c1)) $ \ws -> do
        -- If creating the user takes longer (async) than adding the clients, we get a
        -- `"user.activate"` here, so we use `assertFindsEvent`.
        assertFindsEvent ws $ \e -> do
          e %. "data.event.payload.0.type" `shouldMatch` "user.client-add"
          e %. "data.event.payload.0.client.id" `shouldMatch` c1
          ackEvent ws e

        assertEvent ws $ \e -> do
          e %. "data.event.payload.0.type" `shouldMatch` "user.client-add"
          e %. "data.event.payload.0.client.id" `shouldMatch` c2
          ackEvent ws e

        -- The RabbitMQ admin API takes some time to see new connections, so we need
        -- to try a few times.
        recoverAll (constantDelay 500_000 <> limitRetries 10) $ \_ -> do
          conns <- killAllRabbitMqConns backend
          assertAtLeastOne conns

        waitUntilNoRabbitMqConns backend

        assertNoEventHelper ws `shouldMatch` WebSocketDied

testSingleConsumer :: (HasCallStack) => App ()
testSingleConsumer = do
  alice <- randomUser OwnDomain def
  clientId <-
    addClient alice def {acapabilities = Just ["consumable-notifications"]}
      >>= getJSON 201
      >>= objId

  -- add a second client in order to generate one more notification
  clientId' <- addClient alice def >>= getJSON 201 >>= objId

  lowerCodensity $ do
    ws <- createEventsWebSocket alice (Just clientId)
    ws' <- createEventsWebSocket alice (Just clientId)

    -- the second websocket should get no notifications as long as the first
    -- one is connected
    lift $ assertNoEvent_ ws'

    deliveryTag1 <- lift $ assertEvent ws $ \e -> do
      e %. "type" `shouldMatch` "event"
      e %. "data.event.payload.0.type" `shouldMatch` "user.client-add"
      e %. "data.event.payload.0.client.id" `shouldMatch` clientId
      e %. "data.delivery_tag"

    lift $ assertNoEvent_ ws'

    lift $ sendAck ws deliveryTag1 False
    lift $ assertNoEvent_ ws'

    deliveryTag2 <- lift $ assertEvent ws $ \e -> do
      e %. "type" `shouldMatch` "event"
      e %. "data.event.payload.0.type" `shouldMatch` "user.client-add"
      e %. "data.event.payload.0.client.id" `shouldMatch` clientId'
      e %. "data.delivery_tag"
    lift $ sendAck ws deliveryTag2 False

    lift $ assertNoEvent_ ws'

----------------------------------------------------------------------
-- helpers

data EventWebSocket = EventWebSocket
  { events :: Chan (Either WS.ConnectionException Value),
    ack :: MVar (Maybe Value)
  }

createEventsWebSocket ::
  (HasCallStack, MakesValue uid) =>
  uid ->
  Maybe String ->
  Codensity App EventWebSocket
createEventsWebSocket user cid = do
  eithWS <- createEventsWebSocketEither user cid
  case eithWS of
    Left e -> lift $ assertFailure $ "Websocket failed to connect due to handshake exception: " <> displayException e
    Right ws -> pure ws

createEventsWebSocketEither ::
  (HasCallStack, MakesValue uid) =>
  uid ->
  Maybe String ->
  Codensity App (Either WS.HandshakeException EventWebSocket)
createEventsWebSocketEither user cid = do
  eventsChan <- liftIO newChan
  ackChan <- liftIO newEmptyMVar
  serviceMap <- lift $ getServiceMap =<< objDomain user
  apiVersion <- lift $ getAPIVersionFor $ objDomain user
  wsStarted <- newEmptyMVar
  let minAPIVersion = 8
  lift
    . when (apiVersion < minAPIVersion)
    $ assertFailure ("Events websocket can only be created when APIVersion is at least " <> show minAPIVersion)

  uid <- lift $ objId =<< objQidObject user
  let HostPort caHost caPort = serviceHostPort serviceMap Cannon
      path = "/v" <> show apiVersion <> "/events" <> maybe "" ("?client=" <>) cid
      caHdrs = [(fromString "Z-User", toByteString' uid)]
      app conn = do
        putMVar wsStarted (Right ())
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

  wsThread <-
    Codensity
      $ withAsync
      $ liftIO
      $ WS.runClientWith caHost (fromIntegral caPort) path WS.defaultConnectionOptions caHdrs app
      `catch` \(e :: WS.HandshakeException) -> putMVar wsStarted (Left e)

  timeOutSeconds <- asks (.timeOutSeconds)
  mStarted <- lift $ timeout (timeOutSeconds * 1_000_000) (takeMVar wsStarted)
  case mStarted of
    Nothing -> do
      cancel wsThread
      lift $ assertFailure $ "Websocket failed to connect within " <> show timeOutSeconds <> "s"
    Just (Left e) ->
      pure (Left e)
    Just (Right ()) ->
      Codensity $ \k ->
        k (Right $ EventWebSocket eventsChan ackChan) `finally` do
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
  timeOutSeconds <- asks (.timeOutSeconds)
  timeout (timeOutSeconds * 1_000_000) (readChan ws.events) >>= \case
    Nothing -> assertFailure $ "No event received for " <> show timeOutSeconds <> "s"
    Just (Left ex) ->
      addFailureContext ("WSException: " <> displayException ex)
        $ assertFailure "Websocket closed when waiting for more events"
    Just (Right e) -> do
      pretty <- prettyJSON e
      addFailureContext ("event:\n" <> pretty)
        $ expectations e

-- | Tolerates and consumes other events before expected event
assertFindsEvent :: forall a. (HasCallStack) => EventWebSocket -> ((HasCallStack) => Value -> App a) -> App a
assertFindsEvent ws expectations = go 0
  where
    go :: Int -> App a
    go ignoredEventCount = do
      timeOutSeconds <- asks (.timeOutSeconds)
      timeout (timeOutSeconds * 1_000_000) (readChan ws.events) >>= \case
        Nothing -> assertFailure $ show ignoredEventCount <> " event(s) received, no matching event received for " <> show timeOutSeconds <> "s"
        Just (Left ex) ->
          addFailureContext ("WSException: " <> displayException ex)
            $ assertFailure "Websocket closed when waiting for more events"
        Just (Right ev) -> do
          (expectations ev)
            `catch` \(_ :: AssertionFailure) -> do
              ignoredEventType <-
                maybe (pure "No Type") asString
                  =<< lookupField ev "data.event.payload.0.type"
              ackEvent ws ev
              addJSONToFailureContext ("Ignored Event (" <> ignoredEventType <> ")") ev
                $ go (ignoredEventCount + 1)

data NoEvent = NoEvent | WebSocketDied

instance ToJSON NoEvent where
  toJSON NoEvent = toJSON "no-event"
  toJSON WebSocketDied = toJSON "web-socket-died"

assertNoEventHelper :: (HasCallStack) => EventWebSocket -> App NoEvent
assertNoEventHelper ws = do
  timeOutSeconds <- asks (.timeOutSeconds)
  timeout (timeOutSeconds * 1_000_000) (readChan ws.events) >>= \case
    Nothing -> pure NoEvent
    Just (Left _) -> pure WebSocketDied
    Just (Right e) -> do
      eventJSON <- prettyJSON e
      assertFailure $ "Did not expect event: \n" <> eventJSON

-- | Similar to `assertNoEvent` from Testlib, but with rabbitMQ typing (`/event` end-point, not
-- `/await`).
assertNoEvent_ :: (HasCallStack) => EventWebSocket -> App ()
assertNoEvent_ = void . assertNoEventHelper

assertWebSocketDied :: (HasCallStack) => EventWebSocket -> App ()
assertWebSocketDied ws = do
  recpol <- do
    timeOutSeconds <- asks (.timeOutSeconds)
    pure $ limitRetriesByCumulativeDelay (timeOutSeconds * 1_000_000) (constantDelay 800_000)
  recoverAll recpol $ \_ ->
    assertNoEventHelper ws >>= \case
      NoEvent -> assertFailure $ "WebSocket is still open"
      WebSocketDied -> pure ()

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

-- | Only considers connections from cannon
waitUntilNoRabbitMqConns :: (HasCallStack) => BackendResource -> App ()
waitUntilNoRabbitMqConns backend = do
  rabbitmqAdminClient <- mkRabbitMqAdminClientForResource backend
  recoverAll
    (constantDelay 500_000 <> limitRetries 10)
    (const (go rabbitmqAdminClient))
  where
    go rabbitmqAdminClient = do
      cannonConnections <- getCannonConnections rabbitmqAdminClient backend.berVHost
      cannonConnections `shouldMatch` ([] :: [Connection])

-- | Only kills connections from cannon and returns them
killAllRabbitMqConns :: (HasCallStack) => BackendResource -> App [Connection]
killAllRabbitMqConns backend = do
  rabbitmqAdminClient <- mkRabbitMqAdminClientForResource backend
  cannonConnections <- getCannonConnections rabbitmqAdminClient backend.berVHost
  for_ cannonConnections $ \connection ->
    rabbitmqAdminClient.deleteConnection connection.name
  pure cannonConnections

getCannonConnections :: AdminAPI (AsClientT App) -> String -> App [Connection]
getCannonConnections rabbitmqAdminClient vhost = do
  connections <- rabbitmqAdminClient.listConnectionsByVHost (Text.pack vhost)
  pure $ filter (\c -> maybe False (fromString "pool " `Text.isPrefixOf`) c.userProvidedName) connections

mkRabbitMqAdminClientForResource :: BackendResource -> App (AdminAPI (Servant.AsClientT App))
mkRabbitMqAdminClientForResource backend = do
  rc <- asks (.rabbitMQConfig)
  let opts =
        RabbitMqAdminOpts
          { host = rc.host,
            port = 0,
            adminPort = fromIntegral rc.adminPort,
            vHost = Text.pack backend.berVHost,
            tls =
              if rc.tls
                then Just $ RabbitMqTlsOpts Nothing True
                else Nothing
          }
  servantClient <- liftIO $ mkRabbitMqAdminClientEnv opts
  pure . fromServant $ Servant.hoistClient (Proxy @(ToServant AdminAPI AsApi)) (liftIO @App) (toServant servantClient)
