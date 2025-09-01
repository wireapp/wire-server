{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Test.Events where

import API.Brig
import API.BrigCommon
import API.Common
import API.Galley
import API.Gundeck
import qualified API.GundeckInternal as GundeckInternal
import qualified Control.Concurrent.Timeout as Timeout
import Control.Lens ((.~), (^?!))
import Control.Monad.Codensity
import Control.Monad.RWS (asks)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Retry
import Data.ByteString.Conversion (toByteString')
import qualified Data.ProtoLens as Proto
import Data.ProtoLens.Labels ()
import Data.Proxy (Proxy (..))
import qualified Data.Text as Text
import Data.Timeout
import Network.AMQP.Extended
import Network.RabbitMqAdmin
import qualified Network.WebSockets as WS
import Notifications
import Numeric.Lens
import qualified Proto.Otr as Proto
import qualified Proto.Otr_Fields as Proto
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

  secondClientId <- addClient alice def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201 >>= objId

  runCodensity (createEventsWebSocket alice (Just clientId)) $ \ws -> do
    deliveryTag <- assertEvent ws $ \e -> do
      e %. "type" `shouldMatch` "event"
      e %. "data.event.payload.0.type" `shouldMatch` "user.client-add"
      e %. "data.event.payload.0.client.id" `shouldMatch` secondClientId
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

testWebSocketTimeout :: (HasCallStack) => App ()
testWebSocketTimeout = withModifiedBackend
  def
    { cannonCfg =
        setField "wsOpts.activityTimeout" (1000000 :: Int)
          >=> setField "wsOpts.pongTimeout" (1000000 :: Int)
    }
  $ \domain -> do
    alice <- randomUser domain def
    client <- addClient alice def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
    clientId <- objId client

    runCodensity (createEventsWebSocket alice (Just clientId)) $ \ws -> do
      deliveryTag <- assertEvent ws $ \e -> do
        e %. "type" `shouldMatch` "event"
        e %. "data.event.payload.0.type" `shouldMatch` "user.client-add"
        e %. "data.event.payload.0.client.id" `shouldMatch` clientId
        e %. "data.delivery_tag"
      sendAck ws deliveryTag False

      result <- timeout 2500000 (killWebSocketClient ws)
      when (isNothing result)
        $ assertFailure "Expected web socket timeout"

testConsumeTempEvents :: (HasCallStack) => App ()
testConsumeTempEvents = do
  alice <- randomUser OwnDomain def

  client0 <- addClient alice def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
  clientId0 <- objId client0

  lowerCodensity $ do
    ws0 <- createEventsWebSocket alice (Just clientId0)

    wsTemp <- createEventsWebSocket alice Nothing

    lift $ do
      client1 <- addClient alice def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
      clientId1 <- objId client1

      -- Temp client gets this event as it happens after temp client has started
      -- listening
      expectAndAckNewClientEvent wsTemp clientId1

      assertNoEvent_ wsTemp
      assertNoEvent_ ws0
  where
    expectAndAckNewClientEvent :: (HasCallStack) => EventWebSocket -> String -> App ()
    expectAndAckNewClientEvent ws cid =
      assertEvent ws $ \e -> do
        e %. "type" `shouldMatch` "event"
        e %. "data.event.payload.0.type" `shouldMatch` "user.client-add"
        e %. "data.event.payload.0.client.id" `shouldMatch` cid

        ackEvent ws e

testTemporaryQueuesAreDeletedAfterUse :: (HasCallStack) => App ()
testTemporaryQueuesAreDeletedAfterUse = do
  startDynamicBackendsReturnResources [def] $ \[beResource] -> do
    let domain = beResource.berDomain
    rabbitmqAdmin <- mkRabbitMqAdminClientForResource beResource
    [alice, bob] <- createAndConnectUsers [domain, domain]

    -- Create client for alice, so their temp websocket works
    aliceClient <- addClient alice def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
    aliceId <- asString $ alice %. "qualified_id.id"
    aliceClientId <- asString $ aliceClient %. "id"

    let aliceClientQueueName = "user-notifications." <> aliceId <> "." <> aliceClientId
        aliceClientQueue = Queue {name = fromString aliceClientQueueName, vhost = fromString beResource.berVHost}
        deadNotifsQueue = Queue {name = fromString "dead-user-notifications", vhost = fromString beResource.berVHost}
        cellsEventsQueue = Queue {name = fromString "cells_events", vhost = fromString beResource.berVHost}

    -- Wait for queue for the new client to be created
    eventually $ do
      queuesBeforeWS <- rabbitmqAdmin.listQueuesByVHost (fromString beResource.berVHost) (fromString "") True 100 1
      queuesBeforeWS.items `shouldMatchSet` [deadNotifsQueue, cellsEventsQueue, aliceClientQueue]

    runCodensity (createEventsWebSocket alice Nothing) $ \ws -> do
      handle <- randomHandle
      putHandle bob handle >>= assertSuccess

      queuesDuringWS <- rabbitmqAdmin.listQueuesByVHost (fromString beResource.berVHost) (fromString "") True 100 1
      addJSONToFailureContext "queuesDuringWS" queuesDuringWS $ do
        length queuesDuringWS.items `shouldMatchInt` 4

      -- We cannot use 'assertEvent' here because there is a race between the temp
      -- queue being created and rabbitmq fanning out the previous events.
      void $ assertFindsEvent ws $ \e -> do
        e %. "type" `shouldMatch` "event"
        e %. "data.event.payload.0.type" `shouldMatch` "user.update"
        e %. "data.event.payload.0.user.id" `shouldMatch` objId bob
        e %. "data.event.payload.0.user.handle" `shouldMatch` handle

        ackEvent ws e

    eventually $ do
      queuesAfterWS <- rabbitmqAdmin.listQueuesByVHost (fromString beResource.berVHost) (fromString "") True 100 1
      queuesAfterWS.items `shouldMatchSet` [deadNotifsQueue, cellsEventsQueue, aliceClientQueue]

testSendMessageNoReturnToSenderWithConsumableNotificationsProteus :: (HasCallStack) => App ()
testSendMessageNoReturnToSenderWithConsumableNotificationsProteus = do
  (alice, tid, bob : _) <- createTeam OwnDomain 2
  aliceOldClient <- addClient alice def >>= getJSON 201
  aliceClient <- addClient alice def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
  aliceClientId <- objId aliceClient
  bobClient <- addClient bob def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
  bobClientId <- objId bobClient
  conv <- postConversation alice defProteus {team = Just tid, qualifiedUsers = [bob]} >>= getJSON 201
  msg <- mkProteusRecipients alice [(bob, [bobClient]), (alice, [aliceOldClient])] "hello, bob"

  let protoMsg =
        Proto.defMessage @Proto.QualifiedNewOtrMessage
          & #sender . Proto.client .~ (aliceClientId ^?! hex)
          & #recipients .~ [msg]
          & #reportAll .~ Proto.defMessage
  postProteusMessage alice conv protoMsg >>= assertSuccess

  runCodensity (createEventsWebSocket bob (Just bobClientId)) $ \ws -> do
    assertFindsEvent ws $ \e -> do
      e %. "data.event.payload.0.type" `shouldMatch` "conversation.otr-message-add"
      e %. "data.event.payload.0.data.text" `shouldMatchBase64` "hello, bob"
      ackEvent ws e

  runCodensity (createEventsWebSocket alice (Just aliceClientId)) $ \ws -> do
    assertEvent ws $ \e -> do
      e %. "data.event.payload.0.type" `shouldMatch` "user.client-add"
      ackEvent ws e
    assertEvent ws $ \e -> do
      e %. "data.event.payload.0.type" `shouldMatch` "conversation.create"
      ackEvent ws e
    assertNoEvent_ ws

testEventsForSpecificClients :: (HasCallStack) => App ()
testEventsForSpecificClients = do
  alice <- randomUser OwnDomain def
  uid <- objId alice
  client1 <- addClient alice def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
  cid1 <- objId client1
  client2 <- addClient alice def >>= getJSON 201
  cid2 <- objId client2

  lowerCodensity $ do
    ws1 <- createEventsWebSocket alice (Just cid1)
    wsTemp <- createEventsWebSocket alice Nothing
    lift $ do
      void $ consumeAllEvents ws1

      let eventForClient1 =
            object
              [ "recipients" .= [object ["user_id" .= uid, "clients" .= [cid1], "route" .= "any"]],
                "payload" .= [object ["hello" .= "client1"]]
              ]
          eventForClient2 =
            object
              [ "recipients" .= [object ["user_id" .= uid, "clients" .= [cid2], "route" .= "any"]],
                "payload" .= [object ["hello" .= "client2"]]
              ]

      GundeckInternal.postPush OwnDomain [eventForClient1, eventForClient2] >>= assertSuccess
      assertEvent ws1 $ \e ->
        e %. "data.event.payload.0.hello" `shouldMatch` "client1"

      addFailureContext "client 1 should not get any events meant for client 2"
        $ assertNoEvent_ ws1

      addFailureContext "temp client should not get any events meant solely for client 1 or 2"
        $ assertNoEvent_ wsTemp

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
    secondClientIdAlice <- lift $ addClient alice def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201 >>= objId
    secondClientIdBob <- lift $ addClient bob def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201 >>= objId
    lift $ assertClientAdd secondClientIdAlice aliceWS
    lift $ assertClientAdd secondClientIdBob bobWS
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

    runCodensity (createEventsWebSocket alice (Just newClientId)) $ \ws -> do
      assertNoEvent_ ws

  -- All notifs are also in Cassandra because of the legacy client
  getNotifications alice def {since = Just lastNotifId} `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "notifications.0.payload.0.type" `shouldMatch` "user.client-add"

testConsumeEventsAcks :: (HasCallStack) => App ()
testConsumeEventsAcks = do
  alice <- randomUser OwnDomain def
  client <- addClient alice def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
  clientId <- objId client
  -- add another client to trigger `user.client-add` event
  secondClientId <- addClient alice def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201 >>= objId

  runCodensity (createEventsWebSocket alice (Just clientId)) $ \ws -> do
    assertEvent ws $ \e -> do
      e %. "data.event.payload.0.type" `shouldMatch` "user.client-add"
      e %. "data.event.payload.0.client.id" `shouldMatch` secondClientId

  -- without ack, we receive the same event again
  runCodensity (createEventsWebSocket alice (Just clientId)) $ \ws -> do
    deliveryTag <- assertEvent ws $ \e -> do
      e %. "data.event.payload.0.type" `shouldMatch` "user.client-add"
      e %. "data.event.payload.0.client.id" `shouldMatch` secondClientId
      e %. "data.delivery_tag"
    sendAck ws deliveryTag False

  runCodensity (createEventsWebSocket alice (Just clientId)) $ \ws -> do
    assertNoEvent_ ws

testConsumeEventsMultipleAcks :: (HasCallStack) => App ()
testConsumeEventsMultipleAcks = do
  alice <- randomUser OwnDomain def
  client <- addClient alice def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
  clientId <- objId client

  secondClientId <- addClient alice def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201 >>= objId

  handle <- randomHandle
  putHandle alice handle >>= assertSuccess

  runCodensity (createEventsWebSocket alice (Just clientId)) $ \ws -> do
    assertEvent ws $ \e -> do
      e %. "data.event.payload.0.type" `shouldMatch` "user.client-add"
      e %. "data.event.payload.0.client.id" `shouldMatch` secondClientId

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

  event <- mkEvent' alice client (object ["value" .= "foobar", "type" .= "test-no-ack"]) True
  GundeckInternal.postPush OwnDomain [event] >>= assertSuccess
  handle <- randomHandle
  putHandle alice handle >>= assertSuccess

  runCodensity (createEventsWebSocket alice (Just clientId)) $ \ws -> do
    assertEvent ws $ \e -> do
      e %. "data.event.payload.0.type" `shouldMatch` "test-no-ack"
      e %. "data.event.payload.0.value" `shouldMatch` "foobar"

    deliveryTagHandleAdd <- assertEvent ws $ \e -> do
      e %. "data.event.payload.0.type" `shouldMatch` "user.update"
      e %. "data.event.payload.0.user.handle" `shouldMatch` handle
      e %. "data.delivery_tag"

    -- Only ack the handle add delivery tag
    sendAck ws deliveryTagHandleAdd False

  -- Expect client-add event to be delivered again.
  runCodensity (createEventsWebSocket alice (Just clientId)) $ \ws -> do
    deliveryTagClientAdd <- assertEvent ws $ \e -> do
      e %. "data.event.payload.0.type" `shouldMatch` "test-no-ack"
      e %. "data.event.payload.0.value" `shouldMatch` "foobar"
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
        e %. "type" `shouldMatch` "notifications_missed"

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

testEventsDeadLetteredWithReconnect :: (HasCallStack) => App ()
testEventsDeadLetteredWithReconnect = do
  let notifTTL = 1 # Second
  startDynamicBackendsReturnResources [def {gundeckCfg = setField "settings.notificationTTL" (notifTTL #> Second)}] $ \[resources] -> do
    let domain :: String = resources.berDomain
    alice <- randomUser domain def

    -- This generates an event
    client <- addClient alice def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
    clientId <- objId client

    -- Force a reconnect by deleting the existing connection
    killAllDeadUserNotificationRabbitMqConns resources

    -- We expire the add client event by waiting it out
    Timeout.threadDelay (notifTTL + 500 # MilliSecond)

    -- Generate a second event
    handle1 <- randomHandle
    putHandle alice handle1 >>= assertSuccess

    runCodensity (createEventsWebSocketWithSync alice (Just clientId)) $ \(endMarker, ws) -> do
      assertEvent ws $ \e -> do
        e %. "type" `shouldMatch` "notifications_missed"

      -- Until we ack the full sync, we can't get new events
      ackFullSync ws

      -- Now we can see the next event
      assertEvent ws $ \e -> do
        e %. "data.event.payload.0.type" `shouldMatch` "user.update"
        e %. "data.event.payload.0.user.handle" `shouldMatch` handle1
        ackEvent ws e

      -- We've consumed the whole queue.
      assertEndOfIniitalSync ws endMarker
  where
    killAllDeadUserNotificationRabbitMqConns :: (HasCallStack) => BackendResource -> App ()
    killAllDeadUserNotificationRabbitMqConns backend = do
      rabbitmqAdminClient <- mkRabbitMqAdminClientForResource backend
      connections <- eventually $ do
        conns <- getDeadUserNotificationConnections rabbitmqAdminClient backend.berVHost
        assertAtLeastOne conns
        pure conns
      for_ connections $ \connection -> do
        rabbitmqAdminClient.deleteConnection connection.name

    getDeadUserNotificationConnections :: (HasCallStack) => AdminAPI (AsClientT App) -> String -> App [Connection]
    getDeadUserNotificationConnections rabbitmqAdminClient vhost = do
      connections <- rabbitmqAdminClient.listConnectionsByVHost (Text.pack vhost)
      pure $ filter (\c -> Just (fromString "dead-user-notifications-watcher") == c.userProvidedName) connections

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
  (alice, _, _) <- mkUserPlusClient
  (bob, _, bobClient) <- mkUserPlusClient
  connectTwoUsers alice bob
  bobClientId <- objId bobClient

  conv <- postConversation alice defProteus {qualifiedUsers = [bob]} >>= getJSON 201

  runCodensity (createEventsWebSocketWithSync bob (Just bobClientId)) $ \(marker, bobWs) -> do
    void $ consumeEventsUntilEndOfInitialSync bobWs marker

    sendTypingStatus alice conv "started" >>= assertSuccess

    assertEvent bobWs $ \e -> do
      e %. "data.event.payload.0.type" `shouldMatch` "conversation.typing"
      e %. "data.event.payload.0.qualified_conversation" `shouldMatch` (conv %. "qualified_id")
      ackEvent bobWs e

  handle1 <- randomHandle
  putHandle bob handle1 >>= assertSuccess

  sendTypingStatus alice conv "stopped" >>= assertSuccess

  handle2 <- randomHandle
  putHandle bob handle2 >>= assertSuccess

  -- We shouldn't see the stopped typing status because we were not connected to
  -- the websocket when it was sent. The other events should still show up in
  -- order.
  runCodensity (createEventsWebSocket bob (Just bobClient)) $ \ws -> do
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
      -- we just connect, no need to assert on any events
      for_ clients $ \c -> createEventsWebSocket alice (Just c)

      -- the first client fails to connect because the server runs out of channels
      do
        eithWS <- createEventsWebSocketEither alice (Just client0) Nothing
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

testPrefetchCount :: (HasCallStack) => App ()
testPrefetchCount = do
  (alice, uid, cid) <- mkUserPlusClient
  emptyQueue alice cid

  for_ [1 :: Int .. 550] $ \i ->
    do
      let event =
            object
              [ "recipients" .= [object ["user_id" .= uid, "clients" .= [cid], "route" .= "any"]],
                "payload" .= [object ["no" .= show i]]
              ]
      GundeckInternal.postPush OwnDomain [event] >>= assertSuccess
  runCodensity (createEventsWebSocketWithSync alice (Just cid)) \(endMarker, ws) -> do
    es <- consumeAllEventsNoAck ws
    assertBool ("First 500 events expected, got " ++ show (length es)) $ length es == 500

    forM_ es (ackEvent ws)

    es' <- consumeEventsUntilEndOfInitialSync ws endMarker
    assertBool "Receive at least one outstanding event" $ not (null es')

testEndOfInitialSync :: (HasCallStack) => App ()
testEndOfInitialSync = do
  (alice, uid, cid) <- mkUserPlusClient
  let n = 20
  replicateM_ n $ do
    GundeckInternal.postPush OwnDomain [mkEvent uid cid False] >>= assertSuccess

  -- marker0 <- randomId
  runCodensity (createEventsWebSocketWithSync alice (Just cid)) \(endMarker, ws) -> do
    preExistingEvents <- consumeEventsUntilEndOfInitialSync ws endMarker
    otherEvents <- consumeAllEvents ws

    -- we expect one user.client-add event, n more events, and one sync event
    length (preExistingEvents <> otherEvents) `shouldMatchInt` (n + 2)

    -- more events should not be followed by the sync event
    GundeckInternal.postPush OwnDomain [mkEvent uid cid False] >>= assertSuccess
    assertEvent ws $ \e -> do
      e %. "data.event.payload.0.type" `shouldMatch` "test"
      ackEvent ws e
    assertNoEvent_ ws

  -- when we reconnect, there are no messages but we still get the
  -- synchronization notification.
  runCodensity (createEventsWebSocketWithSync alice (Just cid)) \(endMarker, ws) -> do
    preExistingEvents <- consumeEventsUntilEndOfInitialSync ws endMarker
    otherEvents <- consumeAllEvents ws
    let events = preExistingEvents <> otherEvents
    length events `shouldMatchInt` 1

    -- more events should not be followed by synchronization event
    GundeckInternal.postPush OwnDomain [mkEvent uid cid False] >>= assertSuccess
    assertEvent ws $ \e -> do
      e %. "data.event.payload.0.type" `shouldMatch` "test"
      ackEvent ws e
    assertNoEvent_ ws

testEndOfInitialSyncMoreEventsAfterSyncMessage :: (HasCallStack) => App ()
testEndOfInitialSyncMoreEventsAfterSyncMessage = do
  (alice, uid, cid) <- mkUserPlusClient
  let n = 20
  replicateM_ n $ do
    GundeckInternal.postPush OwnDomain [mkEvent uid cid False] >>= assertSuccess

  runCodensity (createEventsWebSocketWithSync alice (Just cid)) \(endMarker, ws) -> do
    -- it seems this is needed to reduce flakiness,
    -- when the messages below are pushed faster than the sync message is inserted
    Timeout.threadDelay (1 # Second)

    -- before consuming, we push n more events
    replicateM_ n $ do
      GundeckInternal.postPush OwnDomain [mkEvent uid cid False] >>= assertSuccess

    preExistingEvents <- consumeEventsUntilEndOfInitialSync ws endMarker
    otherEvents <- consumeAllEvents ws

    length (preExistingEvents <> otherEvents) `shouldMatchInt` (n + n + 2)
    addFailureContext ("We should have received " <> show n <> " more events after the sync event but got " <> show (length otherEvents))
      $ (length otherEvents >= n)
      `shouldMatch` True

testEndOfInitialSyncIgnoreExpired :: (HasCallStack) => App ()
testEndOfInitialSyncIgnoreExpired = do
  (alice, uid, cid) <- mkUserPlusClient
  let n = 20
  replicateM_ n $ do
    GundeckInternal.postPush OwnDomain [mkEvent uid cid False] >>= assertSuccess

  replicateM_ n $ do
    GundeckInternal.postPush OwnDomain [mkEvent uid cid True] >>= assertSuccess

  -- Wait for transient events to expire
  Timeout.threadDelay (1 # Second)

  runCodensity (createEventsWebSocketWithSync alice (Just cid)) $ \(endMarker, ws) -> do
    preExistingEvents <- consumeEventsUntilEndOfInitialSync ws endMarker
    otherEvents <- consumeAllEvents ws
    let events = preExistingEvents <> otherEvents
    length events `shouldMatchInt` (n + 2) -- +1 for the sync event, +1 for the client add event

testEndOfInitialSyncAckMultiple :: (HasCallStack) => App ()
testEndOfInitialSyncAckMultiple = do
  (alice, uid, cid) <- mkUserPlusClient
  let n = 20
  replicateM_ n $ do
    GundeckInternal.postPush OwnDomain [mkEvent uid cid False] >>= assertSuccess

  runCodensity (createEventsWebSocketWithSync alice (Just cid)) $ \(endMarker, ws) -> do
    void $ assertEvent ws pure
    e <- assertEvent ws pure
    dt <- e %. "data.delivery_tag"
    -- we ack the first 2 events with one ack
    sendAck ws dt True
    let expectedNumEvents = n - 2 + 2 -- +1 for the sync event, +1 for the client add event
    preExistingEvents <- consumeEventsUntilEndOfInitialSync ws endMarker
    otherEvents <- consumeAllEvents ws
    let events = preExistingEvents <> otherEvents
    length events `shouldMatchInt` expectedNumEvents

mkEvent :: (ToJSON a1, ToJSON a2) => a1 -> a2 -> Bool -> Value
mkEvent uid cid transient =
  object
    [ "recipients" .= [object ["user_id" .= uid, "clients" .= [cid], "route" .= "any"]],
      "payload" .= [object ["hello" .= "world", "type" .= "test"]],
      "transient" .= transient
    ]

mkEvent' :: (MakesValue uid, MakesValue cid) => uid -> cid -> Value -> Bool -> App Value
mkEvent' uid cid payload transient = do
  uid' <- objId uid
  cid' <- objId cid
  pure $ object
    [ "recipients" .= [object ["user_id" .= uid', "clients" .= [cid'], "route" .= "any"]],
      "payload" .= [payload],
      "transient" .= transient
    ]

testTypingIndicatorIsNotSentToOwnClient :: (HasCallStack) => TaggedBool "federated" -> App ()
testTypingIndicatorIsNotSentToOwnClient (TaggedBool federated) = do
  (alice, _, aliceClient) <- mkUserPlusClientWithDomain OwnDomain
  (bob, _, bobClient) <- mkUserPlusClientWithDomain (if federated then OtherDomain else OwnDomain)
  connectTwoUsers alice bob
  aliceClientId <- objId aliceClient
  bobClientId <- objId bobClient
  conv <- postConversation alice defProteus {qualifiedUsers = [bob]} >>= getJSON 201

  runCodensity (createEventWebSockets [(alice, Just aliceClientId), (bob, Just bobClientId)]) $ \[aliceWs, bobWs] -> do
    -- consume all events to ensure we start with a clean slate
    consumeAllEvents_ aliceWs
    consumeAllEvents_ bobWs

    -- Alice is typing
    sendTypingStatus alice conv "started" >>= assertSuccess

    -- Bob should receive the typing indicator for Alice
    assertEvent bobWs $ \e -> do
      e %. "data.event.payload.0.type" `shouldMatch` "conversation.typing"
      e %. "data.event.payload.0.qualified_conversation" `shouldMatch` (conv %. "qualified_id")
      e %. "data.event.payload.0.qualified_from" `shouldMatch` (alice %. "qualified_id")
      ackEvent bobWs e

    -- Alice should not receive the typing indicator for herself
    assertNoEvent_ aliceWs

    -- Bob is typing
    sendTypingStatus bob conv "started" >>= assertSuccess

    -- Alice should receive the typing indicator for Bob
    assertEvent aliceWs $ \e -> do
      e %. "data.event.payload.0.type" `shouldMatch` "conversation.typing"
      e %. "data.event.payload.0.qualified_conversation" `shouldMatch` (conv %. "qualified_id")
      e %. "data.event.payload.0.qualified_from" `shouldMatch` (bob %. "qualified_id")
      ackEvent aliceWs e

    -- Bob should not receive the typing indicator for himself
    assertNoEvent_ bobWs

-- We only delete queues to clean up federated integration tests. So, we
-- mostly want to ensure we don't get stuck there.
testBackendPusherRecoversFromQueueDeletion :: (HasCallStack) => App ()
testBackendPusherRecoversFromQueueDeletion = do
  bob <- randomUser OwnDomain def

  domain1 <- asks (.domain1)

  let remotesRefreshInterval = 10000 :: Int
  startDynamicBackendsReturnResources
    [ def
        { backgroundWorkerCfg =
            setField
              "backendNotificationPusher.remotesRefreshInterval"
              remotesRefreshInterval
        }
    ]
    $ \[beResource] -> do
      let domain = beResource.berDomain
      (alice, team, [alex, alison]) <- createTeam domain 3

      -- Create a federated conversion
      connectTwoUsers alice bob
      [alexId, alisonId, bobId] <-
        forM [alex, alison, bob] (%. "qualified_id")
      let nc = (defProteus {qualifiedUsers = [alexId, alisonId, bobId], team = Just team})
      void $ postConversation alice nc >>= getJSON 201

      withWebSockets [bob] $ \[wsBob] -> do
        rabbitmqAdminClient <- mkRabbitMqAdminClientForResource beResource
        let getActiveQueues :: App [String] =
              Text.unpack . (.name)
                <$$> ( (.items)
                         <$> rabbitmqAdminClient.listQueuesByVHost
                           (fromString beResource.berVHost)
                           (fromString "")
                           True
                           100
                           1
                     )

        void $ deleteTeamMember team alice alex >>= getBody 202

        assertConvUserDeletedNotif wsBob alexId

        -- Delete the queue
        let backendNotificationQueueName = "backend-notifications." <> domain1
        void
          $ rabbitmqAdminClient.deleteQueue
            (fromString beResource.berVHost)
            (fromString backendNotificationQueueName)

        -- Ensure the queue was deleted
        eventually $ do
          queueNames <- getActiveQueues
          queueNames `shouldNotContain` [backendNotificationQueueName]

        void $ deleteTeamMember team alice alison >>= getBody 202

        Timeout.threadDelay . fromIntegral $ remotesRefreshInterval * 2

        -- Check that the queue was recreated
        eventually $ do
          queueNames <- getActiveQueues
          queueNames `shouldContain` [backendNotificationQueueName]

        assertConvUserDeletedNotif wsBob alisonId

----------------------------------------------------------------------
-- helpers
mkUserPlusClientWithDomain :: (HasCallStack, MakesValue domain) => domain -> App (Value, String, String)
mkUserPlusClientWithDomain domain = do
  user <- randomUser domain def
  uid <- objId user
  client <- addClient user def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
  cid <- objId client
  pure (user, uid, cid)

mkUserPlusClient :: (HasCallStack) => App (Value, String, String)
mkUserPlusClient = mkUserPlusClientWithDomain OwnDomain

data EventWebSocket = EventWebSocket
  { events :: Chan (Either WS.ConnectionException Value),
    ack :: MVar (Maybe Value),
    kill :: MVar (),
    done :: MVar ()
  }

createEventWebSockets ::
  (HasCallStack, MakesValue user) =>
  [(user, Maybe String)] ->
  Codensity App [EventWebSocket]
createEventWebSockets = traverse (uncurry createEventsWebSocket)

createEventsWebSocket ::
  (HasCallStack, MakesValue user) =>
  user ->
  Maybe String ->
  Codensity App EventWebSocket
createEventsWebSocket user cid = do
  eithWS <- createEventsWebSocketEither user cid Nothing
  case eithWS of
    Left e -> lift $ assertFailure $ "Websocket failed to connect due to handshake exception: " <> displayException e
    Right ws -> pure ws

createEventsWebSocketWithSync ::
  (HasCallStack, MakesValue user) =>
  user ->
  Maybe String ->
  Codensity App (String, EventWebSocket)
createEventsWebSocketWithSync user cid = do
  syncMarker <- lift randomId
  eithWS <- createEventsWebSocketEither user cid (Just syncMarker)
  case eithWS of
    Left e -> lift $ assertFailure $ "Websocket failed to connect due to handshake exception: " <> displayException e
    Right ws -> pure (syncMarker, ws)

createEventsWebSocketEither ::
  (HasCallStack, MakesValue user) =>
  user ->
  Maybe String ->
  Maybe String ->
  Codensity App (Either WS.HandshakeException EventWebSocket)
createEventsWebSocketEither user cid mSyncMarker = do
  eventsChan <- liftIO newChan
  ackChan <- liftIO newEmptyMVar
  serviceMap <- lift $ getServiceMap =<< objDomain user
  apiVersion <- lift $ getAPIVersionFor $ objDomain user
  wsStarted <- newEmptyMVar
  let minAPIVersion = 8
  lift
    . when (apiVersion < minAPIVersion)
    $ assertFailure ("Events websocket can only be created when APIVersion is at least " <> show minAPIVersion)

  varKill <- lift $ newEmptyMVar
  varDone <- lift $ newEmptyMVar

  uid <- lift $ objId =<< objQidObject user
  let HostPort caHost caPort = serviceHostPort serviceMap Cannon
      path = "/v" <> show apiVersion <> "/events" <> maybe "" ("?client=" <>) cid <> maybe "" ("&sync_marker=" <>) mSyncMarker
      caHdrs = [(fromString "Z-User", toByteString' uid)]
      app conn = do
        putMVar wsStarted (Right ())
        race_
          (wsRead conn `catch` (writeChan eventsChan . Left))
          (wsWrite conn)

      wsRead conn = withAsync (wsReadLoop conn) $ \loop -> do
        r <- race (takeMVar varKill) (wait loop)
        case r of
          Left _ -> cancel loop >> waitClosed conn
          Right _ -> pure ()

      waitClosed conn = do
        WS.receive conn >>= \case
          WS.ControlMessage (WS.Close _ _) ->
            void $ tryPutMVar varDone ()
          _ -> waitClosed conn

      wsReadLoop conn = forever $ do
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
        k (Right $ EventWebSocket eventsChan ackChan varKill varDone) `finally` do
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

killWebSocketClient :: EventWebSocket -> App ()
killWebSocketClient ws = do
  void $ tryPutMVar ws.kill ()
  takeMVar ws.done

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
assertFindsEvent = assertFindsEventConfigurableAck ackEvent

assertFindsEventConfigurableAck ::
  forall a.
  (HasCallStack) =>
  ((HasCallStack) => EventWebSocket -> Value -> App ()) ->
  EventWebSocket ->
  ((HasCallStack) => Value -> App a) ->
  App a
assertFindsEventConfigurableAck ackFun ws expectations = go 0
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
                  =<< runMaybeT
                    ( (lookupFieldM ev "data.event" >>= flip lookupFieldM "payload.0.type")
                        <|> (lookupFieldM ev "type")
                    )
              ackFun ws ev
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

consumeEventsUntilEndOfInitialSync :: (HasCallStack) => EventWebSocket -> String -> App [Value]
consumeEventsUntilEndOfInitialSync ws expectedMarkerId = go []
  where
    go events = do
      timeout 1_000_000 (readChan ws.events) >>= \case
        Nothing ->
          addJSONToFailureContext "events" events
            $ assertFailure "timed out waiting for end-of-initial-sync event"
        Just (Left e) ->
          assertFailure
            $ "Websocket closed while waiting for end-of-initial-sync event "
            <> displayException e
        Just (Right e) -> do
          ackEvent ws e
          t <- e %. "type" & asString
          if (t == "synchronization")
            then do
              markerId <- e %. "data.marker_id" & asString
              if (markerId == expectedMarkerId)
                then pure (events <> [e])
                else assertFailure $ "Expected marker_id " <> expectedMarkerId <> ", but got " <> markerId
            else go (events <> [e])

assertEndOfIniitalSync :: (HasCallStack) => EventWebSocket -> String -> App ()
assertEndOfIniitalSync ws endMarker =
  assertEvent ws $ \e -> do
    e %. "type" `shouldMatch` "synchronization"
    e %. "data.marker_id" `shouldMatch` endMarker

consumeAllEvents_ :: EventWebSocket -> App ()
consumeAllEvents_ = void . consumeAllEvents

emptyQueue :: (HasCallStack, MakesValue user) => user -> String -> App ()
emptyQueue user cid = do
  runCodensity (createEventsWebSocketWithSync user (Just cid)) $ \(endMarker, ws) -> do
    void $ consumeEventsUntilEndOfInitialSync ws endMarker

consumeAllEvents :: EventWebSocket -> App [Value]
consumeAllEvents ws = do
  timeout 1_000_000 (readChan ws.events) >>= \case
    Nothing -> pure []
    Just (Left e) ->
      assertFailure
        $ "Websocket closed while consuming all events: "
        <> displayException e
    Just (Right e) -> do
      ackEvent ws e
      (e :) <$> consumeAllEvents ws

consumeAllEventsNoAck :: EventWebSocket -> App [Value]
consumeAllEventsNoAck ws = do
  timeout 1_000_000 (readChan ws.events) >>= \case
    Nothing -> pure []
    Just (Left e) ->
      assertFailure
        $ "Websocket closed while consuming all events: "
        <> displayException e
    Just (Right e) -> do
      (e :) <$> consumeAllEventsNoAck ws

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
  opts <- asks (.rabbitMQConfig)
  servantClient <- liftIO $ mkRabbitMqAdminClientEnv opts {vHost = Text.pack backend.berVHost}
  pure . fromServant $ Servant.hoistClient (Proxy @(ToServant AdminAPI AsApi)) (liftIO @App) (toServant servantClient)
