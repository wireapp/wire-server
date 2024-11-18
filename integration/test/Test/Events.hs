module Test.Events where

import API.Brig
import API.BrigCommon
import API.Common
import API.Galley
import API.Gundeck
import qualified Control.Concurrent.Timeout as Timeout
import Control.Monad.Codensity
import Control.Monad.Trans.Class
import Control.Retry
import Data.ByteString.Conversion (toByteString')
import qualified Data.Text as Text
import Data.Timeout
import qualified Network.WebSockets as WS
import Notifications
import SetupHelpers
import Testlib.Prelude hiding (assertNoEvent)
import UnliftIO hiding (handle)

-- FUTUREWORK: Investigate why these tests are failing without
-- `withModifiedBackend`; No events are received otherwise.
testConsumeEventsOneWebSocket :: (HasCallStack) => App ()
testConsumeEventsOneWebSocket = do
  withModifiedBackend def \domain -> do
    alice <- randomUser domain def

    lastNotifResp <-
      retrying
        (constantDelay 10_000 <> limitRetries 10)
        (\_ resp -> pure $ resp.status == 404)
        (\_ -> getLastNotification alice def)
    lastNotifId <- lastNotifResp.json %. "id" & asString

    client <- addClient alice def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
    clientId <- objId client

    runCodensity (createEventsWebSocket alice clientId) $ \(eventsChan, ackChan) -> do
      deliveryTag <- assertEvent eventsChan $ \e -> do
        e %. "type" `shouldMatch` "event"
        e %. "data.event.payload.0.type" `shouldMatch` "user.client-add"
        e %. "data.event.payload.0.client.id" `shouldMatch` clientId
        e %. "data.delivery_tag"
      assertNoEvent eventsChan

      sendAck ackChan deliveryTag False
      assertNoEvent eventsChan

      handle <- randomHandle
      putHandle alice handle >>= assertSuccess

      assertEvent eventsChan $ \e -> do
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
  withModifiedBackend def $ \domain -> do
    alice <- randomUser domain def
    bob <- randomUser domain def

    aliceClient <- addClient alice def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
    aliceClientId <- objId aliceClient

    bobClient <- addClient bob def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
    bobClientId <- objId bobClient

    lowerCodensity $ do
      (aliceEventsChan, aliceAckChan) <- createEventsWebSocket alice aliceClientId
      (bobEventsChan, bobAckChan) <- createEventsWebSocket bob bobClientId
      lift $ assertClientAdd aliceClientId aliceEventsChan aliceAckChan
      lift $ assertClientAdd bobClientId bobEventsChan bobAckChan
  where
    assertClientAdd :: (HasCallStack) => String -> MVar Value -> MVar (Maybe Value) -> App ()
    assertClientAdd clientId eventsChan ackChan = do
      deliveryTag <- assertEvent eventsChan $ \e -> do
        e %. "data.event.payload.0.type" `shouldMatch` "user.client-add"
        e %. "data.event.payload.0.client.id" `shouldMatch` clientId
        e %. "data.delivery_tag"
      assertNoEvent eventsChan
      sendAck ackChan deliveryTag False

testConsumeEventsWhileHavingLegacyClients :: (HasCallStack) => App ()
testConsumeEventsWhileHavingLegacyClients = do
  withModifiedBackend def $ \domain -> do
    alice <- randomUser domain def

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

      runCodensity (createEventsWebSocket alice newClientId) $ \(eventsChan, _) ->
        assertEvent eventsChan $ \e -> do
          e %. "data.event.payload.0.type" `shouldMatch` "user.client-add"
          e %. "data.event.payload.0.client.id" `shouldMatch` newClientId

    -- All notifs are also in Cassandra because of the legacy client
    getNotifications alice def {since = Just lastNotifId} `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json %. "notifications.0.payload.0.type" `shouldMatch` "user.client-add"
      resp.json %. "notifications.1.payload.0.type" `shouldMatch` "user.client-add"

testConsumeEventsAcks :: (HasCallStack) => App ()
testConsumeEventsAcks = do
  withModifiedBackend def $ \domain -> do
    alice <- randomUser domain def
    client <- addClient alice def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
    clientId <- objId client

    runCodensity (createEventsWebSocket alice clientId) $ \(eventsChan, _ackChan) -> do
      assertEvent eventsChan $ \e -> do
        e %. "data.event.payload.0.type" `shouldMatch` "user.client-add"
        e %. "data.event.payload.0.client.id" `shouldMatch` clientId

    -- without ack, we receive the same event again
    runCodensity (createEventsWebSocket alice clientId) $ \(eventsChan, ackChan) -> do
      deliveryTag <- assertEvent eventsChan $ \e -> do
        e %. "data.event.payload.0.type" `shouldMatch` "user.client-add"
        e %. "data.event.payload.0.client.id" `shouldMatch` clientId
        e %. "data.delivery_tag"
      sendAck ackChan deliveryTag False

    runCodensity (createEventsWebSocket alice clientId) $ \(eventsChan, _) -> do
      assertNoEvent eventsChan

testConsumeEventsMultipleAcks :: (HasCallStack) => App ()
testConsumeEventsMultipleAcks = do
  withModifiedBackend def $ \domain -> do
    alice <- randomUser domain def
    client <- addClient alice def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
    clientId <- objId client

    handle <- randomHandle
    putHandle alice handle >>= assertSuccess

    runCodensity (createEventsWebSocket alice clientId) $ \(eventsChan, ackChan) -> do
      assertEvent eventsChan $ \e -> do
        e %. "data.event.payload.0.type" `shouldMatch` "user.client-add"
        e %. "data.event.payload.0.client.id" `shouldMatch` clientId

      deliveryTag <- assertEvent eventsChan $ \e -> do
        e %. "data.event.payload.0.type" `shouldMatch` "user.update"
        e %. "data.event.payload.0.user.handle" `shouldMatch` handle
        e %. "data.delivery_tag"

      sendAck ackChan deliveryTag True

    runCodensity (createEventsWebSocket alice clientId) $ \(eventsChan, _) -> do
      assertNoEvent eventsChan

testConsumeEventsAckNewEventWithoutAckingOldOne :: (HasCallStack) => App ()
testConsumeEventsAckNewEventWithoutAckingOldOne = do
  withModifiedBackend def $ \domain -> do
    alice <- randomUser domain def
    client <- addClient alice def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
    clientId <- objId client

    handle <- randomHandle
    putHandle alice handle >>= assertSuccess

    runCodensity (createEventsWebSocket alice clientId) $ \(eventsChan, ackChan) -> do
      assertEvent eventsChan $ \e -> do
        e %. "data.event.payload.0.type" `shouldMatch` "user.client-add"
        e %. "data.event.payload.0.client.id" `shouldMatch` clientId

      deliveryTagHandleAdd <- assertEvent eventsChan $ \e -> do
        e %. "data.event.payload.0.type" `shouldMatch` "user.update"
        e %. "data.event.payload.0.user.handle" `shouldMatch` handle
        e %. "data.delivery_tag"

      -- Only ack the handle add delivery tag
      sendAck ackChan deliveryTagHandleAdd False

    -- Expect client-add event to be delivered again.
    runCodensity (createEventsWebSocket alice clientId) $ \(eventsChan, ackChan) -> do
      deliveryTagClientAdd <- assertEvent eventsChan $ \e -> do
        e %. "data.event.payload.0.type" `shouldMatch` "user.client-add"
        e %. "data.event.payload.0.client.id" `shouldMatch` clientId
        e %. "data.delivery_tag"

      sendAck ackChan deliveryTagClientAdd False

    runCodensity (createEventsWebSocket alice clientId) $ \(eventsChan, _) -> do
      assertNoEvent eventsChan

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

    runCodensity (createEventsWebSocket alice clientId) $ \(eventsChan, ackChan) -> do
      assertEvent eventsChan $ \e -> do
        e %. "type" `shouldMatch` "notifications.missed"

      -- Until we ack the full sync, we can't get new events
      ackFullSync ackChan

      -- withEventsWebSocket alice clientId $ \eventsChan ackChan -> do
      -- Now we can see the next event
      assertEvent eventsChan $ \e -> do
        e %. "data.event.payload.0.type" `shouldMatch` "user.update"
        e %. "data.event.payload.0.user.handle" `shouldMatch` handle1
        ackEvent ackChan e

      -- We've consumed the whole queue.
      assertNoEvent eventsChan

testTransientEventsDoNotTriggerDeadLetters :: (HasCallStack) => App ()
testTransientEventsDoNotTriggerDeadLetters = do
  let notifTTL = 1 # Second
  withModifiedBackend (def {gundeckCfg = setField "settings.notificationTTL" (notifTTL #> Second)}) $ \domain -> do
    alice <- randomUser domain def
    -- Creates a non-transient event
    client <- addClient alice def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
    clientId <- objId client

    -- consume it
    runCodensity (createEventsWebSocket alice clientId) $ \(eventsChan, ackChan) -> do
      assertEvent eventsChan $ \e -> do
        e %. "data.event.payload.0.type" `shouldMatch` "user.client-add"
        e %. "type" `shouldMatch` "event"
        e %. "data.event.payload.0.type" `shouldMatch` "user.client-add"
        e %. "data.event.payload.0.client.id" `shouldMatch` clientId
        deliveryTag <- e %. "data.delivery_tag"
        sendAck ackChan deliveryTag False

    -- Self conv ID is same as user's ID, we'll use this to send typing
    -- indicators, so we don't have to create another conv.
    selfConvId <- objQidObject alice
    -- Typing status is transient, currently no one is listening.
    sendTypingStatus alice selfConvId "started" >>= assertSuccess

    runCodensity (createEventsWebSocket alice clientId) $ \(eventsChan, _ackChan) -> do
      assertNoEvent eventsChan

testTransientEvents :: (HasCallStack) => App ()
testTransientEvents = do
  withModifiedBackend def $ \domain -> do
    alice <- randomUser domain def
    client <- addClient alice def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
    clientId <- objId client

    -- Self conv ID is same as user's ID, we'll use this to send typing
    -- indicators, so we don't have to create another conv.
    selfConvId <- objQidObject alice

    runCodensity (createEventsWebSocket alice clientId) $ \(eventsChan, ackChan) -> do
      consumeAllEvents eventsChan ackChan
      sendTypingStatus alice selfConvId "started" >>= assertSuccess
      assertEvent eventsChan $ \e -> do
        e %. "data.event.payload.0.type" `shouldMatch` "conversation.typing"
        e %. "data.event.payload.0.qualified_conversation" `shouldMatch` selfConvId
        deliveryTag <- e %. "data.delivery_tag"
        sendAck ackChan deliveryTag False

    handle1 <- randomHandle
    putHandle alice handle1 >>= assertSuccess

    sendTypingStatus alice selfConvId "stopped" >>= assertSuccess

    handle2 <- randomHandle
    putHandle alice handle2 >>= assertSuccess

    -- We shouldn't see the stopped typing status because we were not connected to
    -- the websocket when it was sent. The other events should still show up in
    -- order.
    runCodensity (createEventsWebSocket alice clientId) $ \(eventsChan, ackChan) -> do
      for_ [handle1, handle2] $ \handle ->
        assertEvent eventsChan $ \e -> do
          e %. "data.event.payload.0.type" `shouldMatch` "conversation.typing"
          e %. "data.event.payload.0.qualified_conversation" `shouldMatch` selfConvId
          deliveryTag <- e %. "data.delivery_tag"
          sendAck ackChan deliveryTag False

      assertNoEvent eventsChan

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
    clients <-
      replicateM 3
        $ addClient alice def {acapabilities = Just ["consumable-notifications"]}
        >>= getJSON 201
        >>= (%. "id")
        >>= asString

    lowerCodensity $ do
      acks <- for clients $ \c -> do
        (events0, ack0) <- createEventsWebSocket alice c
        e <- Codensity $ \k -> assertEvent events0 k
        lift $ do
          e %. "data.event.payload.0.type" `shouldMatch` "user.client-add"
          e %. "data.event.payload.0.client.id" `shouldMatch` c
          tag <- e %. "data.delivery_tag"
          pure (sendAck ack0 tag False)
      lift $ sequenceA_ acks

----------------------------------------------------------------------
-- helpers

createEventsWebSocket ::
  (HasCallStack, MakesValue uid) =>
  uid ->
  String ->
  Codensity App (MVar Value, MVar (Maybe Value))
createEventsWebSocket user cid = do
  eventsChan <- liftIO newEmptyMVar
  ackChan <- liftIO newEmptyMVar
  serviceMap <- lift $ getServiceMap =<< objDomain user
  uid <- lift $ objId =<< objQidObject user
  let HostPort caHost caPort = serviceHostPort serviceMap Cannon
      path = "/events?client=" <> cid
      caHdrs = [(fromString "Z-User", toByteString' uid)]
      app conn =
        race_
          ( wsRead conn `catch` \(e :: WS.ConnectionException) ->
              case e of
                WS.CloseRequest {} -> pure ()
                _ -> throwIO e
          )
          (wsWrite conn)

      wsRead conn = forever $ do
        bs <- WS.receiveData conn
        case decodeStrict' bs of
          Just n -> putMVar eventsChan n
          Nothing ->
            error $ "Failed to decode events: " ++ show bs

      wsWrite conn = forever $ do
        mAck <- takeMVar ackChan
        case mAck of
          Nothing -> WS.sendClose conn (Text.pack "")
          Just ack -> WS.sendBinaryData conn (encode ack)

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
    k (eventsChan, ackChan) `finally` do
      putMVar ackChan Nothing
      liftIO $ wait wsThread

ackFullSync :: (HasCallStack) => MVar (Maybe Value) -> App ()
ackFullSync ackChan =
  putMVar ackChan
    $ Just (object ["type" .= "ack_full_sync"])

ackEvent :: (HasCallStack) => MVar (Maybe Value) -> Value -> App ()
ackEvent ackChan event = do
  deliveryTag <- event %. "data.delivery_tag"
  sendAck ackChan deliveryTag False

sendAck :: (HasCallStack) => MVar (Maybe Value) -> Value -> Bool -> App ()
sendAck ackChan deliveryTag multiple =
  do
    putMVar ackChan
    $ Just
    $ object
      [ "type" .= "ack",
        "data"
          .= object
            [ "delivery_tag" .= deliveryTag,
              "multiple" .= multiple
            ]
      ]

assertEvent :: (HasCallStack) => MVar Value -> ((HasCallStack) => Value -> App a) -> App a
assertEvent eventsChan expectations = do
  timeout 10_000_000 (takeMVar eventsChan) >>= \case
    Nothing -> assertFailure "No event received for 1s"
    Just e -> do
      pretty <- prettyJSON e
      addFailureContext ("event:\n" <> pretty)
        $ expectations e

assertNoEvent :: (HasCallStack) => MVar Value -> App ()
assertNoEvent eventsChan = do
  timeout 1_000_000 (takeMVar eventsChan) >>= \case
    Nothing -> pure ()
    Just e -> do
      eventJSON <- prettyJSON e
      assertFailure $ "Did not expect event: \n" <> eventJSON

consumeAllEvents :: MVar Value -> MVar (Maybe Value) -> App ()
consumeAllEvents eventsChan ackChan = do
  timeout 1_000_000 (takeMVar eventsChan) >>= \case
    Nothing -> pure ()
    Just e -> do
      ackEvent ackChan e
      consumeAllEvents eventsChan ackChan
