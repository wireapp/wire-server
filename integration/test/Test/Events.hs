module Test.Events where

import API.Brig
import API.BrigCommon
import API.Common
import API.Gundeck
import Control.Retry
import qualified Data.Aeson as A
import Data.ByteString.Conversion (toByteString')
import Data.String.Conversions (cs)
import qualified Data.Text as Text
import qualified Network.WebSockets as WS
import Notifications
import SetupHelpers
import Testlib.Prelude hiding (assertNoEvent)
import Testlib.Printing
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

  withEventsWebSocket alice clientId $ \eventsChan ackChan -> do
    deliveryTag <- assertEvent eventsChan $ \e -> do
      e %. "payload.0.type" `shouldMatch` "user.client-add"
      e %. "payload.0.client.id" `shouldMatch` clientId
      e %. "delivery_tag"
    assertNoEvent eventsChan

    sendAck ackChan deliveryTag False
    assertNoEvent eventsChan

    handle <- randomHandle
    putHandle alice handle >>= assertSuccess

    assertEvent eventsChan $ \e -> do
      e %. "payload.0.type" `shouldMatch` "user.update"
      e %. "payload.0.user.handle" `shouldMatch` handle

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

  userIdsContext <- mkContextUserIds [("alice", alice), ("bob", bob)]
  addFailureContext userIdsContext $ do
    withEventsWebSockets [(alice, aliceClientId), (bob, bobClientId)] $ \[(aliceEventsChan, aliceAckChan), (bobEventsChan, bobAckChan)] -> do
      assertClientAdd aliceClientId aliceEventsChan aliceAckChan
      assertClientAdd bobClientId bobEventsChan bobAckChan
  where
    assertClientAdd :: (HasCallStack) => String -> TChan Value -> TChan Value -> App ()
    assertClientAdd clientId eventsChan ackChan = do
      deliveryTag <- assertEvent eventsChan $ \e -> do
        e %. "payload.0.type" `shouldMatch` "user.client-add"
        e %. "payload.0.client.id" `shouldMatch` clientId
        e %. "delivery_tag"
      assertNoEvent eventsChan
      sendAck ackChan deliveryTag False

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

    withEventsWebSocket alice newClientId $ \eventsChan _ ->
      assertEvent eventsChan $ \e -> do
        e %. "payload.0.type" `shouldMatch` "user.client-add"
        e %. "payload.0.client.id" `shouldMatch` newClientId

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

  withEventsWebSocket alice clientId $ \eventsChan _ackChan -> do
    assertEvent eventsChan $ \e -> do
      e %. "payload.0.type" `shouldMatch` "user.client-add"
      e %. "payload.0.client.id" `shouldMatch` clientId

  -- without ack, we receive the same event again
  withEventsWebSocket alice clientId $ \eventsChan ackChan -> do
    deliveryTag <- assertEvent eventsChan $ \e -> do
      e %. "payload.0.type" `shouldMatch` "user.client-add"
      e %. "payload.0.client.id" `shouldMatch` clientId
      e %. "delivery_tag"
    sendAck ackChan deliveryTag False

  withEventsWebSocket alice clientId $ \eventsChan _ -> do
    assertNoEvent eventsChan

testConsumeEventsMultipleAcks :: (HasCallStack) => App ()
testConsumeEventsMultipleAcks = do
  alice <- randomUser OwnDomain def
  client <- addClient alice def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
  clientId <- objId client

  handle <- randomHandle
  putHandle alice handle >>= assertSuccess

  withEventsWebSocket alice clientId $ \eventsChan ackChan -> do
    assertEvent eventsChan $ \e -> do
      e %. "payload.0.type" `shouldMatch` "user.client-add"
      e %. "payload.0.client.id" `shouldMatch` clientId

    deliveryTag <- assertEvent eventsChan $ \e -> do
      e %. "payload.0.type" `shouldMatch` "user.update"
      e %. "payload.0.user.handle" `shouldMatch` handle
      e %. "delivery_tag"

    sendAck ackChan deliveryTag True

  withEventsWebSocket alice clientId $ \eventsChan _ -> do
    assertNoEvent eventsChan

testConsumeEventsAckNewEventWithoutAckingOldOne :: (HasCallStack) => App ()
testConsumeEventsAckNewEventWithoutAckingOldOne = do
  alice <- randomUser OwnDomain def
  client <- addClient alice def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
  clientId <- objId client

  handle <- randomHandle
  putHandle alice handle >>= assertSuccess

  withEventsWebSocket alice clientId $ \eventsChan ackChan -> do
    assertEvent eventsChan $ \e -> do
      e %. "payload.0.type" `shouldMatch` "user.client-add"
      e %. "payload.0.client.id" `shouldMatch` clientId

    deliveryTagHandleAdd <- assertEvent eventsChan $ \e -> do
      e %. "payload.0.type" `shouldMatch` "user.update"
      e %. "payload.0.user.handle" `shouldMatch` handle
      e %. "delivery_tag"

    -- Only ack the handle add delivery tag
    sendAck ackChan deliveryTagHandleAdd False

  -- Expect client-add event to be delivered again.
  withEventsWebSocket alice clientId $ \eventsChan ackChan -> do
    deliveryTagClientAdd <- assertEvent eventsChan $ \e -> do
      e %. "payload.0.type" `shouldMatch` "user.client-add"
      e %. "payload.0.client.id" `shouldMatch` clientId
      e %. "delivery_tag"

    sendAck ackChan deliveryTagClientAdd False

  withEventsWebSocket alice clientId $ \eventsChan _ -> do
    assertNoEvent eventsChan

----------------------------------------------------------------------
-- helpers

withEventsWebSockets :: forall uid a. (HasCallStack, MakesValue uid) => [(uid, String)] -> ([(TChan Value, TChan Value)] -> App a) -> App a
withEventsWebSockets userClients k = go [] $ reverse userClients
  where
    go :: [(TChan Value, TChan Value)] -> [(uid, String)] -> App a
    go chans [] = k chans
    go chans ((uid, cid) : remaining) =
      withEventsWebSocket uid cid $ \eventsChan ackChan ->
        go ((eventsChan, ackChan) : chans) remaining

withEventsWebSocket :: (HasCallStack, MakesValue uid) => uid -> String -> (TChan Value -> TChan Value -> App a) -> App a
withEventsWebSocket uid cid k = do
  closeWS <- newEmptyMVar
  bracket (setup closeWS) (\(_, _, wsThread) -> cancel wsThread) $ \(eventsChan, ackChan, wsThread) -> do
    x <- k eventsChan ackChan

    -- Ensure all the acks are sent before closing the websocket
    isAckChanEmpty <-
      retrying
        (limitRetries 5 <> constantDelay 10_000)
        (\_ isEmpty -> pure $ not isEmpty)
        (\_ -> atomically $ isEmptyTChan ackChan)
    unless isAckChanEmpty $ do
      putStrLn $ colored yellow $ "The ack chan is not empty after 50ms, some acks may not make it to the server"

    void $ tryPutMVar closeWS ()

    timeout 1_000_000 (wait wsThread) >>= \case
      Nothing ->
        putStrLn $ colored yellow $ "The websocket thread did not close after waiting for 1s"
      Just () -> pure ()

    pure x
  where
    setup :: (HasCallStack) => MVar () -> App (TChan Value, TChan Value, Async ())
    setup closeWS = do
      (eventsChan, ackChan) <- liftIO $ (,) <$> newTChanIO <*> newTChanIO
      wsThread <- eventsWebSocket uid cid eventsChan ackChan closeWS
      pure (eventsChan, ackChan, wsThread)

sendMsg :: (HasCallStack) => TChan Value -> Value -> App ()
sendMsg eventsChan msg = liftIO $ atomically $ writeTChan eventsChan msg

sendAck :: (HasCallStack) => TChan Value -> Value -> Bool -> App ()
sendAck ackChan deliveryTag multiple = do
  sendMsg ackChan
    $ object
      [ "type" .= "ack",
        "data"
          .= object
            [ "delivery_tag" .= deliveryTag,
              "multiple" .= multiple
            ]
      ]

assertEvent :: (HasCallStack) => TChan Value -> ((HasCallStack) => Value -> App a) -> App a
assertEvent eventsChan expectations = do
  timeout 10_000_000 (atomically (readTChan eventsChan)) >>= \case
    Nothing -> assertFailure "No event received for 1s"
    Just e -> do
      pretty <- prettyJSON e
      addFailureContext ("event:\n" <> pretty)
        $ expectations e

assertNoEvent :: (HasCallStack) => TChan Value -> App ()
assertNoEvent eventsChan = do
  timeout 1_000_000 (atomically (readTChan eventsChan)) >>= \case
    Nothing -> pure ()
    Just e -> assertFailure $ "Did not expect event: " <> cs (A.encode e)

eventsWebSocket :: (MakesValue user) => user -> String -> TChan Value -> TChan Value -> MVar () -> App (Async ())
eventsWebSocket user clientId eventsChan ackChan closeWS = do
  serviceMap <- getServiceMap =<< objDomain user
  uid <- objId =<< objQidObject user
  let HostPort caHost caPort = serviceHostPort serviceMap Cannon
      path = "/events?client=" <> clientId
      caHdrs = [(fromString "Z-User", toByteString' uid)]
      app conn = do
        r <-
          async $ wsRead conn `catch` \(e :: WS.ConnectionException) ->
            case e of
              WS.CloseRequest {} -> pure ()
              _ -> throwIO e
        w <- async $ wsWrite conn
        void $ waitAny [r, w]

      wsRead conn = forever $ do
        bs <- WS.receiveData conn
        case decodeStrict' bs of
          Just n -> atomically $ writeTChan eventsChan n
          Nothing ->
            -- TODO: Throw an error
            putStrLn $ "Failed to decode events: " ++ show bs

      wsWrite conn = forever $ do
        eitherAck <- race (readMVar closeWS) (atomically $ readTChan ackChan)
        case eitherAck of
          Left () -> WS.sendClose conn (Text.pack "")
          Right ack -> WS.sendBinaryData conn (encode ack)
  liftIO
    $ async
    $ WS.runClientWith
      caHost
      (fromIntegral caPort)
      path
      WS.defaultConnectionOptions
      caHdrs
      app
