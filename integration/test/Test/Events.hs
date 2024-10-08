module Test.Events where

import API.Brig
import API.BrigCommon
import API.Common
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan
import qualified Data.Aeson as A
import Data.ByteString.Conversion (toByteString')
import Data.String.Conversions (cs)
import qualified Network.WebSockets.Client as WS
import qualified Network.WebSockets.Connection as WS
import SetupHelpers
import Testlib.Prelude hiding (assertNoEvent)
import UnliftIO (Async, async, bracket, cancel, race, waitAny)
import UnliftIO.Concurrent (threadDelay)

testConsumeEventsOneWebSocket :: (HasCallStack) => App ()
testConsumeEventsOneWebSocket = do
  alice <- randomUser OwnDomain def
  client <- addClient alice def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
  clientId <- objId client

  withEventsWebSocket alice clientId $ \eventsChan ackChan -> do
    deliveryTag <- assertEvent eventsChan $ \e -> do
      e %. "payload.0.type" `shouldMatch` "user.client-add"
      e %. "payload.0.client.id" `shouldMatch` clientId
      e %. "delivery_tag"
    assertNoEvent eventsChan

    sendAck ackChan deliveryTag
    assertNoEvent eventsChan

    handle <- randomHandle
    putHandle alice handle >>= assertSuccess

    assertEvent eventsChan $ \e -> do
      e %. "payload.0.type" `shouldMatch` "user.update"
      e %. "payload.0.user.handle" `shouldMatch` handle

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
    sendAck ackChan deliveryTag

  withEventsWebSocket alice clientId $ \eventsChan _ -> do
    assertNoEvent eventsChan

testPingPong :: (HasCallStack) => App ()
testPingPong = do
  alice <- randomUser OwnDomain def
  client <- addClient alice def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
  clientId <- objId client

  withEventsWebSocket alice clientId $ \eventsChan ackChan -> do
    assertEvent eventsChan $ const $ pure ()
    sendMsg ackChan $ object ["type" .= "ping"]
    assertEvent eventsChan $ \e -> e %. "type" `shouldMatch` "pong"

----------------------------------------------------------------------
-- helpers

withEventsWebSocket :: (HasCallStack, MakesValue uid) => uid -> String -> (TChan Value -> TChan Value -> App a) -> App a
withEventsWebSocket uid cid f = do
  bracket setup (\(_, _, wsThread) -> cancel wsThread) $ \(eventsChan, ackChan, _) -> do
    f eventsChan ackChan
  where
    setup :: (HasCallStack) => App (TChan Value, TChan Value, Async ())
    setup = do
      (eventsChan, ackChan) <- liftIO $ (,) <$> newTChanIO <*> newTChanIO
      wsThread <- eventsWebSocket uid cid eventsChan ackChan
      pure (eventsChan, ackChan, wsThread)

sendMsg :: (HasCallStack) => TChan Value -> Value -> App ()
sendMsg eventsChan msg = liftIO $ atomically $ writeTChan eventsChan msg

sendAck :: (HasCallStack) => TChan Value -> Value -> App ()
sendAck ackChan deliveryTag = sendMsg ackChan $ object ["type" .= "ack", "data" .= object ["delivery_tag" .= deliveryTag, "multiple" .= False]]

assertEvent :: (HasCallStack) => TChan Value -> (Value -> App a) -> App a
assertEvent eventsChan expectations = do
  mEvent <- race (threadDelay 1_000_000) (liftIO $ atomically (readTChan eventsChan))
  case mEvent of
    Left () -> assertFailure "No event received for 1s"
    Right e -> expectations e

assertNoEvent :: (HasCallStack) => TChan Value -> App ()
assertNoEvent eventsChan = do
  mEvent <- race (threadDelay 1_000_000) (liftIO $ atomically (readTChan eventsChan))
  case mEvent of
    Left () -> pure ()
    Right e -> assertFailure $ "Did not expect event: " <> cs (A.encode e)

eventsWebSocket :: (MakesValue user) => user -> String -> TChan Value -> TChan Value -> App (Async ())
eventsWebSocket user clientId eventsChan ackChan = do
  serviceMap <- getServiceMap =<< objDomain user
  uid <- objId =<< objQidObject user
  let HostPort caHost caPort = serviceHostPort serviceMap Cannon
      path = "/events?client=" <> clientId
      caHdrs = [(fromString "Z-User", toByteString' uid)]
      app conn = do
        r <- async $ wsRead conn
        w <- async $ wsWrite conn
        void $ waitAny [r, w]
      wsRead conn = forever $ do
        bs <- WS.receiveData conn
        case decodeStrict' bs of
          Just n -> atomically $ writeTChan eventsChan n
          Nothing -> putStrLn $ "Failed to decode events: " ++ show bs
      wsWrite conn = forever $ do
        ack <- atomically $ readTChan ackChan
        WS.sendBinaryData conn (encode ack)
  liftIO
    $ async
    $ WS.runClientWith
      caHost
      (fromIntegral caPort)
      path
      WS.defaultConnectionOptions
      caHdrs
      app
