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
import Testlib.Prelude
import UnliftIO (Async, async, cancel, race, waitAny)
import UnliftIO.Concurrent (threadDelay)

testConsumeEventsOneWebSocket :: (HasCallStack) => App ()
testConsumeEventsOneWebSocket = do
  alice <- randomUser OwnDomain def
  client <- addClient alice def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
  clientId <- objId client

  eventsChan <- liftIO newTChanIO
  ackChan <- liftIO newTChanIO
  wsThread <- eventsWebSocket alice clientId eventsChan ackChan

  deliveryTag <- assertEventOnSameWebSocket eventsChan $ \(e :: Value) -> do
    e %. "payload.0.type" `shouldMatch` "user.client-add"
    e %. "payload.0.client.id" `shouldMatch` clientId
    e %. "delivery_tag"

  sendEventOnSameWebSocket ackChan $ object ["ack" .= deliveryTag]
  assertNoEventOnSameWebSocket eventsChan

  handle <- randomHandle
  putHandle alice handle >>= assertSuccess

  assertEventOnNewWebSocket alice clientId $ \(e :: Value) -> do
    e %. "payload.0.type" `shouldMatch` "user.update"
    e %. "payload.0.user.handle" `shouldMatch` handle

  cancel wsThread

testConsumeEventsNewWebSockets :: (HasCallStack) => App ()
testConsumeEventsNewWebSockets = do
  alice <- randomUser OwnDomain def
  client <- addClient alice def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
  clientId <- objId client

  deliveryTag <- assertEventOnNewWebSocket alice clientId $ \(e :: Value) -> do
    e %. "payload.0.type" `shouldMatch` "user.client-add"
    e %. "payload.0.client.id" `shouldMatch` clientId
    e %. "delivery_tag"

  sendEventOnNewWebSocket alice clientId $ object ["ack" .= deliveryTag]
  assertNoEventOnNewWebSocket alice clientId

  handle <- randomHandle
  putHandle alice handle >>= assertSuccess

  assertEventOnNewWebSocket alice clientId $ \(e :: Value) -> do
    e %. "payload.0.type" `shouldMatch` "user.update"
    e %. "payload.0.user.handle" `shouldMatch` handle

----------------------------------------------------------------------
-- helpers

sendEventOnSameWebSocket :: (HasCallStack) => TChan Value -> Value -> App ()
sendEventOnSameWebSocket ackChan msg = do
  liftIO $ atomically $ writeTChan ackChan msg

sendEventOnNewWebSocket :: (HasCallStack, MakesValue uid) => uid -> String -> Value -> App ()
sendEventOnNewWebSocket uid cid msg = do
  eventsChan <- liftIO newTChanIO
  ackChan <- liftIO newTChanIO
  wsThread <- eventsWebSocket uid cid eventsChan ackChan
  sendEventOnSameWebSocket ackChan msg
  -- TODO: is there enough time here to send the message before the websocket is closed?
  cancel wsThread

assertEventOnSameWebSocket :: (HasCallStack) => TChan Value -> (Value -> App a) -> App a
assertEventOnSameWebSocket eventsChan expectations = do
  mEvent <- race (threadDelay 1_000_000) (liftIO $ atomically (readTChan eventsChan))
  case mEvent of
    Left () -> assertFailure "No event recieved for 1s"
    Right e -> expectations e

assertEventOnNewWebSocket :: (HasCallStack, MakesValue uid) => uid -> String -> (Value -> App a) -> App a
assertEventOnNewWebSocket uid cid expectations = do
  eventsChan <- liftIO newTChanIO
  ackChan <- liftIO newTChanIO
  wsThread <- eventsWebSocket uid cid eventsChan ackChan
  result <- assertEventOnSameWebSocket eventsChan expectations
  cancel wsThread
  pure result

assertNoEventOnSameWebSocket :: (HasCallStack) => TChan Value -> App ()
assertNoEventOnSameWebSocket eventsChan = do
  mEvent <- race (threadDelay 1_000_000) (liftIO $ atomically (readTChan eventsChan))
  case mEvent of
    Left () -> pure ()
    Right e -> assertFailure $ "Did not expect event: " <> cs (A.encode e)

assertNoEventOnNewWebSocket :: (HasCallStack, MakesValue uid) => uid -> String -> App ()
assertNoEventOnNewWebSocket uid cid = do
  eventsChan <- liftIO newTChanIO
  ackChan <- liftIO newTChanIO
  wsThread <- eventsWebSocket uid cid eventsChan ackChan
  assertNoEventOnSameWebSocket eventsChan
  cancel wsThread

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

-- TODO: test pingpong?  or drop it?
