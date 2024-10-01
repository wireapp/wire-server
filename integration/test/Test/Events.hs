module Test.Events where

import API.Brig
import API.BrigCommon
import API.Common
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan
import Data.ByteString.Conversion (toByteString')
import qualified Network.WebSockets.Client as WS
import qualified Network.WebSockets.Connection as WS
import SetupHelpers
import Testlib.Prelude
import UnliftIO (Async, async, cancel, race, waitAny)
import UnliftIO.Concurrent (threadDelay)

testConsumeEvents :: (HasCallStack) => App ()
testConsumeEvents = do
  alice <- randomUser OwnDomain def
  client <- addClient alice def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
  clientId <- objId client
  do
    eventsChan <- liftIO newTChanIO
    ackChan <- liftIO newTChanIO
    wsThread <- eventsWebSocket alice clientId eventsChan ackChan
    mEvent <- race (threadDelay 1_000_000) (liftIO $ atomically (readTChan eventsChan))
    case mEvent of
      Left () -> assertFailure "No event recieved for 1s"
      Right e -> do
        e %. "payload.0.type" `shouldMatch` "user.client-add"
        e %. "payload.0.client.id" `shouldMatch` clientId
        deliveryTag <- e %. "delivery_tag"
        liftIO $ atomically $ writeTChan ackChan $ object ["ack" .= deliveryTag]
    cancel wsThread

  handle <- randomHandle
  putHandle alice handle >>= assertSuccess
  do
    eventsChan <- liftIO newTChanIO
    ackChan <- liftIO newTChanIO
    wsThread <- eventsWebSocket alice clientId eventsChan ackChan
    mEvent <- race (threadDelay 1_000_000) (liftIO $ atomically (readTChan eventsChan))
    case mEvent of
      Left () -> assertFailure "No event recieved for 1s"
      Right e -> do
        e %. "payload.0.type" `shouldMatch` "user.update"
        e %. "payload.0.user.handle" `shouldMatch` handle
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
