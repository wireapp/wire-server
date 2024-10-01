module Test.Events where

import API.Brig
import API.BrigCommon
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan
import Data.ByteString.Conversion (toByteString')
import qualified Network.WebSockets.Client as WS
import qualified Network.WebSockets.Connection as WS
import SetupHelpers
import Testlib.Prelude
import UnliftIO (Async, async, cancel, race)
import UnliftIO.Concurrent (threadDelay)

testConsumeEvents :: (HasCallStack) => App ()
testConsumeEvents = do
  alice <- randomUser OwnDomain def
  client <- addClient alice def {acapabilities = Just ["consumable-notifications"]} >>= getJSON 201
  clientId <- objId client
  eventsChan <- liftIO newTChanIO
  wsThread <- eventsWebSocket alice clientId eventsChan
  mEvent <- race (threadDelay 1_000_000) (liftIO $ atomically (readTChan eventsChan))
  case mEvent of
    Left () -> assertFailure "No event recieved for 1s"
    Right e -> do
      e %. "payload.0.type" `shouldMatch` "user.client-add"
      e %. "payload.0.client.id" `shouldMatch` clientId
  cancel wsThread

eventsWebSocket :: (MakesValue user) => user -> String -> TChan Value -> App (Async ())
eventsWebSocket user clientId eventsChan = do
  serviceMap <- getServiceMap =<< objDomain user
  uid <- objId =<< objQidObject user
  let HostPort caHost caPort = serviceHostPort serviceMap Cannon
      path = "/events?client=" <> clientId
      caHdrs = [(fromString "Z-User", toByteString' uid)]
      app = wsRead
      -- r <- async wsRead
      -- w <- async wsWrite
      wsRead conn = forever $ do
        bs <- WS.receiveData conn
        case decodeStrict' bs of
          Just n -> atomically $ writeTChan eventsChan n
          Nothing -> putStrLn $ "Failed to decode events: " ++ show bs
  -- wsWrite = forever $ do
  --   takeMVar latch
  --   WS.sendClose conn ("close" :: ByteString)
  liftIO
    $ async
    $ WS.runClientWith
      caHost
      (fromIntegral caPort)
      path
      WS.defaultConnectionOptions
      caHdrs
      app
