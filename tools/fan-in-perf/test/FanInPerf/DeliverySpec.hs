module FanInPerf.DeliverySpec (spec) where

import Control.Concurrent.Async (waitCatch, withAsync)
import Control.Exception (throwIO)
import Control.Exception qualified as E
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy qualified as BL
import Data.UUID.Types qualified as UUID
import FanInPerf.Delivery
import FanInPerf.Wakeup
import Imports
import Network.HTTP.Types (status404)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (testWithApplication)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets qualified as Socket
import System.IO.Error (userError)
import System.Timeout (timeout)
import Test.Hspec hiding (after, pending)

spec :: Spec
spec = describe "experimental delivery sessions" $ do
  it "delivers to two clients independently and resumes persisted progress" $ do
    hub <- newWakeupHub
    let first = UUID.fromWords 0 0 0 1
        second = UUID.fromWords 0 0 0 2
    rows <- newTVarIO [(first, Null)]
    alice <- newClient
    bob <- newClient
    aCursor <- newIORef UUID.nil
    bCursor <- newIORef UUID.nil
    let run client cursor = do
          start <- readIORef cursor
          deliverEvents hub start (\after -> filter ((> after) . fst) <$> readTVarIO rows) (writeIORef cursor) (send client) (receive client)
    withAsync (run alice aCursor) $ \_ -> do
      withAsync (run bob bCursor) $ \_ -> do
        expectType alice "events"
        expectType bob "events"
        ack alice 1
        expectType alice "acked"
        expectType alice "ready"
        -- Bob disconnects without acknowledging his copy.
        readIORef bCursor `shouldReturn` UUID.nil
      atomically $ modifyTVar' rows (<> [(second, Null)]) >> signal hub
      expectType alice "events"
      ack alice 2
      expectType alice "acked"
      readIORef aCursor `shouldReturn` second
      reconnected <- newClient
      withAsync (run reconnected bCursor) $ \_ -> do
        page <- next reconnected
        parseMaybe (withObject "page" (.: "events")) page
          `shouldBe` Just [object ["id" .= first, "payload" .= Null], object ["id" .= second, "payload" .= Null]]
        ack reconnected 1
        expectType reconnected "acked"
        readIORef bCursor `shouldReturn` second
    -- Reconnecting an acknowledged client does not replay the backlog.
    caughtUp <- newClient
    withAsync (run caughtUp aCursor) $ \_ -> expectType caughtUp "ready"

  it "runs two real WebSocket clients and replays only the unacknowledged client" $ do
    hub <- newWakeupHub
    aCursor <- newIORef UUID.nil
    bCursor <- newIORef UUID.nil
    let nid = UUID.fromWords 0 0 0 1
        server pending = do
          let cursor = if Socket.requestPath (Socket.pendingRequest pending) == "/a" then aCursor else bCursor
          connection <- Socket.acceptRequest pending
          start <- readIORef cursor
          E.handle
            ( \case
                Socket.CloseRequest _ _ -> pure ()
                Socket.ConnectionClosed -> pure ()
                err -> throwIO err
            )
            $ deliverEvents
              hub
              start
              (\after -> pure [(nid, Null) | after < nid])
              (writeIORef cursor)
              (Socket.sendTextData connection . encode)
              (Socket.receiveData connection)
        app = websocketsOr Socket.defaultConnectionOptions server (\_ respond -> respond $ Wai.responseLBS status404 [] "")
        frame connection expected = do
          bytes <- timeout 2_000_000 (Socket.receiveData connection :: IO BL.ByteString)
          let kind = bytes >>= decode >>= parseMaybe (withObject "frame" (.: "type"))
          kind `shouldBe` Just (expected :: Text)
        confirm connection = do
          Socket.sendTextData connection $ encode $ object ["type" .= ("ack" :: Text), "tag" .= (1 :: Int)]
          frame connection "acked"
          frame connection "ready"
    testWithApplication (pure app) $ \port -> do
      Socket.runClient "127.0.0.1" port "/a" $ \a -> do
        frame a "events"
        Socket.runClient "127.0.0.1" port "/b" $ \b -> do
          frame b "events"
          confirm a
          Socket.sendClose b ("disconnect without ack" :: Text)
        Socket.sendClose a ("done" :: Text)
      Socket.runClient "127.0.0.1" port "/a" $ \a -> frame a "ready" >> Socket.sendClose a ("done" :: Text)
      Socket.runClient "127.0.0.1" port "/b" $ \b -> do
        frame b "events"
        confirm b
        Socket.sendClose b ("done" :: Text)
      readIORef aCursor `shouldReturn` nid
      readIORef bCursor `shouldReturn` nid

  it "propagates a disconnect without wrapping it in a linked-thread exception" $ do
    hub <- newWakeupHub
    client <- newClient
    disconnect <- newEmptyTMVarIO
    let receiveClosed = atomically (takeTMVar disconnect) >> throwIO Socket.ConnectionClosed
    withAsync (deliverEvents hub UUID.nil (const $ pure []) (const $ pure ()) (send client) receiveClosed) $ \session -> do
      expectType client "ready"
      atomically $ putTMVar disconnect ()
      result <- timeout 2_000_000 $ waitCatch session
      case result of
        Just (Left err) -> case fromException err of
          Just Socket.ConnectionClosed -> pure ()
          _ -> expectationFailure "disconnect exception was wrapped or replaced"
        _ -> expectationFailure "disconnected session did not terminate"

  it "does not persist progress for an invalid acknowledgement" $ do
    hub <- newWakeupHub
    client <- newClient
    writes <- newIORef (0 :: Int)
    withAsync (deliverEvents hub UUID.nil (const $ pure [(UUID.fromWords 0 0 0 1, Null)]) (const $ modifyIORef' writes (+ 1)) (send client) (receive client)) $ \session -> do
      expectType client "events"
      ack client 2
      result <- timeout 2_000_000 $ waitCatch session
      result `shouldSatisfy` maybe False isLeft
      readIORef writes `shouldReturn` 0

  for_ ["not-json", "{}", BL.replicate 1025 32] $ \invalid ->
    it ("rejects malformed or oversized acknowledgements (" <> show (BL.length invalid) <> " bytes)") $ do
      hub <- newWakeupHub
      client <- newClient
      writes <- newIORef (0 :: Int)
      withAsync (deliverEvents hub UUID.nil (const $ pure [(UUID.fromWords 0 0 0 1, Null)]) (const $ modifyIORef' writes (+ 1)) (send client) (receive client)) $ \session -> do
        expectType client "events"
        atomically $ writeTQueue client.incoming invalid
        result <- timeout 2_000_000 $ waitCatch session
        result `shouldSatisfy` maybe False isLeft
        readIORef writes `shouldReturn` 0

  it "does not confirm an acknowledgement when persistence fails" $ do
    hub <- newWakeupHub
    client <- newClient
    withAsync (deliverEvents hub UUID.nil (const $ pure [(UUID.fromWords 0 0 0 1, Null)]) (const $ throwIO $ userError "synthetic failure") (send client) (receive client)) $ \session -> do
      expectType client "events"
      ack client 1
      result <- timeout 2_000_000 $ waitCatch session
      result `shouldSatisfy` maybe False isLeft
      atomically (isEmptyTQueue client.outgoing) `shouldReturn` True

  it "does not lose a wakeup arriving during an empty fetch" $ do
    hub <- newWakeupHub
    client <- newClient
    calls <- newIORef (0 :: Int)
    let fetch _ = do
          n <- atomicModifyIORef' calls (\n -> (n + 1, n))
          if n == 0 then atomically (signal hub) $> [] else pure [(UUID.fromWords 0 0 0 1, Null)]
    withAsync (deliverEvents hub UUID.nil fetch (const $ pure ()) (send client) (receive client)) $ \_ -> do
      expectType client "ready"
      expectType client "events"

data Client = Client {incoming :: TQueue BL.ByteString, outgoing :: TQueue Value}

newClient :: IO Client
newClient = Client <$> newTQueueIO <*> newTQueueIO

send :: Client -> Value -> IO ()
send client = atomically . writeTQueue client.outgoing

receive :: Client -> IO BL.ByteString
receive client = atomically $ readTQueue client.incoming

ack :: Client -> Integer -> IO ()
ack client tag = atomically $ writeTQueue client.incoming $ encode $ object ["type" .= ("ack" :: Text), "tag" .= tag]

next :: Client -> IO Value
next client = timeout 2_000_000 (atomically $ readTQueue client.outgoing) >>= maybe (expectationFailure "no frame received" $> Null) pure

expectType :: Client -> Text -> IO ()
expectType client expected = do
  frame <- next client
  parseMaybe (withObject "frame" (.: "type")) frame `shouldBe` Just expected
