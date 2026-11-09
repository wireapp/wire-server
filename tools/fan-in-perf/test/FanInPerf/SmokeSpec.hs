module FanInPerf.SmokeSpec (spec) where

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Char8 qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.UUID.Types qualified as UUID
import FanInPerf.Delivery
import FanInPerf.Load
import FanInPerf.Options
import FanInPerf.Smoke
import FanInPerf.Wakeup
import Imports
import Network.HTTP.Types
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (testWithApplication)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets qualified as WS
import Options.Applicative hiding (action, value)
import Test.Hspec hiding (pending)
import UnliftIO.Exception (tryAny)

spec :: Spec
spec = do
  describe "smoke command options" $ do
    it "does not require database credentials" $ case getParseResult $ execParserPure defaultPrefs invocationInfo ["smoke"] of
      Just (SmokeRun opts) -> opts `shouldBe` SmokeOptions "http://127.0.0.1:8086" "ws://127.0.0.1:8083" 30
      _ -> expectationFailure "smoke did not parse"
    it "keeps the existing database commands available" $ case getParseResult $ execParserPure defaultPrefs invocationInfo ["--db", "postgresql://localhost/test", "db-info"] of
      Just (DatabaseRun opts) -> opts.command `shouldBe` DbInfo
      _ -> expectationFailure "database command did not parse"
    for_ ["0", "-1", "301", "invalid"] $ \value ->
      it ("rejects invalid timeout " <> value) $ case execParserPure defaultPrefs invocationInfo ["smoke", "--timeout-seconds", value] of
        Failure _ -> pure ()
        _ -> expectationFailure "invalid timeout accepted"
    it "rejects credentials, wrong schemes and non-root endpoints" $
      for_ ["https://localhost", "http://user:password@localhost", "http://localhost/path", "http://localhost?token=secret", "http://localhost:99999"] $ \url ->
        parseEndpoint "http:" url `shouldSatisfy` isLeft

  describe "service load generator" $ do
    it "parses load options without database credentials" $ case getParseResult $ execParserPure defaultPrefs invocationInfo ["load", "--devices", "3", "--rate", "4", "--duration", "1"] of
      Just (LoadRun opts) -> (opts.devices, opts.rate, opts.duration) `shouldBe` (3, 4, 1)
      _ -> expectationFailure "load did not parse"
    for_ ["devices", "rate", "duration", "drain-seconds", "setup-seconds", "payload-bytes", "connect-concurrency"] $ \name ->
      it ("rejects zero " <> name) $ case execParserPure defaultPrefs invocationInfo ["load", "--" <> name, "0"] of
        Failure _ -> pure ()
        _ -> expectationFailure "zero accepted"
    it "rejects overflowing numeric input" $ case execParserPure defaultPrefs invocationInfo ["load", "--rate", "18446744073709551617"] of
      Failure _ -> pure ()
      _ -> expectationFailure "overflowing rate accepted"
    it "uses bounded latency histogram buckets" $
      map latencyBucket [0, 1, 1.1, 2, 3, 1000] `shouldBe` [1, 1, 2, 2, 4, 1024]
    it "delivers each published event to every device and waits for acknowledgements" $
      withFixture True True $ \opts -> do
        actions <- networkActions opts
        stats <- newTVarIO emptyStats
        connecting <- newTVarIO (0 :: Int, 0 :: Int)
        let observed =
              actions
                { withClient = \device action -> do
                    atomically $ modifyTVar' connecting $ \(current, peak) -> (current + 1, max peak (current + 1))
                    withClient actions device $ \socket -> do
                      firstReady <- newIORef True
                      let receive = do
                            frame <- socket.receive
                            when (frame == Ready) $ do
                              first <- atomicModifyIORef' firstReady (\old -> (False, old))
                              when first $ atomically $ modifyTVar' connecting $ \(current, peak) -> (current - 1, peak)
                            pure frame
                      action socket {receive = receive}
                }
        runLoadWith (loadOptions opts) observed stats (const $ pure ())
        readTVarIO connecting `shouldReturn` (0, 1)
        final <- readTVarIO stats
        final.published `shouldSatisfy` (> 0)
        final.published `shouldSatisfy` (<= 4)
        final.attempted `shouldBe` final.published
        final.acknowledged `shouldBe` (3 * final.published)
        sum (Map.elems final.latencyHistogram) `shouldBe` final.acknowledged
    it "fails the drain when connected devices receive no wakeup" $
      withFixture False True $ \opts -> do
        actions <- networkActions opts
        stats <- newTVarIO emptyStats
        result <- tryAny $ runLoadWith (loadOptions opts) actions stats (const $ pure ())
        result `shouldSatisfy` failedWith "drain timed out"
    it "reports the failure type and ready count without raw exception contents" $
      withFixture True True $ \opts -> do
        actions <- networkActions opts
        stats <- newTVarIO emptyStats
        let broken = actions {withClient = \_ _ -> fail "PRIVATE_EXCEPTION_DETAIL"}
        result <- tryAny $ runLoadWith (loadOptions opts) broken stats (const $ pure ())
        result `shouldSatisfy` failedWith "0/3 reached ready"
        result `shouldSatisfy` failedWith "I/O failure"
        result `shouldSatisfy` (not . failedWith "PRIVATE_EXCEPTION_DETAIL")

    it "does not retry an ambiguous failed publish" $
      withFixture True True $ \opts -> do
        actions <- networkActions opts
        stats <- newTVarIO emptyStats
        let broken = actions {publish = \payload -> actions.publish payload >> fail "publish response lost"}
        result <- tryAny $ runLoadWith (loadOptions opts) broken stats (const $ pure ())
        result `shouldSatisfy` isLeft
        final <- readTVarIO stats
        final.attempted `shouldBe` 1
        final.published `shouldBe` 0

  describe "smoke against in-process HTTP and WebSocket services" $ do
    it "checks live delivery, ack persistence and offline catch-up" $
      withFixture True True $ \opts -> do
        messages <- newIORef []
        actions <- networkActions opts
        runScenario 2 (\msg -> modifyIORef' messages (<> [msg])) actions runId alice bob
        logs <- readIORef messages
        last logs `shouldBe` "OK: acknowledged events are not replayed after reconnect"
    it "fails when RabbitMQ/Cannon wakeups do not reach connected clients" $
      withFixture False True $ \opts -> do
        actions <- networkActions opts
        outcome <- tryAny $ runScenario 1 (const $ pure ()) actions runId alice bob
        outcome `shouldSatisfy` failedWith "timeout"
    it "fails when acknowledgements are confirmed but not persisted" $
      withFixture True False $ \opts -> do
        actions <- networkActions opts
        outcome <- tryAny $ runScenario 2 (const $ pure ()) actions runId alice bob
        outcome `shouldSatisfy` failedWith "replayed"
  where
    loadOptions opts = LoadOptions opts 3 4 1 1 5 32 1 Nothing
    failedWith fragment (Left err) = case fromException err of
      Just (SmokeFailure message) -> fragment `T.isInfixOf` message
      Nothing -> False
    failedWith _ _ = False
    runId = UUID.fromWords 1 0 0 0
    alice = Device (UUID.fromWords 2 0 0 0) "1"
    bob = Device (UUID.fromWords 3 0 0 0) "1"

-- This fixture exercises the command's actual HTTP/WS clients without external
-- services. Only the documented API routes exist; there is no DB/reset endpoint.
withFixture :: Bool -> Bool -> (SmokeOptions -> IO a) -> IO a
withFixture wake persist action = do
  hub <- newWakeupHub
  rows <- newTVarIO []
  positions <- newTVarIO Map.empty
  let identity = withObject "device" $ \o -> (,) <$> o .: "user" <*> o .: "client"
      http request respond = do
        body <- Wai.strictRequestBody request
        case Wai.pathInfo request of
          ["i", "fan-in-perf", "register"] -> case decode body >>= parseMaybe identity of
            Nothing -> respond $ Wai.responseLBS status400 [] ""
            Just key -> do
              atomically $ modifyTVar' positions (Map.insertWith (\_ old -> old) key UUID.nil)
              respond $ Wai.responseLBS status200 [] "null"
          ["i", "fan-in-perf", "initialize"] -> respond $ Wai.responseLBS status200 [] "null"
          ["i", "fan-in-perf", "publish"] -> case decode body >>= parseMaybe (withObject "publish" (.: "payload")) of
            Nothing -> respond $ Wai.responseLBS status400 [] ""
            Just payload -> do
              atomically $ do
                events <- readTVar rows
                writeTVar rows $ events <> [(UUID.fromWords 0 0 0 (fromIntegral $ length events + 1), payload)]
                when wake $ signal hub
              respond $ Wai.responseLBS status200 [] "null"
          _ -> respond $ Wai.responseLBS status404 [] ""
      websocket pending = do
        let headers = WS.requestHeaders $ WS.pendingRequest pending
            user = lookup "Z-User" headers >>= UUID.fromString . BS.unpack
            client = T.pack . BS.unpack <$> lookup "Z-Client" headers
        cursor <- readTVarIO positions
        case do
          key <- (,) <$> user <*> client
          start <- Map.lookup key cursor
          pure (key, start) of
          Nothing -> WS.rejectRequest pending "unknown participant"
          Just (key, start) -> do
            socket <- WS.acceptRequest pending
            void $
              tryAny $
                deliverEvents
                  hub
                  start
                  (\position -> filter ((> position) . fst) <$> readTVarIO rows)
                  (\position -> when persist $ atomically $ modifyTVar' positions $ Map.insert key position)
                  (WS.sendBinaryData socket . encode)
                  (WS.receiveData socket)
      app = websocketsOr WS.defaultConnectionOptions websocket http
  testWithApplication (pure app) $ \port ->
    action $ SmokeOptions ("http://127.0.0.1:" <> show port) ("ws://127.0.0.1:" <> show port) 2
