-- | Load the experimental service path without direct database access.
module FanInPerf.Load (LoadOptions (..), loadParser, runLoad, runLoadWith, LoadStats (..), DeviceProgress (..), latencyBucket, emptyStats) where

import Control.Concurrent.Async (link, mapConcurrently_, race, withAsync)
import Control.Concurrent.STM (check)
import Control.Exception (finally, throwIO)
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.UUID.V4 qualified as UUID
import FanInPerf.Smoke
import GHC.Clock (getMonotonicTimeNSec)
import Imports
import Network.HTTP.Client qualified as HTTP
import Network.WebSockets qualified as WS
import Options.Applicative hiding (action)
import System.Exit (die)
import System.IO qualified as IO
import System.IO.Error (ioeGetErrorType)
import System.Timeout (timeout)
import UnliftIO.Exception (tryAny)

data LoadOptions = LoadOptions
  { service :: SmokeOptions,
    devices :: Int,
    rate :: Int,
    duration :: Int,
    drainSeconds :: Int,
    setupSeconds :: Int,
    payloadBytes :: Int,
    connectConcurrency :: Int,
    summaryFile :: Maybe FilePath
  }
  deriving (Show)

loadParser :: Parser LoadOptions
loadParser =
  LoadOptions
    <$> smokeParser
    <*> number "devices" 10 100000 "Number of connected synthetic devices"
    <*> number "rate" 1 100000 "Target publishes per second (one sequential publisher)"
    <*> number "duration" 30 86400 "Publishing duration in seconds"
    <*> number "drain-seconds" 30 3600 "Time allowed for final acknowledgements"
    <*> number "setup-seconds" 300 86400 "Deadline for registration and connecting all devices"
    <*> number "payload-bytes" 256 60000 "ASCII padding bytes, excluding JSON envelope"
    <*> number "connect-concurrency" 20 100000 "Maximum devices simultaneously establishing a ready WebSocket session"
    <*> optional (strOption (long "summary-json" <> metavar "FILE" <> help "Write final measurements as JSON"))
  where
    number :: String -> Int -> Int -> String -> Parser Int
    number name def maximumValue description =
      option
        (eitherReader readNumber)
        (long name <> value def <> showDefault <> help description)
      where
        readNumber input = case reads input :: [(Integer, String)] of
          [(n, "")] | n > 0 && n <= toInteger maximumValue -> Right (fromInteger n)
          _ -> Left $ name <> " must be between 1 and " <> show maximumValue

data LoadStats = LoadStats
  { attempted :: !Int,
    published :: !Int,
    received :: !Int,
    ackSentDeliveries :: !Int,
    ackRequestsSent :: !Int,
    ackConfirmations :: !Int,
    ackLatencyTotalMs :: !Double,
    ackLatencyMaxMs :: !Double,
    ackLatencyHistogram :: !(Map Int Int),
    deviceProgress :: !(Map Int DeviceProgress),
    acknowledged :: !Int,
    latencyTotalMs :: !Double,
    latencyMaxMs :: !Double,
    latencyHistogram :: !(Map Int Int),
    publishSeconds :: !Double
  }
  deriving (Eq, Show)

data DeviceProgress = DeviceProgress
  { lastReceived :: !Int,
    lastConfirmed :: !Int,
    pendingSince :: !(Maybe Word64),
    ackSendCompleted :: !Bool
  }
  deriving (Eq, Show)

-- Power-of-two millisecond upper bounds keep memory independent of run length.
latencyBucket :: Double -> Int
latencyBucket milliseconds = go 1
  where
    go bound
      | fromIntegral bound >= milliseconds = bound
      | bound > maxBound `div` 2 = maxBound
      | otherwise = go (bound * 2)

emptyStats :: LoadStats
emptyStats =
  LoadStats
    { attempted = 0,
      published = 0,
      received = 0,
      ackSentDeliveries = 0,
      ackRequestsSent = 0,
      ackConfirmations = 0,
      ackLatencyTotalMs = 0,
      ackLatencyMaxMs = 0,
      ackLatencyHistogram = Map.empty,
      deviceProgress = Map.empty,
      acknowledged = 0,
      latencyTotalMs = 0,
      latencyMaxMs = 0,
      latencyHistogram = Map.empty,
      publishSeconds = 0
    }

failLoad :: Text -> IO a
failLoad = throwIO . SmokeFailure

bounded :: Int -> Text -> IO a -> IO a
bounded seconds label action = timeout (seconds * 1000000) action >>= maybe (failLoad $ label <> " timed out") pure

runLoad :: LoadOptions -> IO ()
runLoad opts = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  stats <- newTVarIO emptyStats
  result <- tryAny $ do
    actions <- networkActions opts.service
    runLoadWith opts actions stats (putStrLn . T.unpack)
  final <- readTVarIO stats
  snapshotTime <- getMonotonicTimeNSec
  let expected = opts.devices * final.published
      missing = max 0 (expected - final.acknowledged)
      mean = if final.received == 0 then 0 else final.latencyTotalMs / fromIntegral final.received
      percentile fraction = quantile (ceiling (fraction * fromIntegral final.received)) (Map.toAscList final.latencyHistogram)
      ackPercentile fraction = quantile (ceiling (fraction * fromIntegral final.ackConfirmations)) (Map.toAscList final.ackLatencyHistogram)
      lagging =
        [ object
            [ "device_index" .= index,
              "last_sequence_received" .= d.lastReceived,
              "last_sequence_confirmed" .= d.lastConfirmed,
              "ack_pending" .= isJust d.pendingSince,
              "ack_send_completed" .= d.ackSendCompleted,
              "pending_ack_age_ms" .= fmap (\start -> fromIntegral (snapshotTime - min snapshotTime start) / 1000000 :: Double) d.pendingSince
            ]
        | (index, d) <- Map.toAscList final.deviceProgress,
          d.lastConfirmed < final.published || isJust d.pendingSince
        ]
      quantile _ [] = 0 :: Int
      quantile n ((bound, count) : rest) = if n <= count then bound else quantile (n - count) rest
      summary =
        object
          [ "success" .= isRight result,
            "devices" .= opts.devices,
            "duration_seconds" .= opts.duration,
            "payload_padding_bytes" .= opts.payloadBytes,
            "expected_deliveries" .= expected,
            "target_publish_rate" .= opts.rate,
            "attempted" .= final.attempted,
            "publish_errors" .= (final.attempted - final.published),
            "published" .= final.published,
            "received_deliveries" .= final.received,
            "ack_sent_deliveries" .= final.ackSentDeliveries,
            "ack_not_sent_deliveries" .= (final.received - final.ackSentDeliveries),
            "ack_sent_unconfirmed_deliveries" .= (final.ackSentDeliveries - final.acknowledged),
            "ack_requests_sent" .= final.ackRequestsSent,
            "ack_confirmations_received" .= final.ackConfirmations,
            "not_received_deliveries" .= max 0 (expected - final.received),
            "received_unconfirmed_deliveries" .= (final.received - final.acknowledged),
            "ack_latency_mean_ms" .= (if final.ackConfirmations == 0 then 0 else final.ackLatencyTotalMs / fromIntegral final.ackConfirmations),
            "ack_latency_max_ms" .= final.ackLatencyMaxMs,
            "ack_latency_p50_upper_ms" .= ackPercentile (0.5 :: Double),
            "ack_latency_p95_upper_ms" .= ackPercentile (0.95 :: Double),
            "ack_latency_p99_upper_ms" .= ackPercentile (0.99 :: Double),
            "lagging_devices" .= lagging,
            "acknowledged_deliveries" .= final.acknowledged,
            "missing_deliveries" .= missing,
            "publish_seconds" .= final.publishSeconds,
            "achieved_publish_rate" .= (if final.publishSeconds == 0 then 0 else fromIntegral final.published / final.publishSeconds),
            "latency_mean_ms" .= mean,
            "latency_max_ms" .= final.latencyMaxMs,
            "latency_p50_upper_ms" .= percentile (0.5 :: Double),
            "latency_p95_upper_ms" .= percentile (0.95 :: Double),
            "latency_p99_upper_ms" .= percentile (0.99 :: Double)
          ]
  BL.putStrLn $ encodePretty summary
  for_ opts.summaryFile $ \path -> encodeFile path summary
  case result of
    Right () -> putStrLn "PASS: all published events acknowledged by all devices"
    Left err -> case fromException err of
      Just (SmokeFailure detail) -> die $ "FAIL: " <> T.unpack detail
      Nothing -> die $ "FAIL: " <> T.unpack (describeFailure err)

-- Do not print raw exceptions: HTTP/WS exceptions may contain URLs or bodies.
describeFailure :: SomeException -> Text
describeFailure err
  | Just (SmokeFailure detail) <- fromException err = detail
  | Just ioError <- fromException err = "I/O failure: " <> T.pack (show $ ioeGetErrorType ioError) <> "; check socket/file-descriptor limits and service availability"
  | Just (_ :: WS.HandshakeException) <- fromException err = "WebSocket handshake failed; check Cannon's first error and database pool availability"
  | Just (_ :: WS.ConnectionException) <- fromException err = "WebSocket connection closed or protocol error; check Cannon's first error"
  | Just (_ :: HTTP.HttpException) <- fromException err = "HTTP request failed; check Gundeck availability and request timeout"
  | otherwise = "service communication failed; inspect the first service error"

runLoadWith :: LoadOptions -> SmokeActions -> TVar LoadStats -> (Text -> IO ()) -> IO ()
runLoadWith opts actions stats report = do
  runId <- UUID.nextRandom
  clients <- replicateM opts.devices (Device <$> UUID.nextRandom <*> pure "1")
  atomically $ modifyTVar' stats $ \s -> s {deviceProgress = Map.fromList [(index, DeviceProgress 0 0 Nothing False) | index <- [1 .. opts.devices]]}
  ready <- newTVarIO (0 :: Int)
  slots <- newTVarIO opts.connectConcurrency
  let registerAll = for_ clients $ \device -> actions.register device >> actions.initialize device
      deviceReader index device = do
        atomically $ do
          available <- readTVar slots
          check (available > 0)
          writeTVar slots (available - 1)
        withClient actions device $ \socket -> do
          let loop previous announced =
                socket.receive >>= \case
                  Ready -> do
                    unless announced $ atomically $ do
                      modifyTVar' ready (+ 1)
                      modifyTVar' slots (+ 1)
                    loop previous True
                  Acked _ -> failLoad "unexpected acknowledgement confirmation"
                  Events tag rows -> do
                    receivedAt <- getMonotonicTimeNSec
                    let own =
                          mapMaybe
                            ( parseMaybe
                                ( withObject "load payload" $ \o -> do
                                    marker <- o .: "fan_in_perf_load_run"
                                    guard (marker == runId)
                                    (,) <$> o .: "sequence" <*> o .: "sent_ns"
                                )
                                . snd
                            )
                            rows ::
                            [(Int, Word64)]
                    unless (map fst own == take (length own) [previous + 1 ..]) $ failLoad "duplicate, missing or out-of-order event"
                    let latencies = [fromIntegral (receivedAt - min receivedAt sent) / 1000000 | (_, sent) <- own]
                    -- Record receipt before waiting for acknowledgement persistence.
                    atomically $ modifyTVar' stats $ \s ->
                      s
                        { received = s.received + length own,
                          latencyTotalMs = s.latencyTotalMs + sum latencies,
                          latencyMaxMs = maximum (s.latencyMaxMs : latencies),
                          latencyHistogram = foldl' (\hist ms -> Map.insertWith (+) (latencyBucket ms) 1 hist) s.latencyHistogram latencies,
                          deviceProgress = Map.adjust (\d -> d {lastReceived = previous + length own}) index s.deviceProgress
                        }
                    ackStarted <- getMonotonicTimeNSec
                    atomically $ modifyTVar' stats $ \s ->
                      s
                        { deviceProgress = Map.adjust (\d -> d {pendingSince = Just ackStarted, ackSendCompleted = False}) index s.deviceProgress
                        }
                    socket.acknowledge tag
                    atomically $ modifyTVar' stats $ \s ->
                      s
                        { ackSentDeliveries = s.ackSentDeliveries + length own,
                          ackRequestsSent = s.ackRequestsSent + 1,
                          deviceProgress = Map.adjust (\d -> d {ackSendCompleted = True}) index s.deviceProgress
                        }
                    confirmation <- bounded opts.service.timeoutSeconds "acknowledgement" socket.receive
                    unless (confirmation == Acked tag) $ failLoad "missing acknowledgement confirmation"
                    confirmedAt <- getMonotonicTimeNSec
                    let ackMs = fromIntegral (confirmedAt - ackStarted) / 1000000
                    atomically $ modifyTVar' stats $ \s ->
                      s
                        { acknowledged = s.acknowledged + length own,
                          ackConfirmations = s.ackConfirmations + 1,
                          ackLatencyTotalMs = s.ackLatencyTotalMs + ackMs,
                          ackLatencyMaxMs = max s.ackLatencyMaxMs ackMs,
                          ackLatencyHistogram = Map.insertWith (+) (latencyBucket ackMs) 1 s.ackLatencyHistogram,
                          deviceProgress = Map.adjust (\d -> d {lastConfirmed = previous + length own, pendingSince = Nothing, ackSendCompleted = False}) index s.deviceProgress
                        }
                    loop (previous + length own) announced
          loop 0 False
      guardedReader (index, device) = do
        result <- tryAny $ deviceReader index device
        case result of
          Right () -> failLoad "device session ended unexpectedly"
          Left err -> do
            count <- readTVarIO ready
            failLoad $ "device " <> T.pack (show index) <> " session failed (" <> T.pack (show count) <> "/" <> T.pack (show opts.devices) <> " reached ready): " <> describeFailure err
      publisher = do
        start <- getMonotonicTimeNSec
        let end = start + fromIntegral opts.duration * 1000000000
            interval = 1000000000 `div` fromIntegral opts.rate
            publishLoop sequenceNumber due = do
              now <- getMonotonicTimeNSec
              when (now < due) $ threadDelay $ fromIntegral ((due - now + 999) `div` 1000)
              sent <- getMonotonicTimeNSec
              when (sent < end) $ do
                atomically $ modifyTVar' stats $ \s -> s {attempted = s.attempted + 1}
                actions.publish $
                  object
                    [ "fan_in_perf_load_run" .= runId,
                      "sequence" .= sequenceNumber,
                      "sent_ns" .= sent,
                      "padding" .= T.replicate opts.payloadBytes "x"
                    ]
                atomically $ modifyTVar' stats $ \s -> s {published = s.published + 1}
                finished <- getMonotonicTimeNSec
                -- No catch-up burst when the publisher cannot sustain the target rate.
                publishLoop (sequenceNumber + 1) (max (due + interval) finished)
        publishLoop (1 :: Int) start `finally` do
          endTime <- getMonotonicTimeNSec
          atomically $ modifyTVar' stats $ \s -> s {publishSeconds = fromIntegral (endTime - start) / 1000000000}
      progress = forever $ do
        threadDelay 1000000
        s <- readTVarIO stats
        count <- readTVarIO ready
        report $
          T.pack $
            "ready="
              <> show count
              <> "/"
              <> show opts.devices
              <> " published="
              <> show s.published
              <> " received="
              <> show s.received
              <> " ack_sent="
              <> show s.ackSentDeliveries
              <> " not_received="
              <> show (max 0 $ opts.devices * s.published - s.received)
              <> " received_unconfirmed="
              <> show (s.received - s.acknowledged)
              <> " acknowledged="
              <> show s.acknowledged
              <> " outstanding="
              <> show (max 0 $ opts.devices * s.published - s.acknowledged)
      controller = do
        bounded opts.setupSeconds "connecting devices" $ atomically $ readTVar ready >>= check . (== opts.devices)
        report "All devices connected; publishing"
        publisher
        completed <- readTVarIO stats
        when (completed.published == 0) $ failLoad "no events published within the requested duration"
        report "Publishing finished; draining acknowledgements"
        bounded opts.drainSeconds "drain" $ atomically $ do
          s <- readTVar stats
          check (s.acknowledged == opts.devices * s.published)
  report "Registering fresh devices"
  bounded opts.setupSeconds "registration" registerAll
  report $ "Connecting devices (concurrency " <> T.pack (show opts.connectConcurrency) <> ")"
  withAsync progress $ \monitor -> do
    link monitor
    race (mapConcurrently_ guardedReader (zip [1 ..] clients)) controller >>= \case
      Left () -> failLoad "all device sessions unexpectedly stopped"
      Right () -> pure ()
