-- | API-only smoke test for already-running experimental services.
module FanInPerf.Smoke
  ( SmokeOptions (..),
    smokeParser,
    runSmoke,
    runScenario,
    SmokeActions (..),
    SmokeClient (..),
    FetchObservation (..),
    Device (..),
    Frame (..),
    SmokeFailure (..),
    parseEndpoint,
    networkActions,
  )
where

import Control.Concurrent.Async (concurrently)
import Control.Exception (throwIO)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import GHC.Clock (getMonotonicTimeNSec)
import Imports
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (statusCode)
import Network.URI qualified as URI
import Network.WebSockets qualified as WS
import Options.Applicative hiding (action)
import System.Exit (die)
import System.Timeout (timeout)
import UnliftIO.Exception (tryAny)

data SmokeOptions = SmokeOptions
  {gundeckUrl :: String, cannonUrl :: String, timeoutSeconds :: Int, fetchPercent :: Int}
  deriving (Eq, Show)

smokeParser :: Parser SmokeOptions
smokeParser =
  SmokeOptions
    <$> option (endpointReader "http:") (long "gundeck-url" <> value "http://127.0.0.1:8086" <> showDefault <> help "Gundeck's internal HTTP base URL")
    <*> option (endpointReader "ws:") (long "cannon-url" <> value "ws://127.0.0.1:8083" <> showDefault <> help "Cannon's internal WebSocket base URL")
    <*> option (eitherReader readTimeout) (long "timeout-seconds" <> value 30 <> showDefault <> help "Timeout per test step (1-300 seconds)")
    <*> option (eitherReader readPercent) (long "fetch-percent" <> value 0 <> showDefault <> help "Percentage of devices using WebSocket wakeups and HTTP fetch/ack (0-100); smoke uses two devices")
  where
    readPercent input = case reads input :: [(Integer, String)] of
      [(n, "")] | n >= 0 && n <= 100 -> Right (fromInteger n)
      _ -> Left "fetch-percent must be between 0 and 100"
    endpointReader scheme = eitherReader $ \url -> url <$ parseEndpoint scheme url
    readTimeout input = case reads input of
      [(n, "")] | n >= 1 && n <= 300 -> Right n
      _ -> Left "timeout must be an integer between 1 and 300"

-- Only internal, plaintext local-service endpoints are supported. No credentials,
-- query parameters or path prefixes: this avoids forwarding credentials elsewhere.
parseEndpoint :: String -> String -> Either String (String, Int)
parseEndpoint scheme url = do
  uri <- maybe (Left "invalid service URL") Right (URI.parseURI url)
  auth <- maybe (Left "service URL needs a host") Right (URI.uriAuthority uri)
  unless
    ( URI.uriScheme uri == scheme
        && null (URI.uriUserInfo auth)
        && URI.uriPath uri `elem` ["", "/"]
        && null (URI.uriQuery uri)
        && null (URI.uriFragment uri)
        && not (null (URI.uriRegName auth))
    )
    $ Left ("use a " <> scheme <> "//host[:port] URL without credentials, query or path")
  port <- case URI.uriPort auth of
    "" -> Right 80
    ':' : digits -> case reads digits of
      [(n, "")] | n >= 1 && n <= 65535 -> Right n
      _ -> Left "invalid service port"
    _ -> Left "invalid service port"
  let host = URI.uriRegName auth
  pure (if take 1 host == "[" then take (length host - 2) (drop 1 host) else host, port)

newtype SmokeFailure = SmokeFailure Text deriving (Show)

instance Exception SmokeFailure

failure :: Text -> IO a
failure = throwIO . SmokeFailure

data Device = Device {user :: UUID, client :: Text} deriving (Eq, Show)

data Frame = Ready | Events Integer [(UUID, Value)] | Acked Integer | Wakeup Integer UUID deriving (Eq, Show)

instance FromJSON Frame where
  parseJSON = withObject "frame" $ \o -> do
    kind <- o .: "type"
    case kind :: Text of
      "ready" -> pure Ready
      "acked" -> Acked <$> o .: "tag"
      "wakeup" -> do
        tag <- o .: "tag"
        unless (tag > 0) $ fail "invalid wakeup tag"
        Wakeup tag <$> o .: "cursor"
      "events" -> do
        tag <- o .: "tag"
        rows <- o .: "events" >>= traverse (withObject "event" $ \e -> (,) <$> e .: "id" <*> e .: "payload")
        unless (tag > 0 && not (null rows) && length rows <= 100) $ fail "invalid event page"
        pure (Events tag rows)
      _ -> fail "unknown frame"

data SmokeClient = SmokeClient {receive :: IO Frame, acknowledge :: Integer -> IO ()}

-- Measurements for one HTTP fetch (including empty catch-up probes).
data FetchObservation = FetchObservation
  { eventsFetched :: Int,
    requestMs :: Double,
    wakeupToFetchMs :: Maybe Double
  }

data SmokeActions = SmokeActions
  { register :: Device -> IO (),
    initialize :: Device -> IO (),
    publish :: Value -> IO (),
    withClient :: forall a. Device -> (SmokeClient -> IO a) -> IO a,
    withFetchClient :: forall a. Device -> (FetchObservation -> IO ()) -> (SmokeClient -> IO a) -> IO a
  }

networkActions :: SmokeOptions -> IO SmokeActions
networkActions opts = do
  (host, port) <- either (failure . T.pack) pure $ parseEndpoint "ws:" opts.cannonUrl
  void $ either (failure . T.pack) pure $ parseEndpoint "http:" opts.gundeckUrl
  manager <- HTTP.newManager HTTP.defaultManagerSettings
  let base = reverse $ dropWhile (== '/') $ reverse opts.gundeckUrl
      post endpoint body = do
        request <- HTTP.parseRequest $ base <> "/i/fan-in-perf/" <> endpoint
        response <-
          HTTP.httpNoBody
            request
              { HTTP.method = "POST",
                HTTP.requestBody = HTTP.RequestBodyLBS (encode body),
                HTTP.requestHeaders = [("Content-Type", "application/json")],
                HTTP.redirectCount = 0,
                HTTP.responseTimeout = HTTP.responseTimeoutMicro (opts.timeoutSeconds * 1_000_000)
              }
            manager
        let code = statusCode $ HTTP.responseStatus response
        unless (code >= 200 && code < 300) $ failure $ T.pack endpoint <> " returned HTTP " <> T.pack (show code)
      identity device = object ["user" .= device.user, "client" .= device.client]
      fetchPage cursor = do
        request <- HTTP.parseRequest $ base <> "/i/fan-in-perf/fetch"
        HTTP.withResponse
          request
            { HTTP.method = "POST",
              HTTP.requestBody = HTTP.RequestBodyLBS $ encode $ object ["cursor" .= cursor, "limit" .= (100 :: Int)],
              HTTP.requestHeaders = [("Content-Type", "application/json")],
              HTTP.redirectCount = 0,
              HTTP.responseTimeout = HTTP.responseTimeoutMicro (opts.timeoutSeconds * 1_000_000)
            }
          manager
          $ \response -> do
            unless (statusCode (HTTP.responseStatus response) == 200) $ failure "fetch request failed"
            body <- boundedResponse 8_388_608 [] (HTTP.responseBody response)
            rows <- either (const $ failure "invalid fetch response") pure $ eitherDecode body
            unless (length rows <= 100 && and (zipWith (<) (cursor : map fst rows) (map fst rows))) $ failure "invalid fetch page order or size"
            pure rows
      boundedResponse remaining chunks bodyReader = do
        chunk <- HTTP.brRead bodyReader
        if BS.null chunk
          then pure $ BL.fromChunks (reverse chunks)
          else
            if BS.length chunk > remaining
              then failure "oversized fetch response"
              else boundedResponse (remaining - BS.length chunk) (chunk : chunks) bodyReader
      acknowledgeHttp device nid = post "acknowledge" $ object ["user" .= device.user, "client" .= device.client, "notification" .= nid]
      connect :: forall a. Bool -> Device -> (SmokeClient -> IO a) -> IO a
      connect fetchMode device action = do
        connection <- UUID.nextRandom
        let headers = [("Z-User", BS.pack $ UUID.toString device.user), ("Z-Client", encodeUtf8 device.client), ("Z-Connection", BS.pack $ UUID.toString connection)]
            options =
              WS.defaultConnectionOptions
                { WS.connectionFramePayloadSizeLimit = WS.SizeLimit 8_388_608,
                  WS.connectionMessageDataSizeLimit = WS.SizeLimit 8_388_608
                }
        WS.runClientWith host port (if fetchMode then "/i/fan-in-perf/wakeups" else "/i/fan-in-perf/events") options headers $ \socket -> do
          let receive = do
                bytes <- WS.receiveData socket :: IO BL.ByteString
                either (const $ failure "invalid WebSocket frame") pure $ eitherDecode bytes
              acknowledge tag = WS.sendTextData socket $ encode $ object ["type" .= (if fetchMode then "caught-up" else "ack" :: Text), "tag" .= tag]
          result <- action SmokeClient {..}
          WS.sendClose socket ("smoke step complete" :: Text)
          pure result
  pure
    SmokeActions
      { register = post "register" . identity,
        initialize = post "initialize" . identity,
        publish = \payload -> post "publish" $ object ["payload" .= payload],
        withClient = connect False,
        withFetchClient = \device observe action -> connect True device $ \socket -> do
          client <- fetchingClient socket fetchPage (acknowledgeHttp device) observe
          action client
      }

runSmoke :: SmokeOptions -> IO ()
runSmoke opts = do
  result <- tryAny $ do
    actions <- networkActions opts
    runId <- UUID.nextRandom
    a <- Device <$> UUID.nextRandom <*> pure "1"
    b <- Device <$> UUID.nextRandom <*> pure "1"
    let report = putStrLn . T.unpack
        selected =
          actions
            { withClient = \device action ->
                if opts.fetchPercent == 100 || (opts.fetchPercent >= 50 && device == a)
                  then withFetchClient actions device (const $ pure ()) action
                  else withClient actions device action
            }
    -- Also bound connection setup and teardown, which enclose multiple steps.
    finished <-
      timeout (opts.timeoutSeconds * 12 * 1_000_000) $
        runScenario opts.timeoutSeconds report selected runId a b
    when (isNothing finished) $ failure "overall timeout (connection setup or teardown)"
  case result of
    Right () -> putStrLn "PASS: live delivery, independent acknowledgements and reconnect catch-up"
    Left err -> case fromException err of
      Just (SmokeFailure message) -> die $ "FAIL: " <> T.unpack message
      Nothing -> die "FAIL: service communication failed; check URLs, service logs and experimental configuration"

runScenario :: Int -> (Text -> IO ()) -> SmokeActions -> UUID -> Device -> Device -> IO ()
runScenario seconds report actions runId a b = do
  let step :: Text -> IO a -> IO a
      step label action = do
        report $ "RUN: " <> label
        outcome <- timeout (seconds * 1_000_000) (tryAny action)
        case outcome of
          Nothing -> failure $ label <> ": timeout"
          Just (Left err) -> case fromException err of
            Just (SmokeFailure detail) -> failure $ label <> ": " <> detail
            Nothing -> failure $ label <> ": service communication failed"
          Just (Right result) -> report ("OK: " <> label) $> result
      payload n = object ["fan_in_perf_smoke_run" .= runId, "sequence" .= (n :: Int)]
      own :: Value -> Maybe Int
      own payloadValue =
        parseMaybe
          ( withObject "smoke payload" $ \o -> do
              marker <- o .: "fan_in_perf_smoke_run"
              guard (marker == runId)
              o .: "sequence"
          )
          payloadValue
      drain :: SmokeClient -> IO [(Int, UUID)]
      drain socket = go Set.empty []
        where
          go seen found =
            socket.receive >>= \case
              Ready -> pure $ reverse found
              Acked _ -> failure "unexpected acknowledgement confirmation"
              Wakeup _ _ -> failure "unexpected raw wakeup"
              Events tag rows -> do
                let ids = map fst rows
                unless (Set.size (Set.fromList ids) == length ids && all (`Set.notMember` seen) ids) $ failure "duplicate notification delivery"
                socket.acknowledge tag
                confirmation <- socket.receive
                unless (confirmation == Acked tag) $ failure "missing durable acknowledgement confirmation"
                go (Set.union seen $ Set.fromList ids) (reverse [(n, nid) | (nid, payloadValue) <- rows, Just n <- [own payloadValue]] <> found)
      expect socket sequences = collect []
        where
          collect previous = do
            found <- (previous <>) <$> drain socket
            unless (map fst found == take (length found) sequences) $ failure "unexpected smoke events (replayed or out of order)"
            if length found == length sequences
              then pure $ map snd found
              else collect found
  step "register and initialize two fresh devices" $ for_ [a, b] $ \device -> actions.register device >> actions.initialize device
  withClient actions a $ \alice -> do
    withClient actions b $ \bob -> do
      step "both devices connected and caught up" $ void $ concurrently (expect alice []) (expect bob [])
      step "publish event 1" $ actions.publish $ payload 1
      step "both devices receive and acknowledge the same event" $ do
        (left, right) <- concurrently (expect alice [1]) (expect bob [1])
        unless (left == right) $ failure "devices received different notification IDs"
    report "OK: second device disconnected"
    step "publish event 2 while second device is offline" $ actions.publish $ payload 2
    delivered <- step "connected device receives and acknowledges event 2" $ expect alice [2]
    withClient actions b $ \bob -> step "reconnected device catches up without replaying event 1" $ do
      caughtUp <- expect bob [2]
      unless (delivered == caughtUp) $ failure "catch-up notification differs from live delivery"
  withClient actions a $ \alice -> withClient actions b $ \bob ->
    step "acknowledged events are not replayed after reconnect" $ void $ concurrently (expect alice []) (expect bob [])

-- | Client-driven fetch loop, exposed as pages to the shared smoke/load checks.
-- Acked is synthesized ONLY after the HTTP acknowledgement succeeds. Ready means
-- an empty HTTP fetch, not merely receipt of the WebSocket availability signal.
-- One outstanding signal and one page per device bound buffering; repeated
-- signals are coalesced by Cannon while this loop drains the stream.
fetchingClient :: SmokeClient -> (UUID -> IO [(UUID, Value)]) -> (UUID -> IO ()) -> (FetchObservation -> IO ()) -> IO SmokeClient
fetchingClient socket fetch persist observe = do
  cursor <- newIORef UUID.nil
  active <- newIORef (Nothing :: Maybe Integer)
  pending <- newIORef (Nothing :: Maybe (Integer, UUID))
  confirmed <- newIORef (Nothing :: Maybe Integer)
  nextTag <- newIORef (1 :: Integer)
  let receive =
        readIORef confirmed >>= \case
          Just tag -> writeIORef confirmed Nothing $> Acked tag
          Nothing -> do
            awaiting <- readIORef pending
            when (isJust awaiting) $ failure "fetch page must be acknowledged first"
            signal <- readIORef active
            wokeAt <- case signal of
              Just _ -> pure Nothing
              Nothing ->
                socket.receive >>= \case
                  Wakeup tag start -> do
                    now <- getMonotonicTimeNSec
                    modifyIORef' cursor (max start)
                    writeIORef active (Just tag)
                    pure (Just now)
                  _ -> failure "expected WebSocket wakeup"
            start <- getMonotonicTimeNSec
            position <- readIORef cursor
            rows <- fetch position
            end <- getMonotonicTimeNSec
            let milliseconds t = fromIntegral (end - min end t) / 1000000
            observe $ FetchObservation (length rows) (milliseconds start) (milliseconds <$> wokeAt)
            case rows of
              [] -> do
                tag <- readIORef active >>= maybe (failure "missing wakeup tag") pure
                socket.acknowledge tag
                writeIORef active Nothing
                pure Ready
              _ -> do
                tag <- readIORef nextTag
                writeIORef pending (Just (tag, fst $ last rows))
                pure (Events tag rows)
      acknowledge tag =
        readIORef pending >>= \case
          Just (expected, position) | tag == expected -> do
            persist position
            writeIORef cursor position
            writeIORef pending Nothing
            writeIORef confirmed (Just tag)
            modifyIORef' nextTag (+ 1)
          _ -> failure "invalid HTTP acknowledgement tag"
  pure SmokeClient {..}
