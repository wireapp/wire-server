-- | Experimental, internal-only Gundeck adapter for the benchmark store.
-- This adapter does not manage the schema or send native pushes.
module FanInPerf.Service
  ( configureService,
    serviceMiddleware,
    Request (..),
    executeRequest,
    executeWithWakeup,
    wakeupPayload,
  )
where

import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Id
import Data.UUID (UUID)
import Data.Vector qualified as V
import FanInPerf.Fixture
import FanInPerf.NotificationsStore
import FanInPerf.Store
import Hasql.Pool.Extended (PoolConfig)
import Hasql.TH
import Imports
import Network.HTTP.Types
import Network.HTTP.Types.Header (hAllow)
import Network.Wai qualified as Wai
import UnliftIO.Exception (tryAny)
import Util.Options (FilePathSecrets)
import Wire.API.Push.V2 (Route (RouteDirect))
import Wire.Postgres (runStatement)

-- All requests address one synthetic epoch in the isolated benchmark fixture.
data Request
  = Publish Object
  | Register UserId ClientId
  | Fetch UUID Int
  | Initialize UserId ClientId
  | Acknowledge UserId ClientId UUID
  deriving (Eq, Show)

-- The URL selects the operation; each endpoint parses only its own body.
requestParser :: Text -> Value -> Parser Request
requestParser endpoint = withObject "benchmark request" $ \o ->
  case endpoint of
    "publish" -> Publish <$> o .: "payload"
    "fetch" -> do
      cursor <- o .: "cursor"
      limit <- o .: "limit"
      unless (limit >= 1 && limit <= 1000) $ fail "limit must be between 1 and 1000"
      pure (Fetch cursor limit)
    "register" -> Register <$> o .: "user" <*> o .: "client"
    "initialize" -> Initialize <$> o .: "user" <*> o .: "client"
    "acknowledge" -> Acknowledge <$> o .: "user" <*> o .: "client" <*> o .: "notification"
    _ -> fail "unknown endpoint"

executeRequest :: Env -> Request -> IO (Either () Value)
executeRequest env request = do
  result <- runStore env $ case request of
    Publish payload -> do
      pushViaFanIn
        FanInPush
          { conn = Nothing,
            transient = False,
            route = RouteDirect,
            nativePriority = Nothing,
            origin = Nothing,
            targets = [TargetEpoch (benchmarkGroup, benchmarkEpoch)],
            json = payload,
            apsData = Nothing,
            isCellsEvent = False
          }
      pure Null
    Register user client -> do
      runStatement
        (user.toUUID, fromIntegral client.clientToWord64 :: Int64)
        [resultlessStatement|
        insert into fan_in_perf_participants (user_id, client_id)
        values ($1 :: uuid, $2 :: int8) on conflict do nothing
      |]
      pure Null
    Fetch cursor limit -> toJSON <$> fetchNotifications stream cursor limit
    Initialize user client -> toJSON <$> initAcks user client stream
    Acknowledge user client notification -> ackNotification user client stream notification $> Null
  pure $ either (const (Left ())) Right result
  where
    stream = StreamEpoch benchmarkGroup benchmarkEpoch

-- | A wakeup identifies the shared stream; event payloads stay in PostgreSQL.
wakeupPayload :: Value
wakeupPayload = object ["group" .= benchmarkGroup, "epoch" .= benchmarkEpoch]

-- | Publish a signal only after a successful write. Do not retry the write if
-- signalling fails: it may already be persisted and a retry would duplicate it.
executeWithWakeup :: (Request -> IO (Either () Value)) -> IO () -> Request -> IO (Either () Value)
executeWithWakeup store wakeup request = do
  result <- store request
  case (request, result) of
    (Publish _, Right value) -> do
      sent <- tryAny wakeup
      pure $ either (const $ Left ()) (const $ Right value) sent
    _ -> pure result

-- | Disabled unless explicitly configured. Secrets never appear in errors.
-- Schema setup stays with the benchmark's reset-db command, never service startup.
configureService :: Maybe (Map Text Text) -> Maybe PoolConfig -> Maybe FilePathSecrets -> IO () -> IO (Wai.Middleware, IO [(UserId, ClientId)])
configureService Nothing _ _ _ = pure (id, pure [])
configureService (Just config) poolOptions passwordFile wakeup = do
  env <- configureStore config poolOptions passwordFile
  let participants = runOrAbort "load benchmark participants" env $ do
        rows <-
          runStatement
            ()
            [vectorStatement|
          select user_id :: uuid, client_id :: int8 from fan_in_perf_participants
        |]
        pure [(Id u, ClientId (fromIntegral c)) | (u, c) <- V.toList rows]
  pure (serviceMiddleware (executeWithWakeup (executeRequest env) wakeup), participants)

serviceMiddleware :: (Request -> IO (Either () Value)) -> Wai.Middleware
serviceMiddleware execute fallback req respond =
  case Wai.pathInfo req of
    ["i", "fan-in-perf", endpoint]
      | endpoint `elem` ["publish", "fetch", "initialize", "acknowledge", "register"] ->
          if Wai.requestMethod req /= methodPost
            then respond $ Wai.responseLBS status405 [(hAllow, "POST")] ""
            else do
              body <- boundedBody 65536 req
              case body of
                Nothing -> reply status413 Null
                Just bytes -> case eitherDecode bytes >>= parseEither (requestParser endpoint) of
                  Left _ -> reply status400 Null
                  Right command -> execute command >>= either (const $ reply status503 Null) (reply status200)
    _ -> fallback req respond
  where
    reply status value = respond $ Wai.responseLBS status [(hContentType, "application/json")] (encode value)

boundedBody :: Int -> Wai.Request -> IO (Maybe BL.ByteString)
boundedBody maximumBytes req = go maximumBytes []
  where
    go remaining chunks = do
      chunk <- Wai.getRequestBodyChunk req
      if BS.null chunk
        then pure $ Just $ BL.fromChunks (reverse chunks)
        else
          if BS.length chunk > remaining
            then pure Nothing
            else go (remaining - BS.length chunk) (chunk : chunks)
