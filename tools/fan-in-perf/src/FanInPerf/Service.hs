-- | Experimental, internal-only Gundeck adapter for the benchmark store.
-- No schema changes or external pushes are performed by this adapter.
module FanInPerf.Service
  ( configureService,
    serviceMiddleware,
    Request (..),
    executeRequest,
  )
where

import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Id
import Data.Misc (Duration (..))
import Data.Qualified (toLocalUnsafe)
import Data.UUID (UUID)
import FanInPerf.NotificationsStore
import FanInPerf.Store
import FanInPerf.Targets (localDomain)
import Hasql.Pool.Extended (PoolConfig (..), initPostgresPool)
import Hasql.Transaction.Sessions (IsolationLevel (ReadCommitted))
import Imports
import Network.HTTP.Types
import Network.HTTP.Types.Header (hAllow)
import Network.Wai qualified as Wai
import Util.Options (FilePathSecrets)
import Wire.API.MLS.Epoch
import Wire.API.MLS.Group
import Wire.API.Push.V2 (Route (RouteDirect))

-- All requests address one synthetic epoch in the isolated benchmark fixture.
data Request
  = Publish Object
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
    "initialize" -> Initialize <$> o .: "user" <*> o .: "client"
    "acknowledge" -> Acknowledge <$> o .: "user" <*> o .: "client" <*> o .: "notification"
    _ -> fail "unknown endpoint"

benchmarkGroup :: GroupId
benchmarkGroup = GroupId "fan-in-perf-experimental"

benchmarkEpoch :: Epoch
benchmarkEpoch = Epoch 1

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
    Fetch cursor limit -> toJSON <$> fetchNotifications stream cursor limit
    Initialize user client -> toJSON <$> initAcks user client stream
    Acknowledge user client notification -> ackNotification user client stream notification $> Null
  pure $ either (const (Left ())) Right result
  where
    stream = StreamEpoch benchmarkGroup benchmarkEpoch

-- | Disabled unless explicitly configured. Secrets never appear in errors.
-- Schema setup stays with the benchmark's reset-db command, never service startup.
configureService :: Maybe (Map Text Text) -> Maybe PoolConfig -> Maybe FilePathSecrets -> IO Wai.Middleware
configureService Nothing _ _ = pure id
configureService (Just config) poolOptions passwordFile = do
  let poolConfig = fromMaybe (PoolConfig 32 (Duration 5) (Duration 600)) poolOptions
  pool <- initPostgresPool poolConfig config passwordFile
  let env = Env pool (toLocalUnsafe localDomain ()) ReadCommitted
  pure $ serviceMiddleware (executeRequest env)

serviceMiddleware :: (Request -> IO (Either () Value)) -> Wai.Middleware
serviceMiddleware execute fallback req respond =
  case Wai.pathInfo req of
    ["i", "fan-in-perf", endpoint]
      | endpoint `elem` ["publish", "fetch", "initialize", "acknowledge"] ->
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
