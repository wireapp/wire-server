-- | Internal, opt-in WebSocket adapter for the fixed benchmark epoch.
module Cannon.FanIn (API, Runtime (..), configure, server) where

import Cannon.App (terminate)
import Cannon.Options (Opts (..))
import Cannon.WS qualified as WS
import Control.Exception (finally)
import Data.Aeson (encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Id
import FanInPerf.Delivery (deliverEventsWithTiming, deliverWakeups)
import FanInPerf.Fixture
import FanInPerf.NotificationsStore
import FanInPerf.ServerTiming (ObserveAck, registerAckTimings)
import FanInPerf.Store
import FanInPerf.Wakeup
import Imports
import Network.WebSockets qualified as Socket
import Network.WebSockets.Connection qualified as Connection
import Network.WebSockets.Connection.PingPong
import Servant
import Servant.Types.SourceT (SourceT (..), StepT (..))
import UnliftIO.Exception (tryAny)
import Wire.API.Routes.Named
import Wire.API.Routes.WebSocket

-- Explicit internal routes, included in Cannon's CombinedAPI and metrics.
type API =
  "i"
    :> "fan-in-perf"
    :> ( Named
           "fan-in-perf-events"
           ( "events"
               :> Header' '[Required, Strict] "Z-User" UserId
               :> Header' '[Required, Strict] "Z-Client" ClientId
               :> Header' '[Required, Strict] "Z-Connection" ConnId
               :> WebSocketPending
           )
           :<|> Named
                  "fan-in-perf-wakeups"
                  ( "wakeups"
                      :> Header' '[Required, Strict] "Z-User" UserId
                      :> Header' '[Required, Strict] "Z-Client" ClientId
                      :> Header' '[Required, Strict] "Z-Connection" ConnId
                      :> WebSocketPending
                  )
           :<|> Named
                  "fan-in-perf-wakeup"
                  ("wakeup" :> StreamBody NoFraming WakeupJSON (SourceIO ByteString) :> PostNoContent)
       )

-- Stream the JSON bytes so the handler can enforce the 1 KiB bound before
-- decoding. Keep the existing application/json wire contract.
data WakeupJSON

instance Accept WakeupJSON where
  contentType _ = contentType (Proxy @JSON)

instance MimeUnrender WakeupJSON ByteString where
  mimeUnrender _ = Right . BL.toStrict

data Runtime = Runtime {store :: Env, hub :: WakeupHub, observeAck :: ObserveAck}

configure :: Opts -> IO (Maybe Runtime)
configure opts = for opts._optsFanInPerfPostgresql $ \config ->
  Runtime <$> configureStore config opts._optsFanInPerfPostgresqlPool opts._optsFanInPerfPostgresqlPassword <*> newWakeupHub <*> registerAckTimings

server :: Maybe Runtime -> WS.Env -> Server API
server runtime wsEnv =
  Named @"fan-in-perf-events" (events False) :<|> Named @"fan-in-perf-wakeups" (events True) :<|> Named @"fan-in-perf-wakeup" wakeup
  where
    wakeup source = case runtime of
      Nothing -> throwError err404
      Just rt -> do
        body <- liftIO $ boundedSource source
        case body of
          Nothing -> throwError err413
          Just bytes | validWakeup bytes -> liftIO (atomically $ signal rt.hub) $> NoContent
          _ -> throwError err400
    events fetchMode user client connection pending = liftIO $ case runtime of
      Nothing -> reject pending 404 "Not Found"
      Just rt -> do
        position <- tryAny $ resumePosition rt.store user client
        case position of
          Left _ -> reject pending 503 "Service Unavailable"
          Right Nothing -> reject pending 403 "Forbidden"
          Right (Just cursor) -> do
            let options =
                  (Connection.pendingOptions pending)
                    { Socket.connectionFramePayloadSizeLimit = Socket.SizeLimit 1024,
                      Socket.connectionMessageDataSizeLimit = Socket.SizeLimit 1024
                    }
            conn <- Socket.acceptRequest pending {Connection.pendingOptions = options}
            let key = WS.mkKey user connection
            ws <- WS.runWS wsEnv $ WS.mkWebSocket conn
            let session = do
                  WS.runWS wsEnv $ do
                    WS.registerLocal key ws
                    WS.registerRemote key (Just client)
                  withPingPong defaultPingPongOptions conn $ \socket ->
                    if fetchMode
                      then deliverWakeups rt.hub cursor (\value -> WS.sendMsgIO (encode value) ws) (Socket.receiveData socket)
                      else
                        deliverEventsWithTiming
                          rt.observeAck
                          rt.hub
                          cursor
                          (\after -> runOrAbort "fetch experimental events" rt.store $ fetchNotifications benchmarkStream after 100)
                          (acknowledgeWithTimings rt.observeAck rt.store user client)
                          (\value -> WS.sendMsgIO (encode value) ws)
                          (Socket.receiveData socket)
            result <- tryAny $ session `finally` WS.runWS wsEnv (terminate key ws)
            when (isLeft result) $ void $ tryAny $ Socket.sendClose conn ("experimental stream closed" :: Text)

-- WebSocketPending runs after routing but before acceptRequest; reject on the
-- pending socket rather than throwing a Handler error after the upgrade hook.
reject :: Socket.PendingConnection -> Int -> ByteString -> IO ()
reject pending status message =
  Socket.rejectRequestWith
    pending
    Socket.defaultRejectRequest
      { Socket.rejectCode = status,
        Socket.rejectMessage = message
      }

boundedSource :: SourceIO ByteString -> IO (Maybe BL.ByteString)
boundedSource source = unSourceT source (go 1024 [])
  where
    go remaining chunks = \case
      Stop -> pure $ Just $ BL.fromChunks (reverse chunks)
      Error _ -> pure Nothing
      Skip next -> go remaining chunks next
      Effect next -> next >>= go remaining chunks
      Yield chunk next
        | BS.length chunk > remaining -> pure Nothing
        | otherwise -> go (remaining - BS.length chunk) (chunk : chunks) next
