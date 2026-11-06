-- | Internal, opt-in WebSocket adapter for the fixed benchmark epoch.
module Cannon.FanIn (configure) where

import Cannon.App (terminate)
import Cannon.Options (Opts (..))
import Cannon.WS qualified as WS
import Control.Exception (finally)
import Data.Aeson (encode)
import Data.ByteString.Conversion (fromByteString)
import Data.Id
import FanInPerf.Delivery (deliverEvents)
import FanInPerf.Fixture
import FanInPerf.NotificationsStore
import FanInPerf.Store
import FanInPerf.Wakeup
import Imports
import Network.HTTP.Types
import Network.Wai qualified as Wai
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets qualified as Socket
import Network.WebSockets.Connection.PingPong
import UnliftIO.Exception (tryAny)

configure :: Opts -> WS.Env -> IO Wai.Middleware
configure opts wsEnv = case opts._optsPostgresql of
  Nothing -> pure id
  Just config -> do
    store <- configureStore config opts._optsPostgresqlPool opts._optsPostgresqlPassword
    hub <- newWakeupHub
    let events fallback request respond
          | Wai.pathInfo request /= ["i", "fan-in-perf", "events"] = fallback request respond
          | otherwise = case identity request of
              Nothing -> respond $ Wai.responseLBS status400 [] ""
              Just (user, client, connection) -> do
                position <- tryAny $ resumePosition store user client
                case position of
                  Left _ -> respond $ Wai.responseLBS status503 [] ""
                  Right Nothing -> respond $ Wai.responseLBS status403 [] ""
                  Right (Just cursor) ->
                    websocketsOr
                      socketOptions
                      ( \pending -> do
                          conn <- Socket.acceptRequest pending
                          let key = WS.mkKey user connection
                          ws <- WS.runWS wsEnv $ WS.mkWebSocket conn
                          let session = do
                                WS.runWS wsEnv $ do
                                  WS.registerLocal key ws
                                  WS.registerRemote key (Just client)
                                withPingPong defaultPingPongOptions conn $ \socket ->
                                  deliverEvents
                                    hub
                                    cursor
                                    (\after -> runOrAbort "fetch experimental events" store $ fetchNotifications benchmarkStream after 100)
                                    (\nid -> runOrAbort "acknowledge experimental events" store $ ackNotification user client benchmarkStream nid)
                                    (\value -> WS.sendMsgIO (encode value) ws)
                                    (Socket.receiveData socket)
                          result <- tryAny $ session `finally` WS.runWS wsEnv (terminate key ws)
                          -- Never expose database or connection details to clients.
                          when (isLeft result) $ void $ tryAny $ Socket.sendClose conn ("experimental stream closed" :: Text)
                      )
                      (\_ reply -> reply $ Wai.responseLBS (mkStatus 426 "Upgrade Required") [] "")
                      request
                      respond
    pure $ wakeupMiddleware hub . events
  where
    identity request = do
      let header name = lookup name (Wai.requestHeaders request)
      user <- header "Z-User" >>= fromByteString
      client <- header "Z-Client" >>= fromByteString
      connection <- header "Z-Connection" >>= fromByteString
      pure (user :: UserId, client :: ClientId, connection :: ConnId)
    socketOptions =
      Socket.defaultConnectionOptions
        { Socket.connectionFramePayloadSizeLimit = Socket.SizeLimit 1024,
          Socket.connectionMessageDataSizeLimit = Socket.SizeLimit 1024
        }
