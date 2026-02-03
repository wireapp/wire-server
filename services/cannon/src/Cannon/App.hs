-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Cannon.App where

import Cannon.WS
import Control.Concurrent.Async
import Control.Monad.Catch
import Data.Aeson hiding (Error, Key, (.=))
import Data.ByteString.Lazy (toStrict)
import Data.Id
import Data.Text.Lazy qualified as Text
import Imports hiding (threadDelay)
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error
import Network.WebSockets hiding (Request, Response, requestHeaders)
import System.Logger.Class hiding (Error, close)
import System.Logger.Class qualified as Logger
import UnliftIO (timeout)

-- | Maximum lifetime of a websocket in seconds.
maxLifetime :: Int
maxLifetime = 3 * 24 * 3600

wsapp :: Key -> Maybe ClientId -> Env -> ServerApp
wsapp k c e pc = runWS e (go `catches` ioErrors k)
  where
    go = do
      runInIO <- askRunInIO
      conn0 <- liftIO (acceptRequest pc `catch` rejectOnError pc)
      liftIO . withPingPong defaultPingPongOptions conn0 $ \conn -> runInIO $ do
        ws <- mkWebSocket conn
        debug $ client (key2bytes k) ~~ "websocket" .= connIdent ws
        registerLocal k ws
        registerRemote k c `onException` (unregisterLocal k ws >> close k ws)
        timeout (maxLifetime * 1_000_000) (continue ws k) `finally` terminate k ws >>= \case
          Nothing ->
            Logger.info $ msg (val "websocket reached max lifetime") . client (key2bytes k)
          Just () -> pure ()

continue :: (MonadLogger m, MonadUnliftIO m) => Websocket -> Key -> m ()
continue ws k = do
  runInIO <- askRunInIO
  liftIO $ do
    rloop <- async (readLoop ws)
    result <- waitCatch rloop
    case result of
      (Left x) ->
        let text = client (key2bytes k) . msg (val "error in read loop") . field "error" (show x)
         in runInIO $ Logger.warn text
      _ -> pure ()

terminate :: Key -> Websocket -> WS ()
terminate k ws = do
  success <- unregisterLocal k ws
  debug $ client (key2bytes k) ~~ "websocket" .= connIdent ws ~~ "removed" .= success
  when success $
    close k ws `catchAll` const (pure ())

readLoop :: Websocket -> IO ()
readLoop ws = loop
  where
    loop = do
      m <- receive (connection ws)
      case m of
        ControlMessage (Close _ _) -> pure ()
        perhapsPingMsg -> do
          when (isAppLevelPing perhapsPingMsg) sendAppLevelPong
          loop
    -- control messages are internal to the browser that manages the websockets
    -- <https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API/Writing_WebSocket_servers#Pings_and_Pongs_The_Heartbeat_of_WebSockets>.
    -- since the browser may silently lose a websocket connection, wire clients are allowed send
    -- 'DataMessage' pings as well, and we respond with a 'DataMessage' pong to allow them to
    -- reliably decide whether the connection is still alive.
    isAppLevelPing = \case
      (DataMessage _ _ _ (Text "ping" _)) -> True
      (DataMessage _ _ _ (Binary "ping")) -> True
      _ -> False
    sendAppLevelPong = sendMsgIO @ByteString "pong" ws

rejectOnError :: PendingConnection -> HandshakeException -> IO a
rejectOnError p x = do
  let f lb mg = toStrict . encode $ mkError status400 lb mg
  case x of
    NotSupported -> rejectRequest p (f "protocol not supported" defRequestId)
    MalformedRequest _ m -> rejectRequest p (f "malformed-request" (Text.pack m))
    OtherHandshakeException m -> rejectRequest p (f "other-error" (Text.pack m))
    _ -> pure ()
  throwM x

ioErrors :: (MonadLogger m) => Key -> [Handler m ()]
ioErrors k =
  let f s = Logger.err $ client (key2bytes k) . msg s
   in [ Handler $ \(x :: HandshakeException) -> f (displayException x),
        Handler $ \(x :: IOException) -> f (displayException x)
      ]
