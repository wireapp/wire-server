-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Cannon.App
  ( wsapp,
    terminate,
    maxPingInterval,
  )
where

import Cannon.WS
import Control.Concurrent.Async
import Control.Concurrent.Timeout
import Control.Monad.Catch
import Data.Aeson hiding (Error, (.=))
import Data.ByteString.Conversion
import Data.ByteString.Lazy (toStrict)
import Data.Id (ClientId)
import qualified Data.Text.Lazy as Text
import Data.Timeout
import Imports hiding (threadDelay)
import Lens.Family hiding (set)
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error
import Network.WebSockets hiding (Request, Response, requestHeaders)
import System.Logger.Class hiding (Error, close)
import qualified System.Logger.Class as Logger

-- | Connection state, updated by {read, write}Loop.
data State = State !Int !Timeout

-- | The lifetime of a websocket.
newtype TTL = TTL Word64

counter :: Functor f => LensLike' f State Int
counter f (State c p) = (\x -> State x p) `fmap` (f c)
{-# INLINE counter #-}

pingFreq :: Functor f => LensLike' f State Timeout
pingFreq f (State c p) = (\x -> State c x) `fmap` (f p)
{-# INLINE pingFreq #-}

-- | Maximum ping interval in seconds. The ping interval controls
-- the frequency at which the server pings the client and can be
-- modified by the client.
maxPingInterval :: Word64
maxPingInterval = 3600

-- | Maximum lifetime of a websocket in seconds.
-- The effective maximum lifetime is @maxLifetime + maxPingInterval@.
maxLifetime :: Word64
maxLifetime = 3 * 24 * 3600

wsapp :: Key -> Maybe ClientId -> Env -> ServerApp
wsapp k c e pc = runWS e (go `catches` ioErrors k)
  where
    go = do
      ws <- mkWebSocket =<< liftIO (acceptRequest pc `catch` rejectOnError pc)
      debug $ client (key2bytes k) ~~ "websocket" .= connIdent ws
      registerLocal k ws
      registerRemote k c `onException` (unregisterLocal k ws >> close k ws)
      clock <- getClock
      continue ws clock k `finally` terminate k ws

continue :: (MonadLogger m, MonadUnliftIO m, MonadIO m) => Websocket -> Clock -> Key -> m ()
continue ws clock k = do
  runInIO <- askRunInIO
  liftIO $ do
    ttl <- TTL . (+ maxLifetime) <$> getTime clock
    state <- newIORef $ State 1 (20 # Minute)
    rloop <- async (readLoop ws state)
    wloop <- async (writeLoop ws clock ttl state)
    result <- waitEitherCatchCancel rloop wloop
    case result of
      (Left (Left x)) ->
        let text = client (key2bytes k) . msg (val "read: " +++ show x)
         in runInIO $ Logger.debug text
      (Right (Left x)) ->
        let text = client (key2bytes k) . msg (val "write: " +++ show x)
         in runInIO $ Logger.debug text
      _ -> return ()

terminate :: Key -> Websocket -> WS ()
terminate k ws = do
  success <- unregisterLocal k ws
  debug $ client (key2bytes k) ~~ "websocket" .= connIdent ws ~~ "removed" .= success
  when success $
    close k ws `catchAll` const (return ())

writeLoop :: Websocket -> Clock -> TTL -> IORef State -> IO ()
writeLoop ws clock (TTL ttl) st = loop
  where
    loop = do
      s <- readIORef st
      if
          | s ^. counter == 0 -> do
            set counter st succ
            threadDelay $ s ^. pingFreq
            keepAlive
          | s ^. counter < 3 -> do
            set counter st succ
            send (connection ws) ping
            threadDelay $ (10 # Second) `min` (s ^. pingFreq)
            keepAlive
          | otherwise -> return ()
    keepAlive = do
      time <- getTime clock
      unless (time > ttl) loop

readLoop :: Websocket -> IORef State -> IO ()
readLoop ws s = loop
  where
    loop = do
      m <- receive (connection ws)
      case m of
        ControlMessage (Ping p) -> do
          adjustPingFreq p
          reset counter s 0
          send (connection ws) (pong p)
          loop
        ControlMessage (Close _ _) -> return ()
        perhapsPingMsg -> do
          reset counter s 0
          when (isAppLevelPing perhapsPingMsg) sendAppLevelPong
          loop
    adjustPingFreq p = case fromByteString (toStrict p) of
      Just i | i > 0 && i < maxPingInterval -> reset pingFreq s (i # Second)
      _ -> return ()
    -- control messages are internal to the browser that manages the websockets
    -- <https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API/Writing_WebSocket_servers#Pings_and_Pongs_The_Heartbeat_of_WebSockets>.
    -- since the browser may silently lose a websocket connection, wire clients are allowed send
    -- 'DataMessage' pings as well, and we respond with a 'DataMessage' pong to allow them to
    -- reliably decide whether the connection is still alive.
    isAppLevelPing = \case
      (DataMessage _ _ _ (Text "ping" _)) -> True
      (DataMessage _ _ _ (Binary "ping")) -> True
      _ -> False
    sendAppLevelPong = sendMsgIO "pong" ws

rejectOnError :: PendingConnection -> HandshakeException -> IO a
rejectOnError p x = do
  let f lb mg = toStrict . encode $ Error status400 lb mg
  case x of
    NotSupported -> rejectRequest p (f "protocol not supported" "N/A")
    MalformedRequest _ m -> rejectRequest p (f "malformed-request" (Text.pack m))
    OtherHandshakeException m -> rejectRequest p (f "other-error" (Text.pack m))
    _ -> pure ()
  throwM x

ioErrors :: (MonadLogger m, MonadIO m) => Key -> [Handler m ()]
ioErrors k =
  let f s = Logger.err $ client (key2bytes k) . msg s
   in [ Handler $ \(x :: HandshakeException) -> f (show x),
        Handler $ \(x :: IOException) -> f (show x)
      ]

ping :: Message
ping = ControlMessage (Ping "ping")

pong :: LByteString -> Message
pong = ControlMessage . Pong

set :: ASetter' a b -> IORef a -> (b -> b) -> IO ()
set l v g = atomicModifyIORef' v $ \s -> (over l g s, ())

reset :: ASetter' a b -> IORef a -> b -> IO ()
reset f v a = set f v (const a)
