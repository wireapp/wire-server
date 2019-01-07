{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Cannon.App (wsapp, terminate, maxPingInterval) where

import Imports hiding (threadDelay)
import Cannon.WS
import Control.Concurrent.Async
import Control.Concurrent.Timeout
import Control.Monad.Catch
import Data.Aeson hiding (Error, (.=))
import Data.ByteString.Conversion
import Data.ByteString.Lazy (toStrict)
import Data.Id (ClientId)
import Data.Timeout
import Lens.Family hiding (set)
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error
import Network.WebSockets hiding (Request, Response, requestHeaders)
import System.Logger.Class hiding (Error, close)

import qualified Data.Text.Lazy as Text
import qualified System.Logger  as Logger

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

wsapp :: Key -> Maybe ClientId -> Logger -> Env -> ServerApp
wsapp k c l e pc = go `catches` ioErrors l k
  where
    go = runWS e $ do
        ws <- mkWebSocket =<< liftIO (acceptRequest pc `catch` rejectOnError pc)
        debug $ client (key2bytes k) ~~ "websocket" .= connIdent ws
        registerLocal k ws
        registerRemote k c `onException` (unregisterLocal k ws >> close k ws)
        clock <- getClock
        continue l ws clock k `finally` terminate k ws

continue :: MonadIO m => Logger -> Websocket -> Clock -> Key -> m ()
continue l ws clock k = liftIO $ do
    ttl    <- TTL . (+ maxLifetime) <$> getTime clock
    state  <- newIORef $ State 1 (20 # Minute)
    rloop  <- async (readLoop ws state)
    wloop  <- async (writeLoop ws clock ttl state)
    result <- waitEitherCatchCancel rloop wloop
    case result of
        (Left  (Left x)) ->
            let text = client (key2bytes k) . msg (val "read: " +++ show x) in
            case fromException x of
                Just ConnectionClosed -> Logger.debug l text
                _                     -> Logger.warn  l text
        (Right (Left x)) -> Logger.warn l $
            client (key2bytes k) . msg (val "write: " +++ show x)
        _                -> return ()

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
        if | s^.counter == 0 -> do
                set counter st succ
                threadDelay $ s^.pingFreq
                keepAlive
           | s^.counter < 3 -> do
                set counter st succ
                send (connection ws) ping
                threadDelay $ (10 # Second) `min` (s^.pingFreq)
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
            ControlMessage (Ping p)  -> do
                adjustPingFreq p
                reset counter s 0
                send (connection ws) (pong p)
                loop
            ControlMessage (Close _ _) -> return ()
            _ -> do
                reset counter s 0
                sendAppLevelPong
                loop

    adjustPingFreq p = case fromByteString (toStrict p) of
        Just i | i > 0 && i < maxPingInterval -> reset pingFreq s (i # Second)
        _                                     -> return ()

    -- control messages are internal to the browser that manages the websockets
    -- <https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API/Writing_WebSocket_servers#Pings_and_Pongs_The_Heartbeat_of_WebSockets>.
    -- since the browser may silently lose a websocket connection, wire clients are allowed send
    -- 'DataMessage' pings as well, and we respond with a 'DataMessage' pong to allow them to
    -- reliably decide whether the connection is still alive.
    sendAppLevelPong = sendMsgIO "pong" ws

rejectOnError :: PendingConnection -> HandshakeException -> IO a
rejectOnError p x = do
    let f lb mg = toStrict . encode $ Error status400 lb mg
    case x of
        NotSupported              -> rejectRequest p (f "protocol not supported" "N/A")
        MalformedRequest _ m      -> rejectRequest p (f "malformed-request" (Text.pack m))
        OtherHandshakeException m -> rejectRequest p (f "other-error" (Text.pack m))
        _                         -> throwM x
    throwM x

ioErrors :: MonadIO m => Logger -> Key -> [Handler m ()]
ioErrors l k = let f s = Logger.err l $ client (key2bytes k) . msg s in
    [ Handler $ \(x :: HandshakeException) -> f (show x)
    , Handler $ \(x :: IOException)        -> f (show x)
    ]

ping :: Message
ping = ControlMessage (Ping "ping")

pong :: LByteString -> Message
pong = ControlMessage . Pong

set :: ASetter' a b -> IORef a -> (b -> b) -> IO ()
set l v g = atomicModifyIORef' v $ \s -> (over l g s, ())

reset :: ASetter' a b -> IORef a -> b -> IO ()
reset f v a = set f v (const a)
