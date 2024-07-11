{-# OPTIONS_GHC -Wwarn #-}

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

module Cannon.App
  ( wsapp,
    terminate,
    maxPingInterval,
  )
where

import Cannon.Options
import Cannon.WS
import Control.Concurrent.Async
import Control.Concurrent.Timeout
import Control.Monad.Catch
import Data.Aeson hiding (Error, Key, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Conversion
import Data.ByteString.Lazy (toStrict)
import Data.Id (ClientId, UserId)
import Data.Map qualified as Map
import Data.Text as T
import Data.Text.Encoding as T
import Data.Text.Lazy qualified as LT
import Data.Timeout
import Debug.Trace
import Imports hiding (threadDelay)
import Lens.Family hiding (reset, set)
import Network.AMQP qualified as Q
import Network.AMQP.Extended qualified as Q
import Network.AMQP.Types qualified as Q
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error
import Network.WebSockets hiding (Request, Response, requestHeaders)
import System.Logger.Class hiding (Error, close)
import System.Logger.Class qualified as Logger

-- | Connection state, updated by {read, write}Loop.
data State = State !Int !Timeout

-- | The lifetime of a websocket.
newtype TTL = TTL Word64

counter :: (Functor f) => LensLike' f State Int
counter f (State c p) = (\x -> State x p) `fmap` f c
{-# INLINE counter #-}

pingFreq :: (Functor f) => LensLike' f State Timeout
pingFreq f (State c p) = (\x -> State c x) `fmap` f p
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

routingKey :: UserId -> Text
routingKey uid = T.decodeUtf8 ("client-notifications." <> toByteString' uid)

ensureNotifStream :: Q.Channel -> Env -> UserId -> IO ()
ensureNotifStream chan e uid = do
  let ttlSeconds = e.wsNotificationTTL
      qOpts =
        Q.newQueue
          { Q.queueName = routingKey uid,
            Q.queueHeaders =
              Q.FieldTable $
                Map.fromList
                  [ ("x-queue-type", (Q.FVString "stream")),
                    ("x-max-age", (Q.FVString $ T.encodeUtf8 $ T.pack $ show ttlSeconds <> "s"))
                  ]
          }
  void $ liftIO $ Q.declareQueue chan qOpts

data RabbitmqMessage = MkRabbitmqMessage
  { event :: Value,
    targetClients :: [ClientId]
  }

instance FromJSON RabbitmqMessage where
  parseJSON = withObject "RabbitmqMessage" $ \obj -> do
    MkRabbitmqMessage
      <$> obj .: "event"
      <*> obj .: "target_clients"

wsapp :: Key -> UserId -> Maybe ClientId -> Env -> Q.RabbitMqOpts -> ServerApp
wsapp k uid c e rabbitmqOpts pc = do
  wsVar <- newEmptyMVar

  -- create rabbitmq consumer
  -- chan <- readMVar e.rabbitmqChannel
  chan <- Q.mkRabbitMqChannelMVar e.logg rabbitmqOpts >>= readMVar
  Q.qos chan 0 1 False
  threadDelay 1000000
  traceM "got channel"
  ensureNotifStream chan e uid
  consumerTag <- Q.consumeMsgs chan (routingKey uid) Q.Ack $ \(message, envelope) -> do
    catch
      ( do
          traceM $ "rabbitmq message: " <> show message.msgBody
          traceM $ "message headers: " <> show message.msgHeaders
          notif <- case Aeson.eitherDecode message.msgBody of
            Left errMsg -> error $ "failed parsing rabbitmq message: " <> errMsg
            Right (body :: RabbitmqMessage) -> do
              pure $
                Aeson.encode $
                  object
                    [ "payload" Aeson..= body.event
                    ]
          traceM $ "notif: " <> show notif
          ws <- readMVar wsVar
          runWS e $ sendMsg notif ws
          Q.ackMsg chan envelope.envDeliveryTag False
      )
      $ \(e :: SomeException) -> do
        traceM $ "exception in rabbitmq handler: " <> displayException e

  -- traceM $ "envelope: " <> show envelope
  traceM $ "tag: " <> show consumerTag

  let go = do
        ws <- mkWebSocket =<< liftIO (acceptRequest pc `catch` rejectOnError pc)
        putMVar wsVar ws
        debug $ client (key2bytes k) ~~ "websocket" .= connIdent ws
        registerLocal k ws
        -- registerRemote k c `onException` (unregisterLocal k ws >> close k ws)
        clock <- getClock
        continue ws clock k `finally` terminate k ws (chan, consumerTag)

  -- start websocket app
  runWS e (go `catches` ioErrors k)

continue :: (MonadLogger m, MonadUnliftIO m) => Websocket -> Clock -> Key -> m ()
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
      _ -> pure ()

terminate :: Key -> Websocket -> (Q.Channel, Q.ConsumerTag) -> WS ()
terminate k ws (chan, consumerTag) = do
  liftIO $ Q.cancelConsumer chan consumerTag
  success <- unregisterLocal k ws
  debug $ client (key2bytes k) ~~ "websocket" .= connIdent ws ~~ "removed" .= success
  when success $
    close k ws `catchAll` const (pure ())

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
        | otherwise -> pure ()
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
        ControlMessage (Close _ _) -> pure ()
        perhapsPingMsg -> do
          reset counter s 0
          when (isAppLevelPing perhapsPingMsg) sendAppLevelPong
          loop
    adjustPingFreq p = case fromByteString (toStrict p) of
      Just i | i > 0 && i < maxPingInterval -> reset pingFreq s (i # Second)
      _ -> pure ()
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
    NotSupported -> rejectRequest p (f "protocol not supported" "N/A")
    MalformedRequest _ m -> rejectRequest p (f "malformed-request" (LT.pack m))
    OtherHandshakeException m -> rejectRequest p (f "other-error" (LT.pack m))
    _ -> pure ()
  throwM x

ioErrors :: (MonadLogger m) => Key -> [Handler m ()]
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
