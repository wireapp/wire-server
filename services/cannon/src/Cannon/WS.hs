{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

module Cannon.WS
  ( Env,
    WS,
    env,
    runWS,
    drain,
    close,
    mkWebSocket,
    setRequestId,
    registerLocal,
    unregisterLocal,
    isRemoteRegistered,
    registerRemote,
    sendMsgIO,
    Clock,
    mkClock,
    getClock,
    getTime,
    Websocket,
    connection,
    connIdent,
    Key,
    mkKey,
    key2bytes,
    client,
    sendMsg,
  )
where

import Bilge hiding (trace)
import Bilge.RPC
import Bilge.Retry
import Cannon.Dict (Dict)
import qualified Cannon.Dict as D
import Cannon.Options (DrainOpts, gracePeriodSeconds, millisecondsBetweenBatches, minBatchSize)
import Conduit
import Control.Concurrent.Timeout
import Control.Lens ((^.))
import Control.Monad.Catch
import Control.Retry
import Data.Aeson hiding (Error, Key)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy as L
import Data.Default (def)
import Data.Hashable
import Data.Id (ClientId, ConnId (..), UserId)
import Data.List.Extra (chunksOf)
import Data.Text.Encoding (decodeUtf8)
import Data.Timeout (TimeoutUnit (..), (#))
import Gundeck.Types
import Imports hiding (threadDelay)
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error
import Network.WebSockets hiding (Request)
import qualified System.Logger as Logger
import System.Logger.Class hiding (Error, Settings, close, (.=))
import System.Random.MWC (GenIO, uniform)
import UnliftIO (pooledMapConcurrentlyN_, race_)
import UnliftIO.Async (async)

-----------------------------------------------------------------------------
-- Key

newtype Key = Key
  { _key :: (ByteString, ByteString)
  }
  deriving (Eq, Show, Hashable)

mkKey :: UserId -> ConnId -> Key
mkKey u c = Key (toByteString' u, fromConnId c)

key2bytes :: Key -> ByteString
key2bytes (Key (u, c)) = u <> "." <> c

keyUserBytes :: Key -> ByteString
keyUserBytes = fst . _key

keyConnBytes :: Key -> ByteString
keyConnBytes = snd . _key

-----------------------------------------------------------------------------
-- Websocket

data Websocket = Websocket
  { connection :: Connection,
    connIdent :: !Word
  }

mkWebSocket :: Connection -> WS Websocket
mkWebSocket c = do
  g <- WS $ asks rand
  Websocket c <$> liftIO (uniform g)

-----------------------------------------------------------------------------
-- Clock

-- | A clock that counts the number of seconds since its creation
-- to measure the uptime or lifetime of websockets.
newtype Clock = Clock (IORef Word64)

mkClock :: IO Clock
mkClock = do
  r <- newIORef 0
  void . forkIO . forever $ do
    threadDelay (1 # Second)
    modifyIORef' r (+ 1)
  return $ Clock r

getClock :: WS Clock
getClock = WS $ asks clock

-- | Get the number of seconds elapsed since the clock was created.
getTime :: Clock -> IO Word64
getTime (Clock r) = readIORef r

-----------------------------------------------------------------------------
-- WS Monad

data Env = Env
  { externalHostname :: !ByteString,
    portnum :: !Word16,
    upstream :: !Request,
    reqId :: !RequestId,
    logg :: !Logger,
    manager :: !Manager,
    dict :: !(Dict Key Websocket),
    rand :: !GenIO,
    clock :: !Clock,
    drainOpts :: DrainOpts
  }

setRequestId :: RequestId -> Env -> Env
setRequestId rid e = e {reqId = rid}

newtype WS a = WS
  { _conn :: ReaderT Env IO a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadThrow,
      MonadCatch,
      MonadMask,
      MonadReader Env,
      MonadUnliftIO
    )

instance MonadLogger WS where
  log l m = WS $ do
    g <- asks logg
    r <- field "request" . unRequestId <$> asks reqId
    liftIO $ Logger.log g l (r . m)

instance MonadHttp WS where
  handleRequestWithCont req handler = do
    manager <- asks manager
    liftIO $ withResponse req manager handler

instance HasRequestId WS where
  getRequestId = WS $ asks reqId

env ::
  ByteString ->
  Word16 ->
  ByteString ->
  Word16 ->
  Logger ->
  Manager ->
  Dict Key Websocket ->
  GenIO ->
  Clock ->
  DrainOpts ->
  Env
env leh lp gh gp = Env leh lp (host gh . port gp $ empty) def

runWS :: MonadIO m => Env -> WS a -> m a
runWS e m = liftIO $ runReaderT (_conn m) e

registerLocal :: Key -> Websocket -> WS ()
registerLocal k c = do
  trace $ client (key2bytes k) . msg (val "register")
  d <- WS $ asks dict
  D.insert k c d

unregisterLocal :: Key -> Websocket -> WS Bool
unregisterLocal k c = do
  trace $ client (key2bytes k) . msg (val "unregister")
  d <- WS $ asks dict
  D.removeIf (maybe False ((connIdent c ==) . connIdent)) k d

registerRemote :: Key -> Maybe ClientId -> WS ()
registerRemote k c = do
  let kb = key2bytes k
  debug $ client kb . msg (val "register-remote")
  e <- WS ask
  i <- regInfo k c
  void $
    recovering retry3x rpcHandlers $
      const $
        rpc' "gundeck" (upstream e) (method POST . path "/i/presences" . i . expect2xx)
  debug $ client kb . msg (val "registered")

isRemoteRegistered :: UserId -> ConnId -> WS Bool
isRemoteRegistered u c = do
  e <- WS ask
  rs <-
    recovering retry3x rpcHandlers $
      const $
        rpc' "gundeck" (upstream e) (method GET . paths ["/i/presences", toByteString' u] . expect2xx)
  cs <- map connId <$> parseResponse (mkError status502 "server-error") rs
  return $ c `elem` cs

sendMsgIO :: (WebSocketsData a) => a -> Websocket -> IO ()
sendMsgIO m c =
  recoverAll retry3x $ const $ sendBinaryData (connection c) m

sendMsg :: (WebSocketsData a) => a -> Key -> Websocket -> WS ()
sendMsg message k c = do
  traceLog message
  liftIO $ sendMsgIO message c
  where
    traceLog :: (WebSocketsData a) => a -> WS ()
    traceLog m = trace $ client kb . msg (logMsg m)

    logMsg :: (WebSocketsData a) => a -> Builder
    logMsg m = val "sendMsgConduit: \"" +++ L.take 128 (toLazyByteString m) +++ val "...\""

    kb = key2bytes k

drain :: WS ()
drain = do
  opts <- asks drainOpts
  websockets <- asks dict
  numberOfConns <- fromIntegral <$> D.size websockets
  let maxNumberOfBatches = (opts ^. gracePeriodSeconds * 1000) `div` (opts ^. millisecondsBetweenBatches)
      computedBatchSize = numberOfConns `div` maxNumberOfBatches
      batchSize = max (opts ^. minBatchSize) computedBatchSize
  conns <- D.toList websockets
  info $
    msg (val "draining all websockets")
      . field "numberOfConns" numberOfConns
      . field "computedBatchSize" computedBatchSize
      . field "minBatchSize" (opts ^. minBatchSize)
      . field "batchSize" batchSize
      . field "maxNumberOfBatches" maxNumberOfBatches
  let drainAction = do
        for_ (chunksOf (fromIntegral batchSize) conns) $ \batch -> do
          void . async $ pooledMapConcurrentlyN_ 16 (uncurry close) batch
          liftIO $ threadDelay ((opts ^. millisecondsBetweenBatches) # MilliSecond)
        info $ msg (val "Draining complete")
      -- Sleeps for the grace period. This is to be run in 'race_' with the
      -- drain action. If the sleep completes, it means that draining didn't
      -- finish, and we should log and it is time to exit the process.
      timeoutAction = do
        liftIO $ threadDelay ((opts ^. gracePeriodSeconds) # Second)
        err $ msg (val "Drain grace period expired") . field "gracePeriodSeconds" (opts ^. gracePeriodSeconds)
  race_ timeoutAction drainAction

close :: Key -> Websocket -> WS ()
close k c = do
  let kb = key2bytes k
  debug $ client kb . msg (val "close websocket")
  liftIO $ sendClose (connection c) ("close" :: ByteString)

regInfo :: Key -> Maybe ClientId -> WS (Request -> Request)
regInfo k c = do
  e <- WS ask
  let h = externalHostname e
      p = portnum e
      r = "http://" <> h <> ":" <> pack (show p) <> "/i/push/"
  return . lbytes . encode . object $
    [ "user_id" .= decodeUtf8 (keyUserBytes k),
      "device_id" .= decodeUtf8 (keyConnBytes k),
      "resource" .= decodeUtf8 (r <> keyUserBytes k <> "/" <> keyConnBytes k),
      "client_id" .= c
    ]

client :: ByteString -> Msg -> Msg
client = field "client"

retry3x :: RetryPolicy
retry3x = limitRetries 3 <> exponentialBackoff 100000
