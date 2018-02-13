{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Cannon.WS
    ( Env
    , WS
    , env
    , runWS
    , close
    , mkWebSocket
    , setRequestId
    , registerLocal
    , unregisterLocal
    , registerRemote
    , sendMsg

    , Clock
    , mkClock
    , getClock
    , getTime

    , Websocket
    , connection
    , connIdent
    , Key
    , mkKey
    , key2bytes
    , client
    )
where

import Bilge hiding (trace)
import Bilge.Retry
import Bilge.RPC
import Cannon.Dict (Dict)
import Control.Concurrent (forkIO)
import Control.Concurrent.Timeout
import Control.Monad (void, forever)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Retry
import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Conversion
import Data.Hashable
import Data.Id (ClientId, UserId, ConnId (..))
import Data.IORef
import Data.Monoid
import Data.Text.Encoding (decodeUtf8)
import Data.Timeout (TimeoutUnit (..), (#))
import Data.Word
import Network.HTTP.Types.Method
import Network.WebSockets hiding (Request)
import System.Logger.Class hiding (Settings, (.=), close)
import System.Random.MWC (GenIO, uniform)

import qualified Cannon.Dict          as D
import qualified Data.ByteString.Lazy as L
import qualified System.Logger        as Logger

-----------------------------------------------------------------------------
-- Key

newtype Key = Key
    { _key :: (ByteString, ByteString)
    } deriving (Eq, Show, Hashable)

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
    { connection :: Connection
    , connIdent  :: !Word
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
    void . forkIO $ forever $ do
        threadDelay (1 # Second)
        modifyIORef' r (+1)
    return $ Clock r

getClock :: WS Clock
getClock = WS $ asks clock

-- | Get the number of seconds elapsed since the clock was created.
getTime :: Clock -> IO Word64
getTime (Clock r) = readIORef r

-----------------------------------------------------------------------------
-- WS Monad

data Env = Env
    { externalHostname :: !ByteString
    , portnum          :: !Word16
    , upstream         :: !Request
    , reqId            :: !RequestId
    , logg             :: !Logger
    , manager          :: !Manager
    , dict             :: !(Dict Key Websocket)
    , rand             :: !GenIO
    , clock            :: !Clock
    }

setRequestId :: RequestId -> Env -> Env
setRequestId rid e = e { reqId = rid }

newtype WS a = WS
    { _conn :: ReaderT Env IO a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadIO
               , MonadThrow
               , MonadCatch
               , MonadMask
               )

instance MonadLogger WS where
    log l m = WS $ do
        g <- asks logg
        r <- field "request" . unRequestId <$> asks reqId
        liftIO $ Logger.log g l (r . m)

instance MonadHttp WS where
    getManager = WS $ asks manager

instance HasRequestId WS where
    getRequestId = WS $ asks reqId

env :: ByteString
    -> Word16
    -> ByteString
    -> Word16
    -> Logger
    -> Manager
    -> Dict Key Websocket
    -> GenIO
    -> Clock
    -> Env
env leh lp gh gp = Env leh lp (host gh . port gp $ empty) mempty

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
    void $ recovering retry3x rpcHandlers $ const $
        rpc' "gundeck" (upstream e) (method POST . path "/i/presences" . i . expect2xx)
    debug $ client kb . msg (val "registered")

sendMsg :: L.ByteString -> Key -> Websocket -> WS ()
sendMsg m k c = do
    let kb = key2bytes k
    trace  $ client kb . msg (val "sendMsg: \"" +++ L.take 128 m +++ val "...\"")
    liftIO $ recoverAll retry3x $ const $ sendBinaryData (connection c) m

close :: Key -> Websocket -> WS ()
close k c = do
    let kb = key2bytes k
    debug $ client kb . msg (val "close websocket")
    liftIO $ sendClose (connection c) ("close" :: ByteString)

regInfo :: Key -> Maybe ClientId -> WS (Request -> Request)
regInfo k c = do
    e <- WS ask
    let h  = externalHostname e
        p  = portnum e
        hp = h <> ":" <> pack (show p)
        r  = "http://" <> hp <> "/i/push"
        rb = "http://" <> hp <> "/i/bulkpush"
        ku = keyUserBytes k
        kc = keyConnBytes k
    return . lbytes . encode . object $
        [ "user_id"       .= decodeUtf8 (ku)
        , "device_id"     .= decodeUtf8 (kc)
        , "resource"      .= decodeUtf8 (r <> "/" <> ku <> "/" <> kc)
        , "resourceb"     .= decodeUtf8 (rb)
        , "cannon_host"    .= decodeUtf8 (hp)
        , "client_id"     .= c
        ]

client :: ByteString -> Msg -> Msg
client = field "client"

retry3x :: RetryPolicy
retry3x = limitRetries 3 <> exponentialBackoff 100000
