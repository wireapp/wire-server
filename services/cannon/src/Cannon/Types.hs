{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cannon.Types
    ( Env
    , mon
    , opts
    , applog
    , dict
    , env
    , logger
    , Cannon
    , mapConcurrentlyCannon
    , mkEnv
    , runCannon
    , options
    , clients
    , monitor
    , wsenv
    ) where

import Bilge (Manager, RequestId (..), requestIdName)
import Bilge.RPC (HasRequestId (..))
import Cannon.Dict (Dict)
import Cannon.WS (Key, Websocket, Clock)
import Cannon.Options
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Reader (runReaderT)
import Control.Lens
import Data.ByteString (ByteString)
import Data.Metrics.Middleware
import Data.Text.Encoding
import Network.Wai
import System.Logger.Class hiding (info)
import System.Random.MWC (GenIO)

import qualified Cannon.WS          as WS
import qualified System.Logger      as Logger

-----------------------------------------------------------------------------
-- Cannon monad

data Env = Env
    { mon     :: !Metrics
    , opts    :: !Opts
    , applog  :: !Logger
    , dict    :: !(Dict Key Websocket)
    , reqId   :: !RequestId
    , env     :: !WS.Env
    }

newtype Cannon a = Cannon
    { unCannon :: ReaderT Env IO a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadIO
               , MonadThrow
               , MonadCatch
               , MonadMask
               )

mapConcurrentlyCannon :: Traversable t => (a -> Cannon b) -> t a -> Cannon (t b)
mapConcurrentlyCannon action inputs = Cannon $ ask >>= \e ->
    liftIO $ mapConcurrently ((`runReaderT` e) . unCannon . action) inputs

instance MonadLogger Cannon where
    log l m = Cannon $ do
        g <- asks applog
        r <- field "request" . unRequestId <$> asks reqId
        liftIO $ Logger.log g l (r . m)

instance HasRequestId Cannon where
    getRequestId = Cannon $ asks reqId

mkEnv :: Metrics
      -> ByteString
      -> Opts
      -> Logger
      -> Dict Key Websocket
      -> Manager
      -> GenIO
      -> Clock
      -> Env
mkEnv m external o l d p g t = Env m o l d mempty $
    WS.env external (o^.cannon.port) (encodeUtf8 $ o^.gundeck.host) (o^.gundeck.port) l p d g t

runCannon :: Env -> Cannon a -> Request -> IO a
runCannon e c r = let e' = e { reqId = lookupReqId r } in
    runReaderT (unCannon c) e'

lookupReqId :: Request -> RequestId
lookupReqId = maybe mempty RequestId . lookup requestIdName . requestHeaders
{-# INLINE lookupReqId #-}

options :: Cannon Opts
options = Cannon $ asks opts

clients :: Cannon (Dict Key Websocket)
clients = Cannon $ asks dict

monitor :: Cannon Metrics
monitor = Cannon $ asks mon

wsenv :: Cannon WS.Env
wsenv = Cannon $ do
    e <- asks env
    r <- asks reqId
    return $ WS.setRequestId r e

logger :: Cannon Logger
logger = Cannon $ asks applog
