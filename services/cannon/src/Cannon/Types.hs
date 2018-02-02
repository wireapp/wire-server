{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Cannon.Types
    ( Env
    , UserDevicePayload
    , BulkPush
    , mon
    , opts
    , applog
    , dict
    , env
    , logger
    , Cannon
    , mkEnv
    , runCannon
    , options
    , clients
    , monitor
    , wsenv
    , bpRecipients
    , udUid
    , udDid
    , udData
    ) where

import Bilge (Manager, RequestId (..), requestIdName)
import Bilge.RPC (HasRequestId (..))
import Cannon.Dict (Dict)
import Cannon.WS (Key, Websocket, Clock)
import Cannon.Options
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Lens
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Id (UserId, ConnId)
import Data.Metrics.Middleware
import Data.Text.Encoding
import GHC.Generics
import Gundeck.Types
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

instance MonadLogger Cannon where
    log l m = Cannon $ do
        g <- asks applog
        r <- field "request" . unRequestId <$> asks reqId
        liftIO $ Logger.log g l (r . m)

instance HasRequestId Cannon where
    getRequestId = Cannon $ asks reqId

data UserDevicePayload = UserDevicePayload
    { udUid  :: !UserId
    , udDid  :: !ConnId
    , udData :: !Notification
    } deriving ( Show
               , Generic
               )

instance FromJSON UserDevicePayload
instance ToJSON   UserDevicePayload

data BulkPush = BulkPush
    { bpRecipients :: ![UserDevicePayload]
    } deriving ( Show
               , Generic
               )

instance FromJSON BulkPush
instance ToJSON   BulkPush

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
