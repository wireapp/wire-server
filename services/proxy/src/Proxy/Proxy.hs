{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Proxy.Proxy (Proxy, runProxy)  where

import Imports
import Bilge.Request (requestIdName)
import Data.Default (def)
import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Data.Id (RequestId (..))
import Proxy.Env
import Network.Wai
import System.Logger.Class hiding (Error, info)
import Control.Monad.IO.Unlift()

import qualified System.Logger as Logger

newtype Proxy a = Proxy
    { unProxy :: ReaderT Env IO a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadIO
               , MonadThrow
               , MonadCatch
               , MonadMask
               , MonadReader Env
               , MonadUnliftIO
               )

instance MonadLogger Proxy where
    log l m = ask >>= \e -> Logger.log (e^.applog) l (reqIdMsg (e^.reqId) . m)

runProxy :: Env -> Request -> Proxy ResponseReceived -> IO ResponseReceived
runProxy e r m = runReaderT (unProxy m) (reqId .~ lookupReqId r $ e)

reqIdMsg :: RequestId -> Msg -> Msg
reqIdMsg = ("request" .=) . unRequestId
{-# INLINE reqIdMsg #-}

lookupReqId :: Request -> RequestId
lookupReqId = maybe def RequestId . lookup requestIdName . requestHeaders
