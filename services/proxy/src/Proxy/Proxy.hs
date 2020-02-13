{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Proxy.Proxy
  ( Proxy,
    runProxy,
  )
where

import Bilge.Request (requestIdName)
import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Control.Monad.IO.Unlift ()
import Data.Default (def)
import Data.Id (RequestId (..))
import Imports
import Network.Wai
import Proxy.Env
import qualified System.Logger as Logger
import System.Logger.Class hiding (Error, info)

newtype Proxy a
  = Proxy
      { unProxy :: ReaderT Env IO a
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

instance MonadLogger Proxy where
  log l m = ask >>= \e -> Logger.log (e ^. applog) l (reqIdMsg (e ^. reqId) . m)

runProxy :: Env -> Request -> Proxy ResponseReceived -> IO ResponseReceived
runProxy e r m = runReaderT (unProxy m) (reqId .~ lookupReqId r $ e)

reqIdMsg :: RequestId -> Msg -> Msg
reqIdMsg = ("request" .=) . unRequestId
{-# INLINE reqIdMsg #-}

lookupReqId :: Request -> RequestId
lookupReqId = maybe def RequestId . lookup requestIdName . requestHeaders
