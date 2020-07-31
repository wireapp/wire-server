{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

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

module CargoHold.App
  ( -- * Environment
    Env,
    newEnv,
    closeEnv,
    aws,
    httpManager,
    metrics,
    appLogger,
    requestId,
    settings,

    -- * App Monad
    AppT,
    App,
    runAppT,
    runAppResourceT,

    -- * Handler Monad
    Handler,
    runHandler,
  )
where

import Bilge (Manager, MonadHttp, RequestId (..), newManager, withResponse)
import qualified Bilge
import Bilge.RPC (HasRequestId (..))
import qualified CargoHold.AWS as AWS
import CargoHold.Options as Opt
import Control.Error (ExceptT, exceptT)
import Control.Lens (makeLenses, set, view, (^.))
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Trans.Resource (ResourceT, runResourceT, transResourceT)
import Data.Default (def)
import Data.Metrics.Middleware (Metrics)
import qualified Data.Metrics.Middleware as Metrics
import Imports hiding (log)
import Network.HTTP.Client (ManagerSettings (..), responseTimeoutMicro)
import Network.HTTP.Client.OpenSSL
import Network.Wai (Request, ResponseReceived)
import Network.Wai.Routing (Continue)
import Network.Wai.Utilities (Error (..), lookupRequestId)
import qualified Network.Wai.Utilities.Server as Server
import OpenSSL.Session (SSLContext, SSLOption (..))
import qualified OpenSSL.Session as SSL
import qualified OpenSSL.X509.SystemStore as SSL
import System.Logger.Class hiding (settings)
import qualified System.Logger.Extended as Log

-------------------------------------------------------------------------------
-- Environment

data Env = Env
  { _aws :: AWS.Env,
    _metrics :: Metrics,
    _appLogger :: Logger,
    _httpManager :: Manager,
    _requestId :: RequestId,
    _settings :: Opt.Settings
  }

makeLenses ''Env

newEnv :: Opts -> IO Env
newEnv o = do
  met <- Metrics.metrics
  lgr <- Log.mkLogger (o ^. optLogLevel) (o ^. optLogNetStrings) (o ^. optLogFormat)
  mgr <- initHttpManager
  ama <- initAws (o ^. optAws) lgr mgr
  return $ Env ama met lgr mgr def (o ^. optSettings)

initAws :: AWSOpts -> Logger -> Manager -> IO AWS.Env
initAws o l m =
  AWS.mkEnv l (o ^. awsS3Endpoint) downloadEndpoint (o ^. awsS3Bucket) (o ^. awsCloudFront) m
  where
    downloadEndpoint = fromMaybe (o ^. awsS3Endpoint) (o ^. awsS3DownloadEndpoint)

initHttpManager :: IO Manager
initHttpManager =
  newManager
    (opensslManagerSettings initSSLContext)
      { managerConnCount = 1024,
        managerIdleConnectionCount = 2048,
        managerResponseTimeout = responseTimeoutMicro 10000000
      }

initSSLContext :: IO SSLContext
initSSLContext = do
  ctx <- SSL.context
  SSL.contextAddOption ctx SSL_OP_NO_SSLv2
  SSL.contextAddOption ctx SSL_OP_NO_SSLv3
  SSL.contextSetCiphers ctx "HIGH"
  SSL.contextLoadSystemCerts ctx
  SSL.contextSetVerificationMode ctx $
    SSL.VerifyPeer True True Nothing
  return ctx

closeEnv :: Env -> IO ()
closeEnv e = Log.close $ e ^. appLogger

-------------------------------------------------------------------------------
-- App Monad

newtype AppT m a = AppT (ReaderT Env m a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadThrow,
      MonadCatch,
      MonadMask,
      MonadReader Env
    )

type App = AppT IO

instance MonadLogger App where
  log l m = do
    g <- view appLogger
    r <- view requestId
    Log.log g l $ "request" .= unRequestId r ~~ m

instance MonadLogger (ExceptT e App) where
  log l = lift . log l

instance MonadHttp App where
  handleRequestWithCont req handler = do
    manager <- view httpManager
    liftIO $ withResponse req manager handler

instance HasRequestId App where
  getRequestId = view requestId

instance MonadHttp (ExceptT e App) where
  handleRequestWithCont req handler = lift $ Bilge.handleRequestWithCont req handler

instance HasRequestId (ExceptT e App) where
  getRequestId = lift getRequestId

runAppT :: Env -> AppT m a -> m a
runAppT e (AppT a) = runReaderT a e

runAppResourceT :: MonadIO m => Env -> ResourceT App a -> m a
runAppResourceT e rma = liftIO . runResourceT $ transResourceT (runAppT e) rma

-------------------------------------------------------------------------------
-- Handler Monad

type Handler = ExceptT Error App

runHandler :: Env -> Request -> Handler ResponseReceived -> Continue IO -> IO ResponseReceived
runHandler e r h k =
  let e' = set requestId (maybe def RequestId (lookupRequestId r)) e
   in runAppT e' (exceptT (Server.onError (_appLogger e) [Right $ _metrics e] r k) return h)
