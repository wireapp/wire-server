{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

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
    localUnit,
    options,
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

import Amazonka (S3AddressingStyle (S3AddressingStylePath))
import Bilge (Manager, MonadHttp, RequestId (..), newManager, withResponse)
import qualified Bilge
import Bilge.RPC (HasRequestId (..))
import qualified CargoHold.AWS as AWS
import CargoHold.Options as Opt
import Control.Error (ExceptT, exceptT)
import Control.Exception (throw)
import Control.Lens (Lens', makeLenses, view, (^.))
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Trans.Resource (ResourceT, runResourceT, transResourceT)
import Data.Default (def)
import Data.Metrics.Middleware (Metrics)
import qualified Data.Metrics.Middleware as Metrics
import Data.Qualified
import Imports hiding (log)
import Network.HTTP.Client (ManagerSettings (..), requestHeaders, responseTimeoutMicro)
import Network.HTTP.Client.OpenSSL
import Network.Wai.Utilities (Error (..))
import OpenSSL.Session (SSLContext, SSLOption (..))
import qualified OpenSSL.Session as SSL
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
    _options :: Opt.Opts,
    _localUnit :: Local ()
  }

makeLenses ''Env

settings :: Lens' Env Opt.Settings
settings = options . optSettings

newEnv :: Opts -> IO Env
newEnv o = do
  met <- Metrics.metrics
  lgr <- Log.mkLogger (o ^. optLogLevel) (o ^. optLogNetStrings) (o ^. optLogFormat)
  mgr <- initHttpManager (o ^. optAws . awsS3Compatibility)
  ama <- initAws (o ^. optAws) lgr mgr
  let loc = toLocalUnsafe (o ^. optSettings . Opt.setFederationDomain) ()
  pure $ Env ama met lgr mgr def o loc

initAws :: AWSOpts -> Logger -> Manager -> IO AWS.Env
initAws o l = AWS.mkEnv l (o ^. awsS3Endpoint) addrStyle downloadEndpoint (o ^. awsS3Bucket) (o ^. awsCloudFront)
  where
    downloadEndpoint = fromMaybe (o ^. awsS3Endpoint) (o ^. awsS3DownloadEndpoint)
    addrStyle = maybe S3AddressingStylePath unwrapS3AddressingStyle (o ^. awsS3AddressingStyle)

initHttpManager :: Maybe S3Compatibility -> IO Manager
initHttpManager s3Compat =
  newManager
    (opensslManagerSettings initSSLContext)
      { managerConnCount = 1024,
        managerIdleConnectionCount = 2048,
        managerResponseTimeout = responseTimeoutMicro 10000000,
        managerModifyRequest =
          pure . case s3Compat of
            Nothing -> id
            -- Not a nice place to do this, but it doesn't seem like amazonka
            -- allows us to do it differently in a non-invasive way.
            -- See https://github.com/zinfra/backend-issues/issues/1659.
            Just S3CompatibilityScalityRing -> dropContentLengthHeaderIfChunked
      }
  where
    dropContentLengthHeaderIfChunked req
      | ("content-encoding", "aws-chunked") `elem` requestHeaders req =
          modifyRequestHeaders (filter ((/= "content-length") . fst)) req
      | otherwise =
          req
    modifyRequestHeaders f req =
      req {requestHeaders = f (requestHeaders req)}

initSSLContext :: IO SSLContext
initSSLContext = do
  ctx <- SSL.context
  SSL.contextAddOption ctx SSL_OP_NO_SSLv2
  SSL.contextAddOption ctx SSL_OP_NO_SSLv3
  SSL.contextSetCiphers ctx "HIGH"
  SSL.contextSetDefaultVerifyPaths ctx
  SSL.contextSetVerificationMode ctx $
    SSL.VerifyPeer True True Nothing
  pure ctx

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

runHandler :: Env -> Handler a -> IO a
runHandler e h = runAppT e (exceptT throw pure h)
