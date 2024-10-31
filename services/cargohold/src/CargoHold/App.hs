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
    Env (..),
    newEnv,
    closeEnv,
    awsLens,
    multiIngressLens,
    httpManagerLens,
    http2ManagerLens,
    appLoggerLens,
    requestIdLens,
    localUnitLens,
    optionsLens,

    -- * App Monad
    AppT,
    App,
    runAppT,
    executeBrigInteral,

    -- * Handler Monad
    Handler,
    runHandler,
  )
where

import Amazonka (S3AddressingStyle (S3AddressingStylePath))
import Bilge (Manager, MonadHttp, newManager, withResponse)
import qualified Bilge
import Bilge.RPC (HasRequestId (..))
import qualified CargoHold.AWS as AWS
import CargoHold.Options (AWSOpts, Opts, S3Compatibility (..), brig)
import qualified CargoHold.Options as Opt
import Control.Error (ExceptT, exceptT)
import Control.Exception (throw)
import Control.Lens (lensField, lensRules, makeLensesWith, non, (.~), (?~), (^.))
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Data.Id
import qualified Data.Map as Map
import Data.Qualified
import HTTP2.Client.Manager (Http2Manager, http2ManagerWithSSLCtx)
import Imports hiding (log)
import Network.HTTP.Client (ManagerSettings (..), requestHeaders, responseTimeoutMicro)
import Network.HTTP.Client.OpenSSL
import Network.Wai.Utilities (Error (..))
import OpenSSL.Session (SSLContext, SSLOption (..))
import qualified OpenSSL.Session as SSL
import Prometheus
import qualified Servant.Client as Servant
import System.Logger.Class hiding (settings)
import qualified System.Logger.Extended as Log
import Util.Options
import Util.SuffixNamer
import Wire.API.Routes.Internal.Brig (BrigInternalClient)
import qualified Wire.API.Routes.Internal.Brig as IBrig

-------------------------------------------------------------------------------
-- Environment

data Env = Env
  { aws :: AWS.Env,
    appLogger :: Logger,
    httpManager :: Manager,
    http2Manager :: Http2Manager,
    requestId :: RequestId,
    options :: Opt.Opts,
    localUnit :: Local (),
    multiIngress :: Map String AWS.Env
  }

makeLensesWith (lensRules & lensField .~ suffixNamer) ''Env

newEnv :: Opts -> IO Env
newEnv opts = do
  logger <- Log.mkLogger opts.logLevel opts.logNetStrings opts.logFormat
  checkOpts opts logger
  httpMgr <- initHttpManager opts.aws.s3Compatibility
  http2Mgr <- initHttp2Manager
  awsEnv <- initAws opts.aws logger httpMgr
  multiIngressAWS <- initMultiIngressAWS logger httpMgr
  let localDomain = toLocalUnsafe opts.settings.federationDomain ()
  pure $ Env awsEnv logger httpMgr http2Mgr (RequestId defRequestId) opts localDomain multiIngressAWS
  where
    initMultiIngressAWS :: Logger -> Manager -> IO (Map String AWS.Env)
    initMultiIngressAWS logger httpMgr =
      Map.fromList
        <$> mapM
          ( \(k, v) ->
              initAws (patchS3DownloadEndpoint v) logger httpMgr >>= \v' -> pure (k, v')
          )
          (Map.assocs (opts ^. Opt.awsLens . Opt.multiIngressLens . non Map.empty))

    patchS3DownloadEndpoint :: AWSEndpoint -> AWSOpts
    patchS3DownloadEndpoint e = (opts ^. Opt.awsLens) & Opt.s3DownloadEndpointLens ?~ e

-- | Validate (some) options (`Opts`)
--
-- Logs and throws if an invalid combination is found.
checkOpts :: Opts -> Logger -> IO ()
checkOpts opts lgr = do
  when (multiIngressConfigured && cloudFrontConfigured) $ do
    let errorMsg = "Invalid configuration: multiIngress and cloudFront cannot be combined!"
    Log.fatal lgr $ Log.msg @String errorMsg
    error errorMsg
  when (multiIngressConfigured && singleAwsDownloadEndpointConfigured) $ do
    let errorMsg = "Invalid configuration: multiIngress and s3DownloadEndpoint cannot be combined!"
    Log.fatal lgr $ Log.msg @String errorMsg
    error errorMsg
  where
    multiIngressConfigured :: Bool
    multiIngressConfigured = (not . null) (opts ^. (Opt.awsLens . Opt.multiIngressLens . non Map.empty))

    cloudFrontConfigured :: Bool
    cloudFrontConfigured = isJust opts.aws.cloudFront

    singleAwsDownloadEndpointConfigured :: Bool
    singleAwsDownloadEndpointConfigured = isJust opts.aws.s3DownloadEndpoint

initAws :: AWSOpts -> Logger -> Manager -> IO AWS.Env
initAws o l = AWS.mkEnv l o.s3Endpoint addrStyle downloadEndpoint o.s3Bucket o.cloudFront
  where
    downloadEndpoint = fromMaybe o.s3Endpoint o.s3DownloadEndpoint
    addrStyle = maybe S3AddressingStylePath Opt.unwrapS3AddressingStyle o.s3AddressingStyle

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

initHttp2Manager :: IO Http2Manager
initHttp2Manager = http2ManagerWithSSLCtx =<< initSSLContext

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
closeEnv e = Log.close e.appLogger

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
      MonadReader Env,
      MonadMonitor
    )

type App = AppT IO

instance MonadLogger App where
  log l m = do
    g <- asks (.appLogger)
    r <- asks (.requestId)
    Log.log g l $ "request" .= unRequestId r ~~ m

instance MonadLogger (ExceptT e App) where
  log l = lift . log l

instance MonadHttp App where
  handleRequestWithCont req handler = do
    manager <- asks (.httpManager)
    liftIO $ withResponse req manager handler

instance HasRequestId App where
  getRequestId = asks (.requestId)

instance MonadHttp (ExceptT e App) where
  handleRequestWithCont req handler = lift $ Bilge.handleRequestWithCont req handler

instance HasRequestId (ExceptT e App) where
  getRequestId = lift getRequestId

runAppT :: Env -> AppT m a -> m a
runAppT e (AppT a) = runReaderT a e

executeBrigInteral :: BrigInternalClient a -> App (Either Servant.ClientError a)
executeBrigInteral action = do
  httpMgr <- asks (.httpManager)
  brigEndpoint <- asks (.options.brig)
  liftIO $ IBrig.runBrigInternalClient httpMgr brigEndpoint action

-------------------------------------------------------------------------------
-- Handler Monad

type Handler = ExceptT Error App

runHandler :: Env -> Handler a -> IO a
runHandler e h = runAppT e (exceptT throw pure h)
