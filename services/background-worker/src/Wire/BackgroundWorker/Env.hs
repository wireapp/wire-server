{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Wire.BackgroundWorker.Env where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Trans.Control
import Data.Map.Strict qualified as Map
import HTTP2.Client.Manager
import Imports
import Network.AMQP.Extended
import Network.HTTP.Client
import Network.RabbitMqAdmin qualified as RabbitMqAdmin
import OpenSSL.Session (SSLOption (..))
import OpenSSL.Session qualified as SSL
import Prometheus
import Servant.Client qualified as Servant
import System.Logger qualified as Log
import System.Logger.Class (Logger, MonadLogger (..))
import System.Logger.Extended qualified as Log
import Util.Options
import Wire.BackgroundWorker.Options

type IsWorking = Bool

-- | Eventually this will be a sum type of all the types of workers
data Worker
  = BackendNotificationPusher
  deriving (Show, Eq, Ord)

data Env = Env
  { http2Manager :: Http2Manager,
    rabbitmqAdminClient :: RabbitMqAdmin.AdminAPI (Servant.AsClientT IO),
    rabbitmqVHost :: Text,
    logger :: Logger,
    federatorInternal :: Endpoint,
    httpManager :: Manager,
    defederationTimeout :: ResponseTimeout,
    backendNotificationMetrics :: BackendNotificationMetrics,
    backendNotificationsConfig :: BackendNotificationsConfig,
    statuses :: IORef (Map Worker IsWorking)
  }

data BackendNotificationMetrics = BackendNotificationMetrics
  { pushedCounter :: Vector Text Counter,
    errorCounter :: Vector Text Counter,
    stuckQueuesGauge :: Vector Text Gauge
  }

mkBackendNotificationMetrics :: IO BackendNotificationMetrics
mkBackendNotificationMetrics =
  BackendNotificationMetrics
    <$> register (vector "targetDomain" $ counter $ Prometheus.Info "wire_backend_notifications_pushed" "Number of notifications pushed")
    <*> register (vector "targetDomain" $ counter $ Prometheus.Info "wire_backend_notifications_errors" "Number of errors that occurred while pushing notifications")
    <*> register (vector "targetDomain" $ gauge $ Prometheus.Info "wire_backend_notifications_stuck_queues" "Set to 1 when pushing notifications is stuck")

mkEnv :: Opts -> IO Env
mkEnv opts = do
  http2Manager <- initHttp2Manager
  logger <- Log.mkLogger opts.logLevel Nothing opts.logFormat
  httpManager <- newManager defaultManagerSettings
  let federatorInternal = opts.federatorInternal
      defederationTimeout =
        maybe
          responseTimeoutNone
          (\t -> responseTimeoutMicro $ 1000000 * t) -- seconds to microseconds
          opts.defederationTimeout
      rabbitmqVHost = opts.rabbitmq.vHost
  rabbitmqAdminClient <- mkRabbitMqAdminClientEnv opts.rabbitmq
  statuses <-
    newIORef $
      Map.fromList
        [ (BackendNotificationPusher, False)
        ]
  backendNotificationMetrics <- mkBackendNotificationMetrics
  let backendNotificationsConfig = opts.backendNotificationPusher
  pure Env {..}

initHttp2Manager :: IO Http2Manager
initHttp2Manager = do
  ctx <- SSL.context
  SSL.contextAddOption ctx SSL_OP_NO_SSLv2
  SSL.contextAddOption ctx SSL_OP_NO_SSLv3
  SSL.contextAddOption ctx SSL_OP_NO_TLSv1
  SSL.contextSetCiphers ctx "HIGH"
  SSL.contextSetVerificationMode ctx $
    SSL.VerifyPeer True True Nothing
  SSL.contextSetDefaultVerifyPaths ctx
  http2ManagerWithSSLCtx ctx

newtype AppT m a = AppT {unAppT :: ReaderT Env m a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadCatch,
      MonadThrow,
      MonadMask,
      MonadReader Env,
      MonadTrans,
      MonadMonitor
    )

deriving newtype instance (MonadBase b m) => MonadBase b (AppT m)

deriving newtype instance (MonadBaseControl b m) => MonadBaseControl b (AppT m)

-- Coppied from Federator.
instance (MonadUnliftIO m) => MonadUnliftIO (AppT m) where
  withRunInIO inner =
    AppT . ReaderT $ \r ->
      withRunInIO $ \runner ->
        inner (runner . flip runReaderT r . unAppT)

instance (MonadIO m) => MonadLogger (AppT m) where
  log lvl m = do
    l <- asks logger
    Log.log l lvl m

runAppT :: Env -> AppT m a -> m a
runAppT env app = runReaderT (unAppT app) env

markAsWorking :: (MonadIO m) => Worker -> AppT m ()
markAsWorking worker =
  flip modifyIORef (Map.insert worker True) =<< asks statuses

markAsNotWorking :: (MonadIO m) => Worker -> AppT m ()
markAsNotWorking worker =
  flip modifyIORef (Map.insert worker False) =<< asks statuses
