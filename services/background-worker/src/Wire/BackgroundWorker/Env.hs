{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Wire.BackgroundWorker.Env where

import Cassandra (ClientState)
import Cassandra.Util (defInitCassandra)
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Trans.Control
import Data.Domain (Domain)
import Data.Map.Strict qualified as Map
import HTTP2.Client.Manager
import Hasql.Pool qualified as Hasql
import Hasql.Pool.Extended
import Imports
import Network.AMQP qualified as Q
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
import Wire.ConversationStore (PostgresMigrationOpts)

type IsWorking = Bool

-- | Eventually this will be a sum type of all the types of workers
data Worker
  = BackendNotificationPusher
  | DeadUserNotificationWatcher
  | BackgroundJobConsumer
  deriving (Eq, Ord)

workerName :: Worker -> Text
workerName = \case
  BackendNotificationPusher -> "backend-notification-pusher"
  DeadUserNotificationWatcher -> "dead-user-notification-watcher"
  BackgroundJobConsumer -> "background-job-consumer"

data Env = Env
  { http2Manager :: Http2Manager,
    rabbitmqAdminClient :: Maybe (RabbitMqAdmin.AdminAPI (Servant.AsClientT IO)),
    rabbitmqVHost :: Text,
    logger :: Logger,
    federatorInternal :: Endpoint,
    httpManager :: Manager,
    defederationTimeout :: ResponseTimeout,
    backendNotificationMetrics :: BackendNotificationMetrics,
    backendNotificationsConfig :: BackendNotificationsConfig,
    backgroundJobsConfig :: BackgroundJobsConfig,
    workerRunningGauge :: Vector Text Gauge,
    statuses :: IORef (Map Worker IsWorking),
    cassandra :: ClientState,
    cassandraGalley :: ClientState,
    cassandraBrig :: ClientState,
    hasqlPool :: Hasql.Pool,
    -- Dedicated AMQP channels per concern
    amqpJobsPublisherChannel :: MVar Q.Channel,
    amqpBackendNotificationsChannel :: MVar Q.Channel,
    federationDomain :: Domain,
    postgresMigration :: PostgresMigrationOpts,
    gundeckEndpoint :: Endpoint,
    brigEndpoint :: Endpoint
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

mkWorkerRunningGauge :: IO (Vector Text Gauge)
mkWorkerRunningGauge =
  register (vector "worker" $ gauge $ Prometheus.Info "wire_background_worker_running_workers" "Set to 1 when a worker is running")

mkEnv :: Opts -> IO Env
mkEnv opts = do
  logger <- Log.mkLogger opts.logLevel Nothing opts.logFormat
  cassandra <- defInitCassandra opts.cassandra =<< setLoggerName "cassandra-gundeck" logger
  cassandraGalley <- defInitCassandra opts.cassandraGalley =<< setLoggerName "cassandra-galley" logger
  cassandraBrig <- defInitCassandra opts.cassandraBrig =<< setLoggerName "cassandra-brig" logger
  http2Manager <- initHttp2Manager
  httpManager <- newManager defaultManagerSettings
  let federatorInternal = opts.federatorInternal
      defederationTimeout =
        maybe
          responseTimeoutNone
          (\t -> responseTimeoutMicro $ 1000000 * t) -- seconds to microseconds
          opts.defederationTimeout
      rabbitmqVHost = either (.vHost) (.vHost) opts.rabbitmq.unRabbitMqOpts
  rabbitmqAdminClient <- for (rightToMaybe opts.rabbitmq.unRabbitMqOpts) mkRabbitMqAdminClientEnv
  statuses <-
    newIORef $
      Map.fromList
        [ (BackendNotificationPusher, False),
          (BackgroundJobConsumer, False)
        ]
  backendNotificationMetrics <- mkBackendNotificationMetrics
  let backendNotificationsConfig = opts.backendNotificationPusher
      backgroundJobsConfig = opts.backgroundJobs
      federationDomain = opts.federationDomain
      postgresMigration = opts.postgresMigration
      brigEndpoint = opts.brig
      gundeckEndpoint = opts.gundeck
  workerRunningGauge <- mkWorkerRunningGauge
  hasqlPool <- initPostgresPool opts.postgresqlPool opts.postgresql opts.postgresqlPassword
  amqpJobsPublisherChannel <-
    mkRabbitMqChannelMVar logger (Just "background-worker-jobs-publisher") $
      either id demoteOpts opts.rabbitmq.unRabbitMqOpts
  amqpBackendNotificationsChannel <-
    mkRabbitMqChannelMVar logger (Just "background-worker-backend-notifications") $
      either id demoteOpts opts.rabbitmq.unRabbitMqOpts
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

markAsWorking :: (MonadIO m, MonadMonitor m) => Worker -> AppT m ()
markAsWorking = updateWorkingStatus True

markAsNotWorking :: (MonadIO m, MonadMonitor m) => Worker -> AppT m ()
markAsNotWorking = updateWorkingStatus False

updateWorkingStatus :: (MonadIO m, MonadMonitor m) => Bool -> Worker -> AppT m ()
updateWorkingStatus isWorking worker = do
  env <- ask
  modifyIORef env.statuses (Map.insert worker isWorking)
  withLabel env.workerRunningGauge (workerName worker) (flip setGauge (if isWorking then 1 else 0))

withNamedLogger :: (MonadIO m) => Text -> AppT m a -> AppT m a
withNamedLogger name action = do
  env <- ask
  namedLogger <- setLoggerName name env.logger
  lift $ runAppT (env {logger = namedLogger}) action

setLoggerName :: (MonadIO m) => Text -> Log.Logger -> m Log.Logger
setLoggerName name logger =
  Log.new $ Log.setName (Just name) $ Log.settings logger
