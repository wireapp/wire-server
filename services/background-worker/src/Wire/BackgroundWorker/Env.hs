{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Wire.BackgroundWorker.Env where

import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Trans.Control
import qualified Data.Map.Strict as Map
import qualified Data.Metrics as Metrics
import HTTP2.Client.Manager
import Imports
import Network.AMQP.Extended
import Network.HTTP.Client
import qualified Network.RabbitMqAdmin as RabbitMqAdmin
import OpenSSL.Session (SSLOption (..))
import qualified OpenSSL.Session as SSL
import qualified Servant.Client as Servant
import qualified System.Logger as Log
import System.Logger.Class (Logger, MonadLogger (..))
import qualified System.Logger.Extended as Log
import Util.Options
import Wire.API.FederationUpdate
import Wire.API.Routes.FederationDomainConfig
import Wire.BackgroundWorker.Options

type IsWorking = Bool

-- | Eventually this will be a sum type of all the types of workers
data Worker
  = BackendNotificationPusher
  | DefederationWorker
  deriving (Show, Eq, Ord)

data Env = Env
  { http2Manager :: Http2Manager,
    httpManager :: Manager,
    logger :: Logger,
    metrics :: Metrics.Metrics,
    federatorInternal :: Endpoint,
    galley :: Endpoint,
    brig :: Endpoint,
    defederationTimeout :: ResponseTimeout,
    remoteDomains :: IORef FederationDomainConfigs,
    remoteDomainsChan :: Chan FederationDomainConfigs,
    rabbitmqAdminClient :: RabbitMqAdmin.AdminAPI (Servant.AsClientT IO),
    rabbitmqVHost :: Text,
    statuses :: IORef (Map Worker IsWorking)
  }

mkEnv :: Opts -> IO (Env, Async ())
mkEnv opts = do
  http2Manager <- initHttp2Manager
  logger <- Log.mkLogger opts.logLevel Nothing opts.logFormat
  httpManager <- newManager defaultManagerSettings
  remoteDomainsChan <- newChan
  let federatorInternal = opts.federatorInternal
      galley = opts.galley
      defederationTimeout =
        maybe
          responseTimeoutNone
          (\t -> responseTimeoutMicro $ 1000000 * t) -- seconds to microseconds
          opts.defederationTimeout
      brig = opts.brig
      rabbitmqVHost = opts.rabbitmq.vHost
      callback =
        SyncFedDomainConfigsCallback
          { fromFedUpdateCallback = \_old new -> do
              writeChan remoteDomainsChan new
          }
  (remoteDomains, syncThread) <- syncFedDomainConfigs brig logger callback
  rabbitmqAdminClient <- mkRabbitMqAdminClientEnv opts.rabbitmq
  statuses <- newIORef $ Map.fromList
    [ (BackendNotificationPusher, False)
    , (DefederationWorker, False)
    ]
  metrics <- Metrics.metrics
  pure (Env {..}, syncThread)

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
      MonadTrans
    )

deriving newtype instance MonadBase b m => MonadBase b (AppT m)

deriving newtype instance MonadBaseControl b m => MonadBaseControl b (AppT m)

instance MonadIO m => MonadLogger (AppT m) where
  log lvl m = do
    l <- asks logger
    Log.log l lvl m

runAppT :: Env -> AppT m a -> m a
runAppT env app = runReaderT (unAppT app) env

markAsWorking :: MonadIO m => Worker -> AppT m ()
markAsWorking worker =
  flip modifyIORef (Map.insert worker True) =<< asks statuses

markAsNotWorking :: MonadIO m => Worker -> AppT m ()
markAsNotWorking worker =
  flip modifyIORef (Map.insert worker False) =<< asks statuses
