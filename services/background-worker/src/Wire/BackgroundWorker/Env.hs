{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Wire.BackgroundWorker.Env where

import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Trans.Control
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

data Env = Env
  { http2Manager :: Http2Manager,
    httpManager :: Manager,
    logger :: Logger,
    federatorInternal :: Endpoint,
    galley :: Endpoint,
    brig :: Endpoint,
    defederationTimeout :: ResponseTimeout,
    remoteDomains :: IORef FederationDomainConfigs,
    remoteDomainsChan :: Chan FederationDomainConfigs,
    rabbitmqAdminClient :: RabbitMqAdmin.AdminAPI (Servant.AsClientT IO),
    rabbitmqVHost :: Text
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
