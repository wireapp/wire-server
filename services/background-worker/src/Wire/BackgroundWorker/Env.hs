{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Wire.BackgroundWorker.Env where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Trans.Control
import HTTP2.Client.Manager
import Imports
import Network.AMQP.Extended
import qualified Network.RabbitMqAdmin as RabbitMqAdmin
import OpenSSL.Session (SSLOption (..))
import qualified OpenSSL.Session as SSL
import qualified Servant.Client as Servant
import qualified System.Logger as Log
import System.Logger.Class
import qualified System.Logger.Extended as Log
import Util.Options
import Wire.BackgroundWorker.Options

data Env = Env
  { http2Manager :: Http2Manager,
    rabbitmqAdminClient :: RabbitMqAdmin.AdminAPI (Servant.AsClientT IO),
    rabbitmqVHost :: Text,
    logger :: Logger,
    federatorInternal :: Endpoint,
    backendNotificationPusher :: BackendNotificationPusherOpts
  }

mkEnv :: Opts -> IO Env
mkEnv opts = do
  http2Manager <- initHttp2Manager
  logger <- Log.mkLogger opts.logLevel Nothing opts.logFormat
  let federatorInternal = opts.federatorInternal
  rabbitmqAdminClient <- mkRabbitMqAdminClientEnv opts.rabbitmq
  let rabbitmqVHost = opts.rabbitmq.vHost
      backendNotificationPusher = opts.backendNotificationPusher
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
