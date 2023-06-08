{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Wire.BackgroundWorker.Env where

import Cassandra (ClientState)
import qualified Cassandra as Cass
import qualified Cassandra.Settings as Cass
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Trans.Control
import Data.Domain (Domain)
import HTTP2.Client.Manager
import Imports
import Network.HTTP.Client
import OpenSSL.Session (SSLOption (..))
import qualified OpenSSL.Session as SSL
import qualified System.Logger as Log
import System.Logger.Class
import qualified System.Logger.Extended as Log
import Util.Options
import Wire.BackgroundWorker.Options

data Env = Env
  { manager :: Manager,
    http2Manager :: Http2Manager,
    logger :: Logger,
    federatorInternal :: Endpoint,
    localDomain :: Domain,
    cassandra :: ClientState
  }

mkEnv :: Opts -> IO Env
mkEnv opts = do
  manager <- newManager defaultManagerSettings
  http2Manager <- initHttp2Manager
  logger <- Log.mkLogger opts.logLevel Nothing opts.logFormat
  let federatorInternal = opts.federatorInternal
      localDomain = opts.localDomain
  cassandra <- Cass.init $ Cass.defSettings -- TODO: Update these settings
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

newtype AppT m a where
  AppT :: {unAppT :: ReaderT Env m a} -> AppT m a
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
