{-# LANGUAGE RecordWildCards #-}

module Wire.BackgroundWorker.Env where

import HTTP2.Client.Manager
import Imports
import OpenSSL.Session (SSLOption (..))
import qualified OpenSSL.Session as SSL
import System.Logger.Extended (Logger)
import qualified System.Logger.Extended as Logger
import Util.Options
import Wire.BackgroundWorker.Options

data Env = Env
  { http2Manager :: Http2Manager,
    logger :: Logger,
    federatorInternal :: Endpoint
  }

mkEnv :: Opts -> IO Env
mkEnv opts = do
  http2Manager <- initHttp2Manager
  logger <- Logger.mkLogger opts.logLevel Nothing opts.logFormat
  -- rabbitmqChannel <- initRabbitMq l opts.rabbitmq hooks
  let federatorInternal = opts.federatorInternal
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
