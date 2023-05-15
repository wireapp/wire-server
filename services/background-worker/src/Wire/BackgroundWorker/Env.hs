{-# LANGUAGE RecordWildCards #-}

module Wire.BackgroundWorker.Env where

import qualified Data.Text as Text
import HTTP2.Client.Manager
import Imports
import qualified Network.AMQP as Q
import OpenSSL.Session (SSLOption (..))
import qualified OpenSSL.Session as SSL
import System.Logger.Extended (Logger)
import qualified System.Logger.Extended as Logger
import Util.Options
import Wire.BackgroundWorker.Options

data Env = Env
  { http2Manager :: Http2Manager,
    -- TODO(elland): Find out if there are benefits of having one channel for everything
    -- or should we create more channels?
    rabbitmqChannel :: IORef Q.Channel,
    federatorInternal :: Endpoint
  }

mkEnv :: Opts -> IO Env
mkEnv opts = do
  http2Manager <- initHttp2Manager
  l <- Logger.mkLogger opts.logLevel Nothing opts.logFormat
  rabbitmqChannel <- initRabbitMq l opts.rabbitmq
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

initRabbitMq :: Logger -> RabbitMqOpts -> IO (IORef Q.Channel)
initRabbitMq l opts = do
  username <- Text.pack <$> getEnv "RABBITMQ_USERNAME"
  password <- Text.pack <$> getEnv "RABBITMQ_PASSWORD"
  ref <- newIORef (error "connection to rabbiqmq not established yet!")
  connect username password ref
  pure ref
  where
    connect username password ref = do
      conn <- Q.openConnection' opts.host (fromIntegral opts.port) opts.vHost username password
      chan <- Q.openChannel conn
      -- TODO(elland): Q.addConnectionClosedHandler
      -- TODO(elland): Q.addConnectionBlockedHandler (Probably not required: https://www.rabbitmq.com/connection-blocked.html)
      Q.addChannelExceptionHandler chan (handler username password ref)
      atomicWriteIORef ref chan
    handler username password ref e =
      unless (Q.isNormalChannelClose e) $ do
        Logger.err l $ Logger.msg (Logger.val "RabbitMQ connection had an exception") . Logger.field "error" (displayException e)
        connect username password ref
