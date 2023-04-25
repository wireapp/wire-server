{-# LANGUAGE RecordWildCards #-}

module Wire.BackendNotificationPusher.Env where

import qualified Data.Text as Text
import HTTP2.Client.Manager
import Imports
import qualified Network.AMQP as Q
import OpenSSL.Session (SSLOption (..))
import qualified OpenSSL.Session as SSL
import Util.Options
import Wire.BackendNotificationPusher.Options

data Env = Env
  { http2Manager :: Http2Manager,
    rabbitmqChannel :: IORef Q.Channel,
    federatorInternal :: Endpoint
  }

mkEnv :: Opts -> IO Env
mkEnv opts = do
  http2Manager <- initHttp2Manager
  rabbitmqChannel <- newIORef =<< initRabbitMq opts.rabbitmq
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

initRabbitMq :: RabbitMqOpts -> IO Q.Channel
initRabbitMq opts = do
  username <- Text.pack <$> getEnv "RABBITMQ_USERNAME"
  password <- Text.pack <$> getEnv "RABBITMQ_PASSWORD"
  conn <- Q.openConnection' opts.host (fromIntegral opts.port) opts.vHost username password
  -- TODO: Q.addConnectionClosedHandler
  -- TODO: Q.addConnectionBlockedHandler (Probably not required: https://www.rabbitmq.com/connection-blocked.html)
  -- TODO: Q.addChannelExceptionHandler
  Q.openChannel conn
