module Wire.Defederation where

import qualified Data.Aeson as A
import Data.Domain
import Imports
import qualified Network.AMQP as Q
import qualified Network.AMQP.Lifted as QL
import qualified System.Logger.Class as Log
import Wire.API.Federation.BackendNotifications
import Wire.BackgroundWorker.Env
import Wire.BackgroundWorker.Util
import Control.Concurrent.Async
import Network.HTTP.Client
import Network.HTTP.Types
import Util.Options
import Control.Lens ((^.), to)
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy as L
import Data.Text.Encoding
import Control.Retry
import Bilge.Retry
import Control.Monad.Catch

deleteFederationDomain :: Q.Channel -> AppT IO Q.ConsumerTag
deleteFederationDomain chan = do
  lift $ ensureQueue chan defederationQueue
  QL.consumeMsgs chan (routingKey defederationQueue) Q.Ack deleteFederationDomainInner

x3 :: RetryPolicy
x3 = limitRetries 3 <> exponentialBackoff 100000

-- What should we do with non-recoverable (unparsable) errors/messages?
-- should we deadletter, or do something else?
-- Deadlettering has a privacy implication -- FUTUREWORK.
deleteFederationDomainInner :: RabbitMQEnvelope e => (Q.Message, e) -> AppT IO ()
deleteFederationDomainInner (msg, envelope) = do
  env <- ask
  let manager = httpManager env
      req :: Domain -> Request
      req dom = defaultRequest
        { method = methodDelete
        , secure = False
        , host = galley env ^. epHost . to encodeUtf8
        , port = galley env ^. epPort . to fromIntegral
        , path = "/i/federation/" <> toByteString' dom
        , requestHeaders = ("Accept", "application/json") : requestHeaders defaultRequest
        , responseTimeout = defederationTimeout env
        }
  either
    (\e -> do
      logErr e
      -- ensure that the message is _NOT_ requeued
      -- This means that we won't process this message again
      -- as it is unparsable.
      liftIO $ reject envelope False
    )
    (\d -> do
      -- Retry the request a couple of times. If the final one fails, catch the exception\
      -- so that we can NACK the message and requeue it.
      resp <- try $ recovering x3 httpHandlers $ \_ -> liftIO $ httpLbs (req d) manager
      either
        -- Requeue the exception and rethrow the exception
        (\(e :: SomeException) -> liftIO (reject envelope True) >> throwM e)
        go
        resp
    )
    $ A.eitherDecode @DefederationDomain (Q.msgBody msg)
  where
    go :: Response L.ByteString -> AppT IO ()
    go resp = do
      let code = statusCode $ responseStatus resp
      if code >= 200 && code <= 299
      then do
        liftIO $ ack envelope
      else
         -- ensure that the message is requeued
         -- This message was able to be parsed but something
         -- else in our stack failed and we should try again.
        liftIO $ reject envelope True
    logErr err = Log.err $
      Log.msg (Log.val "Failed to delete federation domain")
        . Log.field "error" err

deleteWorker :: Q.Channel -> AppT IO (Async ())
deleteWorker chan = do
  lift $ Q.qos chan 0 1 False
  env <- ask
  liftIO $ async $ do
    void $ runAppT env $ deleteFederationDomain chan
    forever $ threadDelay maxBound