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

deleteFederationDomain :: Q.Channel -> AppT IO Q.ConsumerTag
deleteFederationDomain chan = do
  lift $ ensureQueue chan defederationQueue
  QL.consumeMsgs chan (routingKey defederationQueue) Q.Ack deleteFederationDomainInner

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
      liftIO $ reject envelope True -- ensure that the message is requeued
    )
    (\d -> do
      resp <- liftIO (httpLbs (req d) manager)
      go resp
    )
    $ A.eitherDecode @DefederationDomain (Q.msgBody msg)
  where
    go :: Response L.ByteString -> AppT IO ()
    go resp = do
      let code = statusCode $ responseStatus resp
      if code >= 200 && code <= 299
      then do
        logErr $ show resp
        liftIO $ ack envelope
      else liftIO $ reject envelope True -- ensure that the message is requeued
    logErr err = Log.err $
      Log.msg (Log.val "Failed delete federation domain")
        . Log.field "error" err

deleteWorker :: Q.Channel -> AppT IO (Async ())
deleteWorker chan = do
  lift $ Q.qos chan 0 1 False
  env <- ask
  liftIO $ async $ do
    void $ runAppT env $ deleteFederationDomain chan
    forever $ threadDelay maxBound