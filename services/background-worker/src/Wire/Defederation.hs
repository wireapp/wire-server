module Wire.Defederation where

import Bilge.Retry
import Control.Concurrent.Async
import Control.Lens (to, (^.))
import Control.Monad.Catch
import Control.Retry
import Data.Aeson qualified as A
import Data.ByteString.Conversion
import Data.Domain
import Data.Text (unpack)
import Data.Text.Encoding
import Imports
import Network.AMQP qualified as Q
import Network.AMQP.Extended
import Network.AMQP.Lifted qualified as QL
import Network.HTTP.Client
import Network.HTTP.Types
import Servant.Client (BaseUrl (..), ClientEnv, Scheme (Http), mkClientEnv)
import System.Logger.Class qualified as Log
import Util.Options
import Wire.API.Federation.BackendNotifications
import Wire.API.Routes.FederationDomainConfig qualified as Fed
import Wire.BackgroundWorker.Env
import Wire.BackgroundWorker.Util

deleteFederationDomain :: MVar () -> Q.Channel -> AppT IO Q.ConsumerTag
deleteFederationDomain runningFlag chan = do
  lift $ ensureQueue chan defederationQueue
  QL.consumeMsgs chan (routingKey defederationQueue) Q.Ack $ deleteFederationDomainInner runningFlag

x3 :: RetryPolicy
x3 = limitRetries 3 <> exponentialBackoff 100000

-- Exposed for testing purposes so we can decode without further processing the message.
deleteFederationDomainInner' :: (RabbitMQEnvelope e) => (e -> DefederationDomain -> AppT IO ()) -> (Q.Message, e) -> AppT IO ()
deleteFederationDomainInner' go (msg, envelope) = do
  either
    ( \e -> do
        void $ logErr e
        -- ensure that the message is _NOT_ requeued
        -- This means that we won't process this message again
        -- as it is unparsable.
        liftIO $ reject envelope False
    )
    (go envelope)
    $ A.eitherDecode @DefederationDomain (Q.msgBody msg)
  where
    logErr err =
      Log.err $
        Log.msg (Log.val "Failed to delete federation domain")
          . Log.field "error" err

mkBrigEnv :: AppT IO ClientEnv
mkBrigEnv = do
  Endpoint brigHost brigPort <- asks brig
  mkClientEnv
    <$> asks httpManager
    <*> pure (BaseUrl Http (unpack brigHost) (fromIntegral brigPort) "")

getRemoteDomains :: AppT IO [Domain]
getRemoteDomains = do
  ref <- asks remoteDomains
  fmap Fed.domain . Fed.remotes <$> readIORef ref

callGalleyDelete ::
  ( MonadReader Env m,
    MonadMask m,
    ToByteString a,
    RabbitMQEnvelope e,
    MonadIO m
  ) =>
  MVar () ->
  e ->
  a ->
  m ()
callGalleyDelete runningFlag envelope domain = do
  env <- ask
  -- Jittered exponential backoff with 10ms as starting delay and 60s as max
  -- delay. When 60 is reached, every retry will happen after 60s.
  let policy = capDelay 60_000_000 $ fullJitterBackoff 10000
      manager = httpManager env
  recovering policy httpHandlers $ \_ ->
    bracket_ (takeMVar runningFlag) (putMVar runningFlag ()) $ do
      -- Non 2xx responses will throw an exception
      -- So we are relying on that to be caught by recovering
      resp <- liftIO $ httpLbs (req env domain) manager
      let code = statusCode $ responseStatus resp
      if code >= 200 && code <= 299
        then do
          liftIO $ ack envelope
        else -- ensure that the message is requeued
        -- This message was able to be parsed but something
        -- else in our stack failed and we should try again.
          liftIO $ reject envelope True

req :: ToByteString a => Env -> a -> Request
req env dom =
  defaultRequest
    { method = methodDelete,
      secure = False,
      host = galley env ^. epHost . to encodeUtf8,
      port = galley env ^. epPort . to fromIntegral,
      path = "/i/federation/" <> toByteString' dom,
      requestHeaders = ("Accept", "application/json") : requestHeaders defaultRequest,
      responseTimeout = defederationTimeout env
    }

-- What should we do with non-recoverable (unparsable) errors/messages?
-- should we deadletter, or do something else?
-- Deadlettering has a privacy implication -- FUTUREWORK.
deleteFederationDomainInner :: RabbitMQEnvelope e => MVar () -> (Q.Message, e) -> AppT IO ()
deleteFederationDomainInner runningFlag (msg, envelope) =
  deleteFederationDomainInner' (const $ callGalleyDelete runningFlag envelope) (msg, envelope)

startDefederator :: IORef (Maybe (Q.ConsumerTag, MVar ())) -> Q.Channel -> AppT IO ()
startDefederator consumerRef chan = do
  markAsWorking DefederationWorker
  lift $ Q.qos chan 0 1 False
  runningFlag <- newMVar ()
  consumer <- deleteFederationDomain runningFlag chan
  liftIO $ atomicWriteIORef consumerRef $ pure (consumer, runningFlag)
  liftIO $ forever $ threadDelay maxBound

startWorker :: RabbitMqAdminOpts -> AppT IO (IORef (Maybe Q.Channel), IORef (Maybe (Q.ConsumerTag, MVar ())))
startWorker rabbitmqOpts = do
  env <- ask
  chanRef <- newIORef Nothing
  consumerRef <- newIORef Nothing
  let clearRefs = do
        runAppT env $ markAsNotWorking DefederationWorker
        atomicWriteIORef chanRef Nothing
        atomicWriteIORef consumerRef Nothing
  void . liftIO . async . openConnectionWithRetries env.logger (demoteOpts rabbitmqOpts) $
    RabbitMqHooks
      { onNewChannel = \chan -> do
          atomicWriteIORef chanRef $ pure chan
          runAppT env $ startDefederator consumerRef chan,
        onChannelException = const clearRefs,
        onConnectionClose = clearRefs
      }
  pure (chanRef, consumerRef)
