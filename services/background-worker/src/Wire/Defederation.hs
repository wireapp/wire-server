module Wire.Defederation where

import Brig.Data.Connection as Brig
import Cassandra
import Control.Concurrent.Async
import Control.Error
import Control.Monad.Catch
import Data.Aeson qualified as A
import Data.ByteString.Conversion
import Data.Data (Proxy (Proxy))
import Data.Qualified
import Data.Range (toRange)
import Galley.API.BackgroundProcesses qualified as Galley
import Galley.API.Error
import Galley.Effects
import Galley.Effects.DefederationNotifications (DefederationNotifications)
import Galley.Effects.DefederationNotifications qualified as E
import Imports
import Network.AMQP qualified as Q
import Network.AMQP.Extended
import Network.AMQP.Lifted qualified as QL
import Network.Wai.Utilities.JSONResponse (JSONResponse)
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog
import System.Logger.Class qualified as Log
import UnliftIO.Exception qualified as UnliftIO
import Wire.API.Federation.BackendNotifications
import Wire.API.Federation.Error
import Wire.BackgroundWorker.Env
import Wire.BackgroundWorker.Util

deleteFederationDomain :: MVar () -> Q.Channel -> AppT IO Q.ConsumerTag
deleteFederationDomain runningFlag chan = do
  lift $ ensureQueue chan defederationQueue
  QL.consumeMsgs chan (routingKey defederationQueue) Q.Ack $ deleteFederationDomainInner runningFlag

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
    logErr e =
      Log.err $
        Log.msg (Log.val "Failed to delete federation domain")
          . Log.field "error" e

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
  bracket_ (takeMVar runningFlag) (putMVar runningFlag ()) $ do
    liftIO $ case fromByteString' @DefederationDomain $ toByteString domain of
      Just d -> do
        let ld = toLocalUnsafe env.federationDomain ()
            rd = toRemoteUnsafe d ()
            maxPage = toRange (Proxy @500)
        evalToIO $ do
          E.sendDefederationNotifications maxPage (tDomain ld)
          -- this also deletes the 1:1 conversations
          Galley.unsafeRemoveRemoteMembersFromLocalConversation maxPage ld rd
          Galley.unsafeRemoveLocalMembersFromRemoteConversation maxPage rd
        liftIO $ runClient env.brigDB $ Brig.deleteRemoteConnectionsDomain (tDomain rd)
        evalToIO $ E.sendDefederationNotifications maxPage (tDomain ld)
        ack envelope
      Nothing ->
        -- reject the message without requeuing it
        reject envelope False

evalToIO :: Sem GalleyEffects a -> IO a
evalToIO action = do
  r <-
    -- log IO exceptions
    runExceptT (eval action) `UnliftIO.catch` \(e :: SomeException) -> do
      UnliftIO.throwIO e
  case r of
    -- throw any errors as IO exceptions without logging them
    Left e -> UnliftIO.throwIO e
    Right a -> pure a

eval :: Sem GalleyEffects a -> ExceptT JSONResponse IO a
eval =
  ExceptT . (error ("todo"))

type GalleyEffects =
  '[ Input ClientState,
     TinyLog,
     Embed IO,
     Error FederationError,
     Error InternalError,
     MemberStore,
     ConversationStore,
     CodeStore,
     TeamStore,
     DefederationNotifications
   ]

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
