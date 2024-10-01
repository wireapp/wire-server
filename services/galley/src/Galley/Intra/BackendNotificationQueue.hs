{-# LANGUAGE NumericUnderscores #-}

module Galley.Intra.BackendNotificationQueue (interpretBackendNotificationQueueAccess) where

import Control.Lens (view)
import Control.Monad.Catch
import Control.Monad.Trans.Except
import Control.Retry
import Data.Domain
import Data.Qualified
import Galley.Cassandra.Util
import Galley.Effects.BackendNotificationQueueAccess (BackendNotificationQueueAccess (..))
import Galley.Env
import Galley.Monad
import Galley.Options
import Imports
import Network.AMQP qualified as Q
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog
import System.Logger.Class qualified as Log
import UnliftIO
import Wire.API.Federation.BackendNotifications
import Wire.API.Federation.Error

interpretBackendNotificationQueueAccess ::
  ( Member (Embed IO) r,
    Member (Input Env) r,
    Member TinyLog r
  ) =>
  Sem (BackendNotificationQueueAccess ': r) a ->
  Sem r a
interpretBackendNotificationQueueAccess = interpret $ \case
  EnqueueNotification deliveryMode remote action -> do
    logEffect "BackendNotificationQueueAccess.EnqueueNotification"
    embedApp . runExceptT $ enqueueNotification deliveryMode (tDomain remote) action
  EnqueueNotificationsConcurrently m xs rpc -> do
    logEffect "BackendNotificationQueueAccess.EnqueueNotificationsConcurrently"
    embedApp . runExceptT $ enqueueNotificationsConcurrently m xs rpc
  EnqueueNotificationsConcurrentlyBuckets m xs rpc -> do
    logEffect "BackendNotificationQueueAccess.EnqueueNotificationsConcurrentlyBuckets"
    embedApp . runExceptT $ enqueueNotificationsConcurrentlyBuckets m xs rpc

enqueueSingleNotification :: Domain -> Q.DeliveryMode -> MVar Q.Channel -> FedQueueClient c a -> App a
enqueueSingleNotification remoteDomain deliveryMode chanVar action = do
  ownDomain <- view (options . settings . federationDomain)
  let policy = limitRetries 3 <> constantDelay 1_000_000
      handlers =
        skipAsyncExceptions
          <> [logRetries (const $ pure True) logError]
  recovering policy handlers (const $ go ownDomain)
  where
    logError willRetry (SomeException e) status = do
      rid <- view reqId
      Log.err $
        Log.msg @Text "failed to enqueue notification in RabbitMQ"
          . Log.field "error" (displayException e)
          . Log.field "willRetry" willRetry
          . Log.field "retryCount" status.rsIterNumber
          . Log.field "request" rid
    go ownDomain = do
      rid <- view reqId
      mChan <- timeout 1_000_000 (readMVar chanVar)
      case mChan of
        Nothing -> throwM NoRabbitMqChannel
        Just chan -> do
          liftIO $ enqueue chan rid ownDomain remoteDomain deliveryMode action

enqueueNotification :: Q.DeliveryMode -> Domain -> FedQueueClient c a -> ExceptT FederationError App a
enqueueNotification deliveryMode remoteDomain action = do
  chanVar <- view rabbitmqChannel
  lift $ enqueueSingleNotification remoteDomain deliveryMode chanVar action

enqueueNotificationsConcurrently ::
  (Foldable f, Functor f) =>
  Q.DeliveryMode ->
  f (Remote x) ->
  (Remote [x] -> FedQueueClient c a) ->
  ExceptT FederationError App [Remote a]
enqueueNotificationsConcurrently m xs f =
  enqueueNotificationsConcurrentlyBuckets m (bucketRemote xs) f

enqueueNotificationsConcurrentlyBuckets ::
  (Foldable f) =>
  Q.DeliveryMode ->
  f (Remote x) ->
  (Remote x -> FedQueueClient c a) ->
  ExceptT FederationError App [Remote a]
enqueueNotificationsConcurrentlyBuckets m xs f = do
  case toList xs of
    -- only attempt to get a channel if there is at least one notification to send
    [] -> pure []
    _ -> do
      chanVar <- view rabbitmqChannel
      lift $ pooledForConcurrentlyN 8 (toList xs) $ \r ->
        qualifyAs r
          <$> enqueueSingleNotification (tDomain r) m chanVar (f r)

data NoRabbitMqChannel = NoRabbitMqChannel
  deriving (Show)

instance Exception NoRabbitMqChannel
