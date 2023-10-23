{-# LANGUAGE NumericUnderscores #-}

module Galley.Intra.BackendNotificationQueue (interpretBackendNotificationQueueAccess) where

import Control.Lens (view)
import Control.Monad.Catch
import Control.Monad.Trans.Except
import Control.Retry
import Data.Domain
import Data.Qualified
import Galley.Effects.BackendNotificationQueueAccess (BackendNotificationQueueAccess (..))
import Galley.Env
import Galley.Monad
import Galley.Options
import Imports
import Network.AMQP qualified as Q
import Polysemy
import Polysemy.Input
import System.Logger.Class qualified as Log
import UnliftIO
import Wire.API.Federation.BackendNotifications
import Wire.API.Federation.Error

interpretBackendNotificationQueueAccess ::
  ( Member (Embed IO) r,
    Member (Input Env) r
  ) =>
  Sem (BackendNotificationQueueAccess ': r) a ->
  Sem r a
interpretBackendNotificationQueueAccess = interpret $ \case
  EnqueueNotification remote deliveryMode action -> do
    embedApp . runExceptT $ enqueueNotification (tDomain remote) deliveryMode action
  EnqueueNotificationsConcurrently m xs rpc -> do
    embedApp . runExceptT $ enqueueNotificationsConcurrently m xs rpc

getChannel :: ExceptT FederationError App (MVar Q.Channel)
getChannel = view rabbitmqChannel >>= maybe (throwE FederationNotConfigured) pure

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
      Log.err $
        Log.msg @Text "failed to enqueue notification in RabbitMQ"
          . Log.field "error" (displayException e)
          . Log.field "willRetry" willRetry
          . Log.field "retryCount" status.rsIterNumber
    go ownDomain = do
      mChan <- timeout 1_000_000 (readMVar chanVar)
      case mChan of
        Nothing -> throwM NoRabbitMqChannel
        Just chan -> do
          liftIO $ enqueue chan ownDomain remoteDomain deliveryMode action

enqueueNotification :: Domain -> Q.DeliveryMode -> FedQueueClient c a -> ExceptT FederationError App a
enqueueNotification remoteDomain deliveryMode action = do
  chanVar <- getChannel
  lift $ enqueueSingleNotification remoteDomain deliveryMode chanVar action

enqueueNotificationsConcurrently ::
  (Foldable f, Functor f) =>
  Q.DeliveryMode ->
  f (Remote x) ->
  (Remote [x] -> FedQueueClient c a) ->
  ExceptT FederationError App [Remote a]
enqueueNotificationsConcurrently m xs f = do
  chanVar <- getChannel
  lift $ pooledForConcurrentlyN 8 (bucketRemote xs) $ \r ->
    qualifyAs r
      <$> enqueueSingleNotification (tDomain r) m chanVar (f r)

data NoRabbitMqChannel = NoRabbitMqChannel
  deriving (Show)

instance Exception NoRabbitMqChannel
