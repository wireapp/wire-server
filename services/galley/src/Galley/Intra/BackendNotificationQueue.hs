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
import Network.NATS.Client qualified as NATS
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
  EnqueueNotification remote action -> do
    logEffect "BackendNotificationQueueAccess.EnqueueNotification"
    embedApp . runExceptT $ enqueueNotification (tDomain remote) action
  EnqueueNotificationsConcurrently xs rpc -> do
    logEffect "BackendNotificationQueueAccess.EnqueueNotificationsConcurrently"
    embedApp . runExceptT $ enqueueNotificationsConcurrently xs rpc
  EnqueueNotificationsConcurrentlyBuckets xs rpc -> do
    logEffect "BackendNotificationQueueAccess.EnqueueNotificationsConcurrentlyBuckets"
    embedApp . runExceptT $ enqueueNotificationsConcurrentlyBuckets xs rpc

getChannel :: ExceptT FederationError App (MVar NATS.NatsChannel)
getChannel = view natsChannel >>= maybe (throwE FederationNotConfigured) pure

enqueueSingleNotification :: Domain -> MVar NATS.NatsChannel -> FedQueueClient c a -> App a
enqueueSingleNotification remoteDomain chanVar action = do
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
        Log.msg @Text "failed to enqueue notification in NATS"
          . Log.field "error" (displayException e)
          . Log.field "willRetry" willRetry
          . Log.field "retryCount" status.rsIterNumber
          . Log.field "request" rid
    go ownDomain = do
      rid <- view reqId
      mChan <- timeout 1_000_000 (readMVar chanVar)
      case mChan of
        Nothing -> throwM NoNatsChannel
        Just chan -> do
          liftIO $ enqueue chan rid ownDomain remoteDomain action

enqueueNotification :: Domain -> FedQueueClient c a -> ExceptT FederationError App a
enqueueNotification remoteDomain action = do
  chanVar <- getChannel
  lift $ enqueueSingleNotification remoteDomain chanVar action

enqueueNotificationsConcurrently ::
  (Foldable f, Functor f) =>
  f (Remote x) ->
  (Remote [x] -> FedQueueClient c a) ->
  ExceptT FederationError App [Remote a]
enqueueNotificationsConcurrently xs f =
  enqueueNotificationsConcurrentlyBuckets (bucketRemote xs) f

enqueueNotificationsConcurrentlyBuckets ::
  (Foldable f) =>
  f (Remote x) ->
  (Remote x -> FedQueueClient c a) ->
  ExceptT FederationError App [Remote a]
enqueueNotificationsConcurrentlyBuckets xs f = do
  case toList xs of
    -- only attempt to get a channel if there is at least one notification to send
    [] -> pure []
    _ -> do
      chanVar <- getChannel
      lift $ pooledForConcurrentlyN 8 (toList xs) $ \r ->
        qualifyAs r
          <$> enqueueSingleNotification (tDomain r) chanVar (f r)

data NoNatsChannel = NoNatsChannel
  deriving (Show)

instance Exception NoNatsChannel
