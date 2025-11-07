{-# LANGUAGE NumericUnderscores #-}

module Wire.BackendNotificationQueueAccess.RabbitMq
  ( interpretBackendNotificationQueueAccess,
    Env (..),
  )
where

import Control.Monad.Catch
import Control.Retry
import Data.Domain
import Data.Id
import Data.Qualified
import Imports
import Network.AMQP qualified as Q
import Polysemy
import Polysemy.Error
import System.Logger qualified as Log
import UnliftIO
import Wire.API.Federation.BackendNotifications
import Wire.API.Federation.Error
import Wire.BackendNotificationQueueAccess (BackendNotificationQueueAccess (..))

data Env = Env
  { channelMVar :: (MVar (Q.Channel)),
    logger :: Log.Logger,
    local :: Local (),
    requestId :: RequestId
  }

interpretBackendNotificationQueueAccess ::
  (Member (Embed IO) r) =>
  Maybe Env ->
  Sem (BackendNotificationQueueAccess ': r) a ->
  Sem r a
interpretBackendNotificationQueueAccess mEnv = interpret $ \case
  EnqueueNotification deliveryMode remote action -> runError do
    env <- note FederationNotConfigured mEnv
    embed $ enqueueSingleNotification env (tDomain remote) deliveryMode action
  EnqueueNotificationsConcurrently m xs rpc -> runError do
    embed $ enqueueNotificationsConcurrently mEnv m xs rpc
  EnqueueNotificationsConcurrentlyBuckets m xs rpc -> runError do
    embed $ enqueueNotificationsConcurrentlyBuckets mEnv m xs rpc

enqueueSingleNotification :: Env -> Domain -> Q.DeliveryMode -> FedQueueClient c a -> IO a
enqueueSingleNotification env remoteDomain deliveryMode action = do
  let ownDomain = tDomain env.local
  let policy = limitRetries 3 <> constantDelay 1_000_000
      handlers =
        skipAsyncExceptions
          <> [logRetries (const $ pure True) logError]
  recovering policy handlers (const $ go ownDomain)
  where
    logError willRetry (SomeException e) status = do
      Log.err env.logger $
        Log.msg @Text "failed to enqueue notification in RabbitMQ"
          . Log.field "error" (displayException e)
          . Log.field "willRetry" willRetry
          . Log.field "retryCount" status.rsIterNumber
          . Log.field "request" env.requestId
    go ownDomain = do
      mChan <- timeout 1_000_000 (readMVar env.channelMVar)
      case mChan of
        Nothing -> throwM NoRabbitMqChannel
        Just chan -> do
          liftIO $ enqueue chan env.requestId ownDomain remoteDomain deliveryMode action

enqueueNotificationsConcurrently ::
  (Foldable f, Functor f) =>
  Maybe Env ->
  Q.DeliveryMode ->
  f (Remote x) ->
  (Remote [x] -> FedQueueClient c a) ->
  IO [Remote a]
enqueueNotificationsConcurrently env m xs f =
  enqueueNotificationsConcurrentlyBuckets env m (bucketRemote xs) f

enqueueNotificationsConcurrentlyBuckets ::
  (Foldable f) =>
  Maybe Env ->
  Q.DeliveryMode ->
  f (Remote x) ->
  (Remote x -> FedQueueClient c a) ->
  IO [Remote a]
enqueueNotificationsConcurrentlyBuckets mEnv m xs f = do
  case toList xs of
    -- only attempt to get a channel if there is at least one notification to send
    [] -> pure []
    _ -> do
      case mEnv of
        Nothing -> throwM FederationNotConfigured
        Just env ->
          pooledForConcurrentlyN 8 (toList xs) $ \r ->
            qualifyAs r
              <$> enqueueSingleNotification env (tDomain r) m (f r)

data NoRabbitMqChannel = NoRabbitMqChannel
  deriving (Show)

instance Exception NoRabbitMqChannel
