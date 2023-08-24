{-# LANGUAGE NumericUnderscores #-}

module Galley.Intra.BackendNotificationQueue (interpretBackendNotificationQueueAccess) where

import Control.Lens (view)
import Control.Monad.Catch
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
import UnliftIO.Timeout (timeout)
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
    embedApp $ enqueueNotification (tDomain remote) deliveryMode action

enqueueNotification :: Domain -> Q.DeliveryMode -> FedQueueClient c () -> App (Either FederationError ())
enqueueNotification remoteDomain deliveryMode action = do
  mChanVar <- view rabbitmqChannel
  ownDomain <- view (options . settings . federationDomain)
  case mChanVar of
    Nothing -> pure (Left FederationNotConfigured)
    Just chanVar -> do
      let policy = limitRetries 3 <> constantDelay 1_000_000
          handlers =
            skipAsyncExceptions
              <> [logRetries (const $ pure True) logError]
      Right <$> recovering policy handlers (const $ go ownDomain chanVar)
  where
    logError willRetry (SomeException e) status = do
      Log.err $
        Log.msg @Text "failed to enqueue notification in RabbitMQ"
          . Log.field "error" (displayException e)
          . Log.field "willRetry" willRetry
          . Log.field "retryCount" status.rsIterNumber
    go ownDomain chanVar = do
      mChan <- timeout 1_000_000 (readMVar chanVar)
      case mChan of
        Nothing -> throwM NoRabbitMqChannel
        Just chan -> do
          liftIO $ enqueue chan ownDomain remoteDomain deliveryMode action

data NoRabbitMqChannel = NoRabbitMqChannel
  deriving (Show)

instance Exception NoRabbitMqChannel
