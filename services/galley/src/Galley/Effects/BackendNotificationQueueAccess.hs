{-# LANGUAGE TemplateHaskell #-}

module Galley.Effects.BackendNotificationQueueAccess where

import Data.Qualified
import Imports
import Network.AMQP qualified as Q
import Polysemy
import Wire.API.Federation.BackendNotifications
import Wire.API.Federation.Component
import Wire.API.Federation.Error

data BackendNotificationQueueAccess m a where
  EnqueueNotification ::
    KnownComponent c =>
    Remote x ->
    Q.DeliveryMode ->
    FedQueueClient c () ->
    BackendNotificationQueueAccess m (Either FederationError ())
  EnqueueNotificationsConcurrently ::
    (KnownComponent c, Foldable f, Functor f) =>
    Q.DeliveryMode ->
    f (Remote x) ->
    (Remote [x] -> (FedQueueClient c (), b)) ->
    BackendNotificationQueueAccess m [Either (Remote [x], FederationError) (Remote b)]

makeSem ''BackendNotificationQueueAccess
