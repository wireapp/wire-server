{-# LANGUAGE TemplateHaskell #-}

module Galley.Effects.BackendNotificationQueueAccess where

import Data.Qualified
import Imports
import qualified Network.AMQP as Q
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

makeSem ''BackendNotificationQueueAccess
