module Galley.Effects.BackendNotificationQueueAccess where

import Data.Qualified
import Imports
import Polysemy
import Polysemy.Error
import Wire.API.Federation.BackendNotifications
import Wire.API.Federation.Component
import Wire.API.Federation.Error

data BackendNotificationQueueAccess m a where
  EnqueueNotification ::
    (KnownComponent c) =>
    Remote x ->
    FedQueueClient c a ->
    BackendNotificationQueueAccess m (Either FederationError a)
  EnqueueNotificationsConcurrently ::
    (KnownComponent c, Foldable f, Functor f) =>
    f (Remote x) ->
    (Remote [x] -> FedQueueClient c a) ->
    BackendNotificationQueueAccess m (Either FederationError [Remote a])
  EnqueueNotificationsConcurrentlyBuckets ::
    (KnownComponent c, Foldable f, Functor f) =>
    f (Remote x) ->
    (Remote x -> FedQueueClient c a) ->
    BackendNotificationQueueAccess m (Either FederationError [Remote a])

enqueueNotification ::
  ( KnownComponent c,
    Member (Error FederationError) r,
    Member BackendNotificationQueueAccess r
  ) =>
  Remote x ->
  FedQueueClient c a ->
  Sem r a
enqueueNotification r q = send (EnqueueNotification r q) >>= either throw pure

enqueueNotificationsConcurrently ::
  ( KnownComponent c,
    Foldable f,
    Functor f,
    Member (Error FederationError) r,
    Member BackendNotificationQueueAccess r
  ) =>
  f (Remote x) ->
  (Remote [x] -> FedQueueClient c a) ->
  Sem r [Remote a]
enqueueNotificationsConcurrently r q =
  send (EnqueueNotificationsConcurrently r q)
    >>= either throw pure

enqueueNotificationsConcurrentlyBuckets ::
  ( KnownComponent c,
    Foldable f,
    Functor f,
    Member (Error FederationError) r,
    Member BackendNotificationQueueAccess r
  ) =>
  f (Remote x) ->
  (Remote x -> FedQueueClient c a) ->
  Sem r [Remote a]
enqueueNotificationsConcurrentlyBuckets r q =
  send (EnqueueNotificationsConcurrentlyBuckets r q) >>= either throw pure
