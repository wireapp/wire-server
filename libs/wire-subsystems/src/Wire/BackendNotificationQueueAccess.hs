-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.BackendNotificationQueueAccess where

import Data.Qualified
import Imports
import Network.AMQP qualified as Q
import Polysemy
import Polysemy.Error
import Wire.API.Federation.BackendNotifications
import Wire.API.Federation.Component
import Wire.API.Federation.Error

data BackendNotificationQueueAccess m a where
  EnqueueNotification ::
    (KnownComponent c) =>
    Q.DeliveryMode ->
    Remote x ->
    FedQueueClient c a ->
    BackendNotificationQueueAccess m (Either FederationError a)
  EnqueueNotificationsConcurrently ::
    (KnownComponent c, Foldable f, Functor f) =>
    Q.DeliveryMode ->
    f (Remote x) ->
    (Remote [x] -> FedQueueClient c a) ->
    BackendNotificationQueueAccess m (Either FederationError [Remote a])
  EnqueueNotificationsConcurrentlyBuckets ::
    (KnownComponent c, Foldable f, Functor f) =>
    Q.DeliveryMode ->
    f (Remote x) ->
    (Remote x -> FedQueueClient c a) ->
    BackendNotificationQueueAccess m (Either FederationError [Remote a])

enqueueNotification ::
  ( KnownComponent c,
    Member (Error FederationError) r,
    Member BackendNotificationQueueAccess r
  ) =>
  Q.DeliveryMode ->
  Remote x ->
  FedQueueClient c a ->
  Sem r a
enqueueNotification m r q = send (EnqueueNotification m r q) >>= either throw pure

enqueueNotificationsConcurrently ::
  ( KnownComponent c,
    Foldable f,
    Functor f,
    Member (Error FederationError) r,
    Member BackendNotificationQueueAccess r
  ) =>
  Q.DeliveryMode ->
  f (Remote x) ->
  (Remote [x] -> FedQueueClient c a) ->
  Sem r [Remote a]
enqueueNotificationsConcurrently m r q =
  send (EnqueueNotificationsConcurrently m r q)
    >>= either throw pure

enqueueNotificationsConcurrentlyBuckets ::
  ( KnownComponent c,
    Foldable f,
    Functor f,
    Member (Error FederationError) r,
    Member BackendNotificationQueueAccess r
  ) =>
  Q.DeliveryMode ->
  f (Remote x) ->
  (Remote x -> FedQueueClient c a) ->
  Sem r [Remote a]
enqueueNotificationsConcurrentlyBuckets m r q =
  send (EnqueueNotificationsConcurrentlyBuckets m r q) >>= either throw pure
