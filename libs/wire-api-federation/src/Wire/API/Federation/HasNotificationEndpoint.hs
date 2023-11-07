-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2023 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.Federation.HasNotificationEndpoint where

import Data.Aeson
import Data.Domain
import Data.Kind
import Data.Proxy
import Data.Text qualified as T
import GHC.TypeLits
import Imports
import Wire.API.Federation.BackendNotifications
import Wire.API.Federation.Component
import Wire.API.RawJson

class IsNotificationTag k where
  type NotificationComponent k = (c :: Component) | c -> k

class HasNotificationEndpoint t where
  -- | The type of the payload for this endpoint
  type Payload t :: Type

  -- | The central path component of a notification endpoint, e.g.,
  -- "on-conversation-updated".
  type NotificationPath t :: Symbol

-- | Convert a federation endpoint to a backend notification to be enqueued to a
-- RabbitMQ queue.
fedNotifToBackendNotif ::
  forall {k} (tag :: k).
  KnownSymbol (NotificationPath tag) =>
  KnownComponent (NotificationComponent k) =>
  ToJSON (Payload tag) =>
  Domain ->
  Payload tag ->
  BackendNotification
fedNotifToBackendNotif ownDomain payload =
  let p = T.pack . symbolVal $ Proxy @(NotificationPath tag)
      b = RawJson . encode $ payload
   in toNotif p b
  where
    toNotif :: Text -> RawJson -> BackendNotification
    toNotif path body =
      BackendNotification
        { ownDomain = ownDomain,
          targetComponent = componentVal @(NotificationComponent k),
          path = path,
          body = body
        }
