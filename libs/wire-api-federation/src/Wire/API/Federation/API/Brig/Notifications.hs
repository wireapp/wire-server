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

module Wire.API.Federation.API.Brig.Notifications where

import Data.Aeson
import Data.Id
import Data.OpenApi (ToSchema)
import Data.Range
import Imports
import Wire.API.Federation.Component
import Wire.API.Federation.Endpoint
import Wire.API.Federation.HasNotificationEndpoint
import Wire.API.Util.Aeson
import Wire.Arbitrary

type UserDeletedNotificationMaxConnections = 1000

data UserDeletedConnectionsNotification = UserDeletedConnectionsNotification
  { -- | This is qualified implicitly by the origin domain
    user :: UserId,
    -- | These are qualified implicitly by the target domain
    connections :: Range 1 UserDeletedNotificationMaxConnections [UserId]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform UserDeletedConnectionsNotification)
  deriving (FromJSON, ToJSON) via (CustomEncoded UserDeletedConnectionsNotification)

data BrigNotificationTag = OnUserDeletedConnectionsTag
  deriving (Show, Eq, Generic, Bounded, Enum)

instance IsNotificationTag BrigNotificationTag where
  type NotificationComponent _ = 'Brig

instance HasNotificationEndpoint 'OnUserDeletedConnectionsTag where
  type Payload 'OnUserDeletedConnectionsTag = UserDeletedConnectionsNotification
  type NotificationPath 'OnUserDeletedConnectionsTag = "on-user-deleted-connections"

instance ToSchema UserDeletedConnectionsNotification

-- | All the notification endpoints return an 'EmptyResponse'.
type BrigNotificationAPI =
  NotificationFedEndpoint 'OnUserDeletedConnectionsTag
