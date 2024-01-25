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

import Data.Kind
import GHC.TypeLits
import Wire.API.Federation.Component
import Wire.API.Federation.Version

class IsNotificationTag k where
  type NotificationComponent k = (c :: Component) | c -> k

class HasNotificationEndpoint t where
  -- | The type of the payload for this endpoint
  type Payload t :: Type

  -- | The central path component of a notification endpoint, e.g.,
  -- "on-conversation-updated".
  type NotificationPath t :: Symbol

  -- | The federation API version range this endpoint is supported in.
  versionRange :: VersionRange
