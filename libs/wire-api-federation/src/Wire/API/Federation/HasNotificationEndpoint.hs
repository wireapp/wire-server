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

module Wire.API.Federation.HasNotificationEndpoint
  ( IsNotificationTag (..),
    HasNotificationEndpoint (..),
    HasFedPath,
    HasVersionRange,
    fedPath,
    versionRange,
  )
where

import Data.Kind
import Data.Proxy
import Data.Singletons
import GHC.TypeLits
import Imports
import Wire.API.Federation.Component
import Wire.API.Federation.Version
import Wire.API.Routes.Version (From, Until)

class IsNotificationTag k where
  type NotificationComponent k = (c :: Component) | c -> k

class HasNotificationEndpoint t where
  -- | The type of the payload for this endpoint
  type Payload t = (p :: Type) | p -> t

  -- | The central path component of a notification endpoint, e.g.,
  -- "on-conversation-updated".
  type NotificationPath t :: Symbol

  -- | An optional version tag to distinguish different versions of the same
  -- endpoint.
  type NotificationVersionTag t :: Maybe Version

  type NotificationVersionTag t = 'Nothing

  type NotificationMods t :: [Type]

  type NotificationMods t = '[]

type HasFedPath t = KnownSymbol (NotificationPath t)

type HasVersionRange t = MkVersionRange (NotificationMods t)

fedPath :: forall t. (HasFedPath t) => String
fedPath = symbolVal (Proxy @(NotificationPath t))

-- | Build a version range using any 'Until' and 'From' combinators present in
-- the endpoint modifiers.
class MkVersionRange mods where
  mkVersionRange :: VersionRange

instance MkVersionRange '[] where
  mkVersionRange = allVersions

instance
  {-# OVERLAPPING #-}
  (MkVersionRange mods, SingI v) =>
  MkVersionRange (From (v :: Version) ': mods)
  where
  mkVersionRange = mkVersionRange @mods <> rangeFromVersion (demote @v)

instance
  {-# OVERLAPPING #-}
  (MkVersionRange mods, SingI v) =>
  MkVersionRange (Until (v :: Version) ': mods)
  where
  mkVersionRange = mkVersionRange @mods <> rangeUntilVersion (demote @v)

instance {-# OVERLAPPABLE #-} (MkVersionRange mods) => MkVersionRange (m ': mods) where
  mkVersionRange = mkVersionRange @mods

-- | The federation API version range this endpoint is supported in.
versionRange :: forall t. (HasVersionRange t) => VersionRange
versionRange = mkVersionRange @(NotificationMods t)
