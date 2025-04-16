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

module Wire.API.UserGroup
  ( module Wire.API.UserGroup,
    UserGroupId,
  )
where

import Data.Id
import Data.Time
import Imports
import Wire.API.User.Profile
import Wire.Arbitrary

-- request bodies
data NewUserGroup = NewUserGroup
  { name :: Text,
    members :: [UserId]
  }
  deriving (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via GenericUniform NewUserGroup

data UserGroupUpdate = UserGroupUpdate
  { name :: Text
  }
  deriving (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via GenericUniform UserGroupUpdate

-- response bodies
data UserGroup = UserGroup
  { id_ :: UserGroupId,
    name :: Text,
    members :: [UserId],
    managedBy :: ManagedBy,
    createdAt :: UTCTime
  }
  deriving (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via GenericUniform UserGroup

-- | About pagination: We have 'MultiTablePage', "Wire.Sem.Paging", 'Page' from cql, in-type
-- paging, and probably lots more.  i wonder if we should make up our minds and pick one?
data UserGroupPage = UserGroupPage
  { page :: [UserGroup],
    hasMore :: Bool
  }
  deriving (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via GenericUniform UserGroupPage
