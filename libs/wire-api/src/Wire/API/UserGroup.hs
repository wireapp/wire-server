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

-- request bodies
data NewUserGroup = NewUserGroup
  { name :: Text,
    members :: [UserId]
  }
  deriving (Eq, Show, Generic)

data UserGroupUpdate = UserGroupUpdate
  { name :: Text
  }
  deriving (Eq, Show, Generic)

-- response body
data UserGroup = UserGroup
  { id_ :: UserGroupId,
    name :: Text,
    members :: [UserId],
    managedBy :: ManagedBy,
    createdAt :: UTCTime
  }
  deriving (Eq, Show, Generic)

-- | About pagination: We have 'MultiTablePage', "Wire.Sem.Paging", 'Page' from cql, in-type
-- paging, and probably lots more.  i wonder if we should make up our minds and pick one?
data UserGroupPage = UserGroupPage
  { page :: [UserGroup],
    hasMore :: Bool,
    pagingState :: Maybe Text
  }
  deriving (Eq, Show, Generic)
