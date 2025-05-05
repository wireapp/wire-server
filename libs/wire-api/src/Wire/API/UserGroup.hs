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

import Data.Aeson qualified as A
import Data.Id
import Data.Json.Util
import Data.OpenApi qualified as OpenApi
import Data.Schema
import Data.Vector (Vector)
import Imports
import Wire.API.User.Profile
import Wire.Arbitrary

data NewUserGroup = NewUserGroup
  { name :: Text,
    members :: Vector UserId
  }
  deriving (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via GenericUniform NewUserGroup
  deriving (A.ToJSON, A.FromJSON, OpenApi.ToSchema) via Schema NewUserGroup

instance ToSchema NewUserGroup where
  schema =
    object "NewUserGroup" $
      NewUserGroup
        <$> (.name) .= field "name" schema
        <*> (.members) .= field "members" (vector schema)

data UserGroupUpdate = UserGroupUpdate
  { name :: Text
  }
  deriving (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via GenericUniform UserGroupUpdate
  deriving (A.ToJSON, A.FromJSON, OpenApi.ToSchema) via Schema UserGroupUpdate

instance ToSchema UserGroupUpdate where
  schema =
    object "UserGroupUpdate" $
      UserGroupUpdate
        <$> (.name) .= field "name" schema

data UserGroup = UserGroup
  { id_ :: UserGroupId,
    name :: Text,
    members :: Vector UserId,
    managedBy :: ManagedBy,
    createdAt :: UTCTimeMillis
  }
  deriving (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via GenericUniform UserGroup
  deriving (A.ToJSON, A.FromJSON, OpenApi.ToSchema) via Schema UserGroup

instance ToSchema UserGroup where
  schema =
    object "UserGroup" $
      UserGroup
        <$> (.id_) .= field "id" schema
        <*> (.name) .= field "name" schema
        <*> (.members) .= field "members" (vector schema)
        <*> (.managedBy) .= field "managedBy" schema
        <*> (.createdAt) .= field "createdAt" schema

-- | About pagination: We have 'MultiTablePage', "Wire.Sem.Paging", 'Page' from cql, in-type
-- paging, and probably lots more.  i wonder if we should make up our minds and pick one?
data UserGroupPage = UserGroupPage -- TODO: rework this to "data Page payload = ..."
  { page :: [UserGroup],
    hasMore :: Bool
  }
  deriving (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via GenericUniform UserGroupPage
  deriving (A.ToJSON, A.FromJSON, OpenApi.ToSchema) via Schema UserGroupPage

instance ToSchema UserGroupPage where
  schema =
    object "UserGroupPage" $
      UserGroupPage
        <$> (.page) .= field "page" (array schema)
        <*> (.hasMore) .= field "hasMore" schema
