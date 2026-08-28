{-# LANGUAGE StrictData #-}

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

module Wire.API.BackgroundJobs where

import Data.Aeson qualified as Aeson
import Data.Id
import Data.OpenApi qualified as S
import Data.Schema
import Imports
import Wire.Arbitrary (Arbitrary (..), GenericUniform (..))

data BackgroundJobPayload
  = BackgroundJobSyncUserGroupAndChannel SyncUserGroupAndChannel
  | BackgroundJobSyncUserGroup SyncUserGroup
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via GenericUniform BackgroundJobPayload

data SyncUserGroupAndChannel = SyncUserGroupAndChannel
  { teamId :: TeamId,
    userGroupId :: UserGroupId,
    convId :: ConvId,
    actor :: Maybe UserId
  }
  deriving (Show, Eq, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON, S.ToSchema) via (Schema SyncUserGroupAndChannel)
  deriving (Arbitrary) via GenericUniform SyncUserGroupAndChannel

instance ToSchema SyncUserGroupAndChannel where
  schema =
    object $
      SyncUserGroupAndChannel
        <$> (.teamId) .= field "team_id" schema
        <*> (.userGroupId) .= field "user_group_id" schema
        <*> (.convId) .= field "conv_id" schema
        <*> (.actor) .= maybe_ (optField "actor" schema)

data SyncUserGroup = SyncUserGroup
  { teamId :: TeamId,
    userGroupId :: UserGroupId,
    actor :: Maybe UserId
  }
  deriving (Show, Eq, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON, S.ToSchema) via (Schema SyncUserGroup)
  deriving (Arbitrary) via GenericUniform SyncUserGroup

instance ToSchema SyncUserGroup where
  schema =
    object $
      SyncUserGroup
        <$> (.teamId) .= field "team_id" schema
        <*> (.userGroupId) .= field "user_group_id" schema
        <*> (.actor) .= maybe_ (optField "actor" schema)
