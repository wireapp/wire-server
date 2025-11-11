{-# LANGUAGE TemplateHaskell #-}

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

module Wire.UserGroupStore where

import Data.Id
import Data.Json.Util
import Data.Time.Clock
import Data.Vector
import Imports
import Polysemy
import Wire.API.Pagination
import Wire.API.User.Profile
import Wire.API.UserGroup
import Wire.API.UserGroup.Pagination
import Wire.PaginationState

data UserGroupPageRequest = UserGroupPageRequest
  { team :: TeamId,
    searchString :: Maybe Text,
    paginationState :: PaginationState UserGroupId,
    sortOrder :: SortOrder,
    pageSize :: PageSize,
    includeMemberCount :: Bool,
    includeChannels :: Bool
  }

userGroupCreatedAtPaginationState :: UserGroup_ f -> (UTCTime, UserGroupId)
userGroupCreatedAtPaginationState ug = (fromUTCTimeMillis ug.createdAt, ug.id_)

toSortBy :: PaginationState UserGroupId -> SortBy
toSortBy = \case
  PaginationSortByName _ -> SortByName
  PaginationSortByCreatedAt _ -> SortByCreatedAt

data UserGroupStore m a where
  CreateUserGroup :: TeamId -> NewUserGroup -> ManagedBy -> UserGroupStore m UserGroup
  GetUserGroup :: TeamId -> UserGroupId -> Bool -> UserGroupStore m (Maybe UserGroup)
  GetUserGroups :: UserGroupPageRequest -> UserGroupStore m UserGroupPage
  GetUserGroupsForConv :: ConvId -> UserGroupStore m (Vector UserGroup)
  UpdateUserGroup :: TeamId -> UserGroupId -> UserGroupUpdate -> UserGroupStore m (Maybe ())
  DeleteUserGroup :: TeamId -> UserGroupId -> UserGroupStore m (Maybe ())
  AddUser :: UserGroupId -> UserId -> UserGroupStore m ()
  UpdateUsers :: UserGroupId -> Vector UserId -> UserGroupStore m ()
  RemoveUser :: UserGroupId -> UserId -> UserGroupStore m ()
  AddUserGroupChannels :: UserGroupId -> Vector ConvId -> UserGroupStore m ()
  UpdateUserGroupChannels :: UserGroupId -> Vector ConvId -> UserGroupStore m ()
  GetUserGroupIdsForUsers :: [UserId] -> UserGroupStore m (Map UserId [UserGroupId])
  GetUserGroupChannels :: TeamId -> UserGroupId -> UserGroupStore m (Maybe (Vector ConvId))

makeSem ''UserGroupStore
