{-# LANGUAGE TemplateHaskell #-}

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
