{-# LANGUAGE TemplateHaskell #-}

module Wire.UserGroupStore where

import Data.Id
import Data.Json.Util
import Data.Vector
import Imports
import Polysemy
import Wire.API.Pagination
import Wire.API.User.Profile
import Wire.API.UserGroup
import Wire.API.UserGroup.Pagination

data UserGroupPageRequest = UserGroupPageRequest
  { team :: TeamId,
    searchString :: Maybe Text,
    paginationState :: PaginationState,
    sortOrder :: SortOrder,
    pageSize :: PageSize,
    includeMemberCount :: Bool,
    includeChannels :: Bool
  }

data PaginationState = PaginationSortByName (Maybe (UserGroupName, UserGroupId)) | PaginationSortByCreatedAt (Maybe (UTCTimeMillis, UserGroupId))

userGroupCreatedAtPaginationState :: UserGroup_ f -> (UTCTimeMillis, UserGroupId)
userGroupCreatedAtPaginationState ug = (ug.createdAt, ug.id_)

toSortBy :: PaginationState -> SortBy
toSortBy = \case
  PaginationSortByName _ -> SortByName
  PaginationSortByCreatedAt _ -> SortByCreatedAt

data UserGroupStore m a where
  CreateUserGroup :: TeamId -> NewUserGroup -> ManagedBy -> UserGroupStore m UserGroup
  GetUserGroup :: TeamId -> UserGroupId -> Bool -> UserGroupStore m (Maybe UserGroup)
  GetUserGroups :: UserGroupPageRequest -> UserGroupStore m UserGroupPage
  UpdateUserGroup :: TeamId -> UserGroupId -> UserGroupUpdate -> UserGroupStore m (Maybe ())
  DeleteUserGroup :: TeamId -> UserGroupId -> UserGroupStore m (Maybe ())
  AddUser :: UserGroupId -> UserId -> UserGroupStore m ()
  UpdateUsers :: UserGroupId -> Vector UserId -> UserGroupStore m ()
  RemoveUser :: UserGroupId -> UserId -> UserGroupStore m ()
  UpdateUserGroupChannels :: UserGroupId -> Vector ConvId -> UserGroupStore m ()
  GetUserGroupCount :: TeamId -> UserId -> UserGroupStore m Int

makeSem ''UserGroupStore
