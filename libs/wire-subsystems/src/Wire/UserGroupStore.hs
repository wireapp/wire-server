{-# LANGUAGE TemplateHaskell #-}

module Wire.UserGroupStore where

import Data.Id
import Data.Json.Util
import Imports
import Polysemy
import Wire.API.User.Profile
import Wire.API.UserGroup

data ListUserGroupsQuery = ListUserGroupsQuery
  { team :: TeamId,
    lastRowSent :: Maybe (Text {- name -}, UTCTimeMillis {- created_at -}),
    sortByName :: Bool {- False: created_at -},
    sortDescending :: Bool {- False: ascending -},
    pageSize :: Int {- page size -}
  }

data UserGroupStore m a where
  CreateUserGroup :: TeamId -> NewUserGroup -> ManagedBy -> UserGroupStore m UserGroup
  GetUserGroup :: TeamId -> UserGroupId -> UserGroupStore m (Maybe UserGroup)
  GetUserGroups :: ListUserGroupsQuery -> UserGroupStore m [UserGroup]
  UpdateUserGroup :: TeamId -> UserGroupId -> UserGroupUpdate -> UserGroupStore m (Maybe ())
  DeleteUserGroup :: TeamId -> UserGroupId -> UserGroupStore m (Maybe ())
  AddUser :: UserGroupId -> UserId -> UserGroupStore m ()
  RemoveUser :: UserGroupId -> UserId -> UserGroupStore m ()

makeSem ''UserGroupStore
