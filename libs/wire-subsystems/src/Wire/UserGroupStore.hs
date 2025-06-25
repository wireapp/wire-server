{-# LANGUAGE TemplateHaskell #-}

module Wire.UserGroupStore where

import Data.Id
import Imports
import Polysemy
import Wire.API.User.Profile
import Wire.API.UserGroup

data ListUserGroupsQuery a = ListUserGroupsQuery
  { team :: TeamId,
    lastRowSent :: Maybe a,
    searchString :: Maybe Text, -- TODO: is it ok to have this in here?  the rest is general-purpose pagination, can we somehow make it less redundant?
    sortByName :: Bool {- False: created_at -},
    sortDescending :: Bool {- False: ascending -},
    pageSize :: Int {- page size -}
  }
  deriving (Eq, Show)

data UserGroupStore m a where
  CreateUserGroup :: TeamId -> NewUserGroup -> ManagedBy -> UserGroupStore m UserGroup
  GetUserGroup :: TeamId -> UserGroupId -> UserGroupStore m (Maybe UserGroup)
  GetUserGroups :: ListUserGroupsQuery UserGroupKey -> UserGroupStore m [UserGroup]
  UpdateUserGroup :: TeamId -> UserGroupId -> UserGroupUpdate -> UserGroupStore m (Maybe ())
  DeleteUserGroup :: TeamId -> UserGroupId -> UserGroupStore m (Maybe ())
  AddUser :: UserGroupId -> UserId -> UserGroupStore m ()
  RemoveUser :: UserGroupId -> UserId -> UserGroupStore m ()

makeSem ''UserGroupStore
