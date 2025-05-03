{-# LANGUAGE TemplateHaskell #-}

module Wire.UserGroupStore where

import Data.Id
import Data.UUID
import Imports
import Polysemy
import Wire.API.User.Profile
import Wire.API.UserGroup

data UserGroupStore m a where
  CreateUserGroup :: TeamId -> NewUserGroup -> ManagedBy -> UserGroupStore m UserGroupId
  GetUserGroup :: TeamId -> UserGroupId -> UserGroupStore m (Maybe UserGroup)
  GetUserGroups :: TeamId -> Maybe Int -> Maybe UUID -> UserGroupStore m UserGroupPage
  UpdateUserGroup :: TeamId -> UserGroupId -> UserGroupUpdate -> UserGroupStore m (Maybe UserGroup)
  DeleteUserGroup :: TeamId -> UserGroupId -> UserGroupStore m ()
  AddUser :: TeamId -> UserGroupId -> UserId -> UserGroupStore m ()
  RemoveUser :: TeamId -> UserGroupId -> UserId -> UserGroupStore m ()

makeSem ''UserGroupStore
