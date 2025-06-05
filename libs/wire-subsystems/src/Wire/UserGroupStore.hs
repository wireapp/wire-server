{-# LANGUAGE TemplateHaskell #-}

module Wire.UserGroupStore where

import Data.Id
import Imports
import Polysemy
import Wire.API.User.Profile
import Wire.API.UserGroup

data UserGroupStore m a where
  CreateUserGroup :: TeamId -> NewUserGroup -> ManagedBy -> UserGroupStore m UserGroup
  GetUserGroup :: TeamId -> UserGroupId -> UserGroupStore m (Maybe UserGroup)
  UpdateUserGroup :: TeamId -> UserGroupId -> UserGroupUpdate -> UserGroupStore m (Maybe ())
  DeleteUserGroup :: TeamId -> UserGroupId -> UserGroupStore m (Maybe ())
  AddUser :: UserGroupId -> UserId -> UserGroupStore m ()
  RemoveUser :: UserGroupId -> UserId -> UserGroupStore m ()

makeSem ''UserGroupStore
