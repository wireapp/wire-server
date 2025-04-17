{-# LANGUAGE TemplateHaskell #-}

module Wire.UserGroupStore where

import Imports
import Polysemy
import Wire.API.UserGroup

data UserGroupStore m a where
  CreateUserGroup :: NewUserGroup -> UserGroupStore m UserGroupId
  GetUserGroup :: UserGroupId -> UserGroupStore m (Maybe UserGroup)

makeSem ''UserGroupStore
