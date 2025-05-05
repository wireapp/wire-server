{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Wire.UserGroupSubsystem where

import Data.Id
import Imports
import Polysemy
import Wire.API.UserGroup

data UserGroupSubsystem m a where
  -- TODO: why not local user?
  CreateGroup :: UserId -> NewUserGroup -> UserGroupSubsystem m UserGroup
  GetGroup :: UserId -> UserGroupId -> UserGroupSubsystem m (Maybe UserGroup)
  GetGroups :: UserId -> Maybe Int -> Maybe UserGroupId -> UserGroupSubsystem m UserGroupPage
  UpdateGroup :: UserId -> UserGroupId -> UserGroupUpdate -> UserGroupSubsystem m (Maybe UserGroup)
  DeleteGroup :: UserId -> UserGroupId -> UserGroupSubsystem m ()
  AddUser :: UserId -> UserGroupId -> UserId -> UserGroupSubsystem m ()
  RemoveUser :: UserId -> UserGroupId -> UserId -> UserGroupSubsystem m ()

makeSem ''UserGroupSubsystem
