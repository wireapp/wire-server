{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Wire.UserGroupSubsystem where

import Data.Id
import Imports
import Polysemy
import Wire.API.UserGroup

data UserGroupSubsystem m a where
  -- exposed as POST /user-groups
  CreateGroup :: NewUserGroup -> UserGroupSubsystem m UserGroup
  -- exposed as GET /user-groups/:d
  GetGroup :: UserGroupId -> UserGroupSubsystem m (Maybe UserGroup)
  -- exposed as GET /user-groups?pagingState=?
  GetGroups :: Text -> UserGroupSubsystem m UserGroupPage
  -- exposed as PUT /user-groups/:id
  UpdateGroup :: UserGroupId -> UserGroupUpdate -> UserGroupSubsystem m UserGroup
  -- exposed as DELETE /user-groups/:id
  DeleteGroup :: UserGroupId -> UserGroupSubsystem m ()
  -- exposed as POST /user-groups/:id/users/:uid
  AddUser :: UserGroupId -> UserId -> UserGroupSubsystem m ()
  -- exposed as DELETE /user-groups/:id/users/:uid
  RemoveUser :: UserGroupId -> UserId -> UserGroupSubsystem m ()

makeSem ''UserGroupSubsystem
