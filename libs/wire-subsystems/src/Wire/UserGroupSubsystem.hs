{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Wire.UserGroupSubsystem where

import Data.Id
import Imports
import Polysemy
import Wire.API.UserGroup
import Wire.API.UserGroup.Pagination

data UserGroupSubsystem m a where
  CreateGroup :: UserId -> NewUserGroup -> UserGroupSubsystem m UserGroup
  GetGroup :: UserId -> UserGroupId -> UserGroupSubsystem m (Maybe UserGroup)
  GetGroups ::
    UserId ->
    Maybe Text ->
    Maybe SortBy ->
    Maybe SortOrder ->
    Maybe PageSize ->
    Maybe PaginationState ->
    UserGroupSubsystem m UserGroupPage
  UpdateGroup :: UserId -> UserGroupId -> UserGroupUpdate -> UserGroupSubsystem m ()
  DeleteGroup :: UserId -> UserGroupId -> UserGroupSubsystem m ()
  AddUser :: UserId -> UserGroupId -> UserId -> UserGroupSubsystem m ()
  RemoveUser :: UserId -> UserGroupId -> UserId -> UserGroupSubsystem m ()

makeSem ''UserGroupSubsystem
