{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Wire.UserGroupSubsystem where

import Data.Id
import Data.Json.Util (UTCTimeMillis)
import Data.Vector (Vector)
import Imports
import Polysemy
import Wire.API.Pagination
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
    Maybe UserGroupName ->
    Maybe UTCTimeMillis ->
    Maybe UserGroupId ->
    Bool ->
    UserGroupSubsystem m UserGroupPage
  UpdateGroup :: UserId -> UserGroupId -> UserGroupUpdate -> UserGroupSubsystem m ()
  DeleteGroup :: UserId -> UserGroupId -> UserGroupSubsystem m ()
  AddUser :: UserId -> UserGroupId -> UserId -> UserGroupSubsystem m ()
  AddUsers :: UserId -> UserGroupId -> Vector UserId -> UserGroupSubsystem m ()
  UpdateUsers :: UserId -> UserGroupId -> Vector UserId -> UserGroupSubsystem m ()
  RemoveUser :: UserId -> UserGroupId -> UserId -> UserGroupSubsystem m ()

makeSem ''UserGroupSubsystem
