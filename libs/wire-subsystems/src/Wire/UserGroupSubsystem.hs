{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Wire.UserGroupSubsystem where

import Data.Default
import Data.Id
import Data.Time.Clock
import Data.Vector (Vector)
import Imports
import Polysemy
import Wire.API.Pagination
import Wire.API.Routes.Internal.Brig
import Wire.API.User.Profile (ManagedBy)
import Wire.API.UserGroup
import Wire.API.UserGroup.Pagination

data GroupSearch = GroupSearch
  { query :: Maybe Text,
    sortBy :: Maybe SortBy,
    sortOrder :: Maybe SortOrder,
    pageSize :: Maybe PageSize,
    lastName :: Maybe Text,
    lastCreatedAt :: Maybe UTCTime,
    lastId :: Maybe UserGroupId,
    includeMemberCount :: Bool,
    includeChannels :: Bool
  }

instance Default GroupSearch where
  def =
    GroupSearch
      { query = Nothing,
        sortBy = Nothing,
        sortOrder = Nothing,
        pageSize = Nothing,
        lastName = Nothing,
        lastCreatedAt = Nothing,
        lastId = Nothing,
        includeMemberCount = False,
        includeChannels = False
      }

data UserGroupSubsystem m a where
  CreateGroup :: UserId -> NewUserGroup -> UserGroupSubsystem m UserGroup
  GetGroup :: UserId -> UserGroupId -> Bool -> UserGroupSubsystem m (Maybe UserGroup)
  GetGroups :: UserId -> GroupSearch -> UserGroupSubsystem m UserGroupPage
  UpdateGroup :: UserId -> UserGroupId -> UserGroupUpdate -> UserGroupSubsystem m ()
  DeleteGroup :: UserId -> UserGroupId -> UserGroupSubsystem m ()
  AddUser :: UserId -> UserGroupId -> UserId -> UserGroupSubsystem m ()
  AddUsers :: UserId -> UserGroupId -> Vector UserId -> UserGroupSubsystem m ()
  UpdateUsers :: UserId -> UserGroupId -> Vector UserId -> UserGroupSubsystem m ()
  RemoveUser :: UserId -> UserGroupId -> UserId -> UserGroupSubsystem m ()
  RemoveUserFromAllGroups :: UserId -> TeamId -> UserGroupSubsystem m ()
  AddChannels :: UserId -> UserGroupId -> Vector ConvId -> UserGroupSubsystem m ()
  UpdateChannels :: UserId -> UserGroupId -> Vector ConvId -> UserGroupSubsystem m ()
  -- Internal API handlers
  CreateGroupInternal :: ManagedBy -> TeamId -> Maybe UserId -> NewUserGroup -> UserGroupSubsystem r UserGroup
  GetGroupInternal :: TeamId -> UserGroupId -> Bool -> UserGroupSubsystem m (Maybe UserGroup)
  ResetUserGroupInternal :: UpdateGroupInternalRequest -> UserGroupSubsystem m ()

makeSem ''UserGroupSubsystem
