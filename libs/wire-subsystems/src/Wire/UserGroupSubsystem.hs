{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.UserGroupSubsystem where

import Data.Id
import Data.Vector (Vector)
import Imports
import Polysemy
import Wire.API.Routes.Internal.Brig
import Wire.API.User.Profile (ManagedBy)
import Wire.API.UserGroup
import Wire.API.UserGroup.Pagination
import Wire.UserGroupStore (UserGroupPageRequest)

data UserGroupSubsystem m a where
  CreateGroup :: UserId -> NewUserGroup -> UserGroupSubsystem m UserGroup
  GetGroup :: UserId -> UserGroupId -> Bool -> UserGroupSubsystem m (Maybe UserGroup)
  GetGroups :: UserId -> UserGroupPageRequest -> UserGroupSubsystem m UserGroupPage
  UpdateGroup :: UserId -> UserGroupId -> UserGroupUpdate -> UserGroupSubsystem m ()
  DeleteGroup :: UserId -> UserGroupId -> UserGroupSubsystem m ()
  DeleteGroupManaged :: ManagedBy -> TeamId -> UserGroupId -> UserGroupSubsystem m ()
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
  GetGroupsInternal :: TeamId -> Maybe Text -> Maybe Int -> Maybe Int -> UserGroupSubsystem m UserGroupPageWithMembers
  ResetUserGroupInternal :: UpdateGroupInternalRequest -> UserGroupSubsystem m ()

makeSem ''UserGroupSubsystem
