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

module Wire.API.Team.Member.Info where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Id (UserId)
import Data.Json.Util
import Data.OpenApi qualified as S
import Data.Schema
import Imports
import Wire.API.Team.Permission

data TeamMemberInfo = TeamMemberInfo
  { userId :: UserId,
    permissions :: Permissions,
    permissionsWriteTime :: UTCTimeMillis
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema TeamMemberInfo)

instance ToSchema TeamMemberInfo where
  schema =
    object "TeamMemberInfo" $
      TeamMemberInfo
        <$> (.userId) .= field "userId" schema
        <*> (.permissions) .= field "permissions" schema
        <*> (.permissionsWriteTime) .= field "permissionsWriteTime" schema

newtype TeamMemberInfoList = TeamMemberInfoList
  { members :: [TeamMemberInfo]
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema TeamMemberInfoList)

instance ToSchema TeamMemberInfoList where
  schema =
    object "TeamMemberInfoList" $
      TeamMemberInfoList
        <$> (.members) .= field "members" (array schema)
