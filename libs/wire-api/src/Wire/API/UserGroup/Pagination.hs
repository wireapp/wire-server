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

module Wire.API.UserGroup.Pagination where

import Data.Aeson qualified as A
import Data.Default
import Data.OpenApi qualified as S
import Data.Schema
import Data.Time.Clock
import GHC.Generics
import Imports
import Wire.API.Pagination
import Wire.API.User.Profile
import Wire.API.UserGroup
import Wire.Arbitrary as Arbitrary

-- | Request for a paginated list of user groups.
--
-- (This is not technically API, but since it is used by several
-- different wire-subsystems we've moved it here anyway.)
data UserGroupPageRequest = UserGroupPageRequest
  { searchString :: Maybe Text,
    managedByFilter :: Maybe ManagedBy,
    paginationState :: PaginationState UserGroupId,
    sortOrder :: SortOrder,
    pageSize :: PageSize,
    includeMemberCount :: Bool,
    includeChannels :: Bool
  }

instance Default UserGroupPageRequest where
  def =
    UserGroupPageRequest
      { searchString = Nothing,
        managedByFilter = Nothing,
        paginationState = PaginationSortByCreatedAt Nothing, -- default sort by is 'createdAt', with no state
        sortOrder = Desc,
        pageSize = def, -- default is 15
        includeMemberCount = True,
        includeChannels = False
      }

data PaginationState a
  = PaginationSortByName (Maybe (Text, a))
  | PaginationSortByCreatedAt (Maybe (UTCTime, a))
  | PaginationOffset Word

-- | User group without members
type UserGroupPage = UserGroupPage_ UserGroupMeta

-- | User group with members
type UserGroupPageWithMembers = UserGroupPage_ UserGroup

-- * User group pages

-- | User group pages with different types of user groups.
data UserGroupPage_ a = UserGroupPage
  { page :: [a],
    total :: Int
  }
  deriving (Eq, Show, Generic)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via Schema (UserGroupPage_ a)

instance (ToSchema a) => ToSchema (UserGroupPage_ a) where
  schema =
    objectWithDocModifier "UserGroupPage" addPageDocs $
      UserGroupPage
        <$> page .= field "page" (array schema)
        <*> total .= field "total" schema

instance (Arbitrary a) => Arbitrary (UserGroupPage_ a) where
  arbitrary = UserGroupPage <$> arbitrary <*> arbitrary
