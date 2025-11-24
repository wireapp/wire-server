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
import Data.OpenApi qualified as S
import Data.Schema
import GHC.Generics
import Imports
import Wire.API.Pagination
import Wire.API.UserGroup
import Wire.Arbitrary as Arbitrary

-- | User group without members
type UserGroupPage = UserGroupPage_ UserGroupMeta

-- | User group with members
type UserGroupPageWithMembers = UserGroupPage_ UserGroup

-- * User group pages

--

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
