{-# LANGUAGE QuasiQuotes #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Web.Scim.Schema.ResourceType where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.URI.Static
import Web.Scim.Schema.Common
import Web.Scim.Schema.Schema (Schema (..))
import Prelude hiding (map)

-- | Supported resource types. Each resource type also corresponds to an
-- endpoint, described by 'ResourceTypeEndpoint'.
data ResourceType
  = UserResource
  | GroupResource
  deriving (Show, Eq)

instance ToJSON ResourceType where
  toJSON UserResource = "User"
  toJSON GroupResource = "Group"

instance FromJSON ResourceType where
  parseJSON = withText "ResourceType" $ \case
    "User" -> pure UserResource
    "Group" -> pure GroupResource
    other -> fail ("unknown ResourceType: " ++ show other)

-- | Definitions of endpoints, returned by @/ResourceTypes@.
data Resource = Resource
  { name :: Text,
    endpoint :: URI,
    schema :: Schema
  }
  deriving (Show, Eq, Generic)

instance ToJSON Resource where
  toJSON = genericToJSON serializeOptions

instance FromJSON Resource where
  parseJSON = genericParseJSON parseOptions . jsonLower

----------------------------------------------------------------------------
-- Available resource endpoints

usersResource :: Resource
usersResource =
  Resource
    { name = "User",
      endpoint = URI [relativeReference|/Users|],
      schema = User20
    }

groupsResource :: Resource
groupsResource =
  Resource
    { name = "Group",
      endpoint = URI [relativeReference|/Groups|],
      schema = Group20
    }
