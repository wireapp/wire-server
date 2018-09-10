{-# LANGUAGE QuasiQuotes #-}

module Web.SCIM.Schema.ResourceType where

import Prelude hiding (map)

import Data.Text (Text)
import Data.Aeson

import Web.SCIM.Util
import Web.SCIM.Schema.Common
import Web.SCIM.Schema.Schema (Schema(..))

import GHC.Generics (Generic)

-- | Supported resource types. Each resource type also corresponds to an
-- endpoint, described by 'ResourceTypeEndpoint'.
data ResourceType
  = UserResource
  | GroupResource
  deriving (Show, Eq)

instance ToJSON ResourceType where
  toJSON UserResource = "User"
  toJSON GroupResource = "Group"

-- | Definitions of endpoints, returned by @/ResourceTypes@.
data Resource = Resource
  { name :: Text
  , endpoint :: URI
  , schema :: Schema
  } deriving (Show, Eq, Generic)

instance ToJSON Resource where
  toJSON = genericToJSON serializeOptions

----------------------------------------------------------------------------
-- Available resource endpoints

usersResource :: Resource
usersResource = Resource
  { name = "User"
  , endpoint = URI [relativeUri|/Users|]
  , schema = User20
  }

groupsResource :: Resource
groupsResource = Resource
  { name = "Group"
  , endpoint = URI [relativeUri|/Groups|]
  , schema = Group20
  }
