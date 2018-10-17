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

instance FromJSON ResourceType where
  parseJSON = withText "ResourceType" $ \case
    "User" -> pure UserResource
    "Group" -> pure GroupResource
    other -> fail ("unknown ResourceType: " ++ show other)

-- | Definitions of endpoints, returned by @/ResourceTypes@.
data Resource = Resource
  { name :: Text
  , endpoint :: URI
  , schema :: Schema
  } deriving (Show, Eq, Generic)

instance ToJSON Resource where
  toJSON = genericToJSON serializeOptions

instance FromJSON Resource where
  parseJSON = genericParseJSON parseOptions . jsonLower

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
