{-# LANGUAGE QuasiQuotes #-}

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
data Resource
  = Resource
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
