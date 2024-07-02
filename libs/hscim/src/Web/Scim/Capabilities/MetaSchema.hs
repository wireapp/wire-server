-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Web.Scim.Capabilities.MetaSchema
  ( ConfigSite,
    configServer,
    Supported (..),
    BulkConfig (..),
    FilterConfig (..),
    Configuration (..),
    empty,
  )
where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import Data.Typeable (Typeable, cast)
import Servant hiding (URI)
import Servant.API.Generic
import Servant.Server.Generic
import Web.Scim.Capabilities.MetaSchema.Group
import Web.Scim.Capabilities.MetaSchema.ResourceType
import Web.Scim.Capabilities.MetaSchema.SPConfig
import Web.Scim.Capabilities.MetaSchema.Schema
import Web.Scim.Capabilities.MetaSchema.User
import Web.Scim.ContentType
import Web.Scim.Handler
import qualified Web.Scim.Schema.AuthenticationScheme as AuthScheme
import Web.Scim.Schema.Common
import Web.Scim.Schema.Error hiding (schemas)
import Web.Scim.Schema.ListResponse as ListResponse hiding (schemas)
import Web.Scim.Schema.ResourceType hiding (schema)
import Web.Scim.Schema.Schema
import Prelude hiding (filter)

data Supported a = Supported
  { supported :: ScimBool,
    subConfig :: a
  }
  deriving (Show, Eq, Generic)

instance (ToJSON a) => ToJSON (Supported a) where
  toJSON (Supported (ScimBool b) v) = case toJSON v of
    (Object o) -> Object $ KeyMap.insert "supported" (Bool b) o
    _ -> Object $ KeyMap.fromList [("supported", Bool b)]

-- | See module "Test.Schema.MetaSchemaSpec" for golden tests that explain this instance
-- better.
instance (Typeable a, FromJSON a) => FromJSON (Supported a) where
  parseJSON val = do
    Supported
      <$> withObject "Supported a" (.: "supported") val
      <*> let -- allow special case for empty subConfig (`()` does not parse from json objects)
              val' = case cast @() @a () of
                Just _ -> Array mempty
                Nothing -> val
           in parseJSON @a val'

data BulkConfig = BulkConfig
  { maxOperations :: Int,
    maxPayloadSize :: Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON BulkConfig where
  toJSON = genericToJSON serializeOptions

instance FromJSON BulkConfig where
  parseJSON = either (fail . show) (genericParseJSON parseOptions) . jsonLower

data FilterConfig = FilterConfig
  { maxResults :: Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON FilterConfig where
  toJSON = genericToJSON serializeOptions

instance FromJSON FilterConfig where
  parseJSON = either (fail . show) (genericParseJSON parseOptions) . jsonLower

data Configuration = Configuration
  { documentationUri :: Maybe URI,
    schemas :: [Schema],
    patch :: Supported (),
    bulk :: Supported BulkConfig,
    filter :: Supported FilterConfig,
    changePassword :: Supported (),
    sort :: Supported (),
    etag :: Supported (),
    authenticationSchemes :: [AuthScheme.AuthenticationSchemeEncoding]
  }
  deriving (Show, Eq, Generic)

instance ToJSON Configuration where
  toJSON = genericToJSON serializeOptions

instance FromJSON Configuration where
  parseJSON = either (fail . show) (genericParseJSON parseOptions) . jsonLower

empty :: Configuration
empty =
  Configuration
    { documentationUri = Nothing,
      schemas =
        [ User20,
          ServiceProviderConfig20,
          Group20,
          Schema20,
          ResourceType20
        ],
      patch = Supported (ScimBool True) (),
      bulk = Supported (ScimBool False) $ BulkConfig 0 0,
      filter = Supported (ScimBool False) $ FilterConfig 0,
      changePassword = Supported (ScimBool False) (),
      sort = Supported (ScimBool False) (),
      etag = Supported (ScimBool False) (),
      authenticationSchemes = [AuthScheme.authHttpBasicEncoding]
    }

configServer ::
  (Monad m) =>
  Configuration ->
  ConfigSite (AsServerT (ScimHandler m))
configServer config =
  ConfigSite
    { spConfig = pure config,
      getSchemas =
        pure $
          ListResponse.fromList
            [ userSchema,
              spConfigSchema,
              groupSchema,
              metaSchema,
              resourceSchema
            ],
      schema = \uri -> case getSchema (fromSchemaUri uri) of
        Nothing -> throwScim (notFound "Schema" uri)
        Just s -> pure s,
      resourceTypes =
        pure $
          ListResponse.fromList
            [ usersResource,
              groupsResource
            ]
    }

data ConfigSite route = ConfigSite
  { spConfig :: route :- "ServiceProviderConfig" :> Get '[SCIM] Configuration,
    getSchemas :: route :- "Schemas" :> Get '[SCIM] (ListResponse Value),
    schema :: route :- "Schemas" :> Capture "id" Text :> Get '[SCIM] Value,
    resourceTypes :: route :- "ResourceTypes" :> Get '[SCIM] (ListResponse Resource)
  }
  deriving (Generic)
