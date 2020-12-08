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
import qualified Data.HashMap.Lazy as HML
import Data.Text (Text)
import Servant hiding (URI)
import Servant.API.Generic
import Servant.Server.Generic
import Web.Scim.Capabilities.MetaSchema.Group
import Web.Scim.Capabilities.MetaSchema.ResourceType
import Web.Scim.Capabilities.MetaSchema.SPConfig
import Web.Scim.Capabilities.MetaSchema.Schema
import Web.Scim.Capabilities.MetaSchema.User
import Web.Scim.ContentType
import Web.Scim.Schema.AuthenticationScheme
import Web.Scim.Schema.Common
import Web.Scim.Schema.Error hiding (schemas)
import Web.Scim.Schema.ListResponse as ListResponse hiding (schemas)
import Web.Scim.Schema.ResourceType hiding (schema)
import Web.Scim.Schema.Schema
import Prelude hiding (filter)

data Supported a = Supported
  { supported :: Bool,
    subConfig :: a
  }
  deriving (Show, Eq, Generic)

instance ToJSON a => ToJSON (Supported a) where
  toJSON (Supported b v) = case toJSON v of
    (Object o) -> Object $ HML.insert "supported" (Bool b) o
    _ -> Object $ HML.fromList [("supported", Bool b)]

data BulkConfig = BulkConfig
  { maxOperations :: Int,
    maxPayloadSize :: Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON BulkConfig where
  toJSON = genericToJSON serializeOptions

data FilterConfig = FilterConfig
  { maxResults :: Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON FilterConfig where
  toJSON = genericToJSON serializeOptions

data Configuration = Configuration
  { documentationUri :: Maybe URI,
    schemas :: [Schema],
    patch :: Supported (),
    bulk :: Supported BulkConfig,
    filter :: Supported FilterConfig,
    changePassword :: Supported (),
    sort :: Supported (),
    etag :: Supported (),
    authenticationSchemes :: [AuthenticationSchemeEncoding]
  }
  deriving (Show, Eq, Generic)

instance ToJSON Configuration where
  toJSON = genericToJSON serializeOptions

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
      patch = Supported True (),
      bulk = Supported False $ BulkConfig 0 0,
      filter = Supported False $ FilterConfig 0,
      changePassword = Supported False (),
      sort = Supported False (),
      etag = Supported False (),
      authenticationSchemes = [authHttpBasicEncoding]
    }

configServer ::
  Monad m =>
  Configuration ->
  ConfigSite (AsServerT m)
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
        Nothing -> respond (NotFound "Schema" uri)
        Just s -> respond (WithStatus @200 s),
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
    schema :: route :- "Schemas" :> Capture "id" Text :> UVerb 'GET '[SCIM] [WithStatus 200 Value, NotFound],
    resourceTypes :: route :- "ResourceTypes" :> Get '[SCIM] (ListResponse Resource)
  }
  deriving (Generic)
