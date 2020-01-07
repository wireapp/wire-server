module Web.Scim.Capabilities.MetaSchema (
  ConfigSite
  , configServer
  , Supported (..)
  , BulkConfig (..)
  , FilterConfig (..)
  , Configuration (..)
  , empty
  ) where

import           Web.Scim.Schema.Schema
import           Web.Scim.Schema.Common
import           Web.Scim.Schema.Error hiding (schemas)
import           Web.Scim.Schema.ResourceType hiding (schema)
import           Web.Scim.Schema.AuthenticationScheme
import           Web.Scim.Schema.ListResponse as ListResponse hiding (schemas)
import           Web.Scim.Capabilities.MetaSchema.User
import           Web.Scim.Capabilities.MetaSchema.SPConfig
import           Web.Scim.Capabilities.MetaSchema.Group
import           Web.Scim.Capabilities.MetaSchema.Schema
import           Web.Scim.Capabilities.MetaSchema.ResourceType
import           Web.Scim.ContentType
import           Web.Scim.Handler
import           Data.Aeson
import qualified Data.HashMap.Lazy as HML
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Servant hiding (URI)
import           Servant.API.Generic
import           Servant.Server.Generic

import           Prelude hiding (filter)

data Supported a = Supported
  { supported :: Bool
  , subConfig :: a
  } deriving (Show, Eq, Generic)

instance ToJSON a => ToJSON (Supported a) where
  toJSON (Supported b v) = case toJSON v of
    (Object o) -> Object $ HML.insert "supported" (Bool b) o
    _          -> Object $ HML.fromList [("supported", Bool b)]


data BulkConfig = BulkConfig
  { maxOperations :: Int
  , maxPayloadSize :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON BulkConfig where
  toJSON = genericToJSON serializeOptions

data FilterConfig = FilterConfig
  { maxResults :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON FilterConfig where
  toJSON = genericToJSON serializeOptions

data Configuration = Configuration
  { documentationUri :: Maybe URI
  , schemas :: [Schema]
  , patch :: Supported ()
  , bulk :: Supported BulkConfig
  , filter :: Supported FilterConfig
  , changePassword :: Supported ()
  , sort :: Supported ()
  , etag :: Supported ()
  , authenticationSchemes :: [AuthenticationSchemeEncoding]
  } deriving (Show, Eq, Generic)

instance ToJSON Configuration where
  toJSON = genericToJSON serializeOptions

empty :: Configuration
empty = Configuration
  { documentationUri = Nothing
  , schemas = [ User20
              , ServiceProviderConfig20
              , Group20
              , Schema20
              , ResourceType20
              ]
  , patch = Supported True ()
  , bulk = Supported False $ BulkConfig 0 0
  , filter = Supported False $ FilterConfig 0
  , changePassword = Supported False ()
  , sort = Supported False ()
  , etag = Supported False ()
  , authenticationSchemes = [authHttpBasicEncoding]
  }

configServer
  :: Monad m
  => Configuration -> ConfigSite (AsServerT (ScimHandler m))
configServer config = ConfigSite
  { spConfig = pure config
  , getSchemas = pure $
      ListResponse.fromList [ userSchema
                            , spConfigSchema
                            , groupSchema
                            , metaSchema
                            , resourceSchema
                            ]
  , schema = \uri -> case getSchema (fromSchemaUri uri) of
      Nothing -> throwScim (notFound "Schema" uri)
      Just s  -> pure s
  , resourceTypes = pure $
      ListResponse.fromList [ usersResource
                            , groupsResource
                            ]
  }

data ConfigSite route = ConfigSite
  { spConfig :: route :- "ServiceProviderConfig" :> Get '[SCIM] Configuration
  , getSchemas :: route :- "Schemas" :> Get '[SCIM] (ListResponse Value)
  , schema :: route :- "Schemas" :> Capture "id" Text :> Get '[SCIM] Value
  , resourceTypes :: route :- "ResourceTypes" :> Get '[SCIM] (ListResponse Resource)
  } deriving (Generic)
