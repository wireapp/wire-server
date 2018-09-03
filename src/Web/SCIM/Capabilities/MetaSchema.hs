{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE ConstraintKinds   #-}

module Web.SCIM.Capabilities.MetaSchema (
  ConfigSite
  , configServer
  , Supported (..)
  , BulkConfig (..)
  , FilterConfig (..)
  , Configuration (..)
  , empty
  ) where

import           Web.SCIM.Schema.Schema
import           Web.SCIM.Schema.Common
import           Web.SCIM.Schema.AuthenticationScheme
import           Web.SCIM.Schema.ListResponse as ListResponse
import           Web.SCIM.Capabilities.MetaSchema.User
import           Web.SCIM.Capabilities.MetaSchema.SPConfig
import           Web.SCIM.Capabilities.MetaSchema.Group
import           Web.SCIM.Capabilities.MetaSchema.Schema
import           Web.SCIM.Capabilities.MetaSchema.ResourceType
import           Web.SCIM.ContentType
import           Control.Monad.Except
import           Control.Error.Util (note)
import           Data.Aeson
import qualified Data.HashMap.Lazy as HML
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Servant hiding (URI)
import           Servant.Generic

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
  , authenticationSchemes :: [AuthenticationScheme]
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
  , patch = Supported False ()
  , bulk = Supported False $ BulkConfig 0 0
  , filter = Supported False $ FilterConfig 0
  , changePassword = Supported False ()
  , sort = Supported False ()
  , etag = Supported False ()
  , authenticationSchemes = [AuthHttpBasic]
  }

configServer :: MonadError ServantErr m =>
                Configuration -> ConfigSite (AsServerT m)
configServer config = ConfigSite
  { spConfig = pure config
  , getSchemas = pure $
      ListResponse.fromList [ userSchema
                            , spConfigSchema
                            , groupSchema
                            , metaSchema
                            , resourceSchema
                            ]
  , schema = either throwError pure . note err404 . (getSchema <=< fromSchemaUri)
  , resourceTypes = pure ""
  }

data ConfigSite route = ConfigSite
  { spConfig :: route :- "ServiceProviderConfig" :> Get '[SCIM] Configuration
  , getSchemas :: route :- "Schemas" :> Get '[SCIM] (ListResponse Value)
  , schema :: route :- "Schemas" :> Capture "id" Text :> Get '[SCIM] Value
  , resourceTypes :: route :- "ResourceTypes" :> Get '[SCIM] Text
  } deriving (Generic)
