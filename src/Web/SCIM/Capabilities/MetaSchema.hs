{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE ConstraintKinds   #-}

module Web.SCIM.Capabilities.MetaSchema (
  ConfigAPI
  , configServer
  , Supported (..)
  , BulkConfig (..)
  , FilterConfig (..)
  , AuthenticationScheme (..)
  , Configuration (..)
  , empty
  ) where

import           Web.SCIM.Capabilities.MetaSchema.User
import           Web.SCIM.Capabilities.MetaSchema.SPConfig
import           Web.SCIM.Capabilities.MetaSchema.Group
import           Web.SCIM.Capabilities.MetaSchema.Schema
import           Web.SCIM.Capabilities.MetaSchema.ResourceType
import           Control.Monad.Except
import           Control.Error.Util (note)
import           Data.Aeson
import qualified Data.HashMap.Lazy as HML
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Servant
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

instance ToJSON BulkConfig

data FilterConfig = FilterConfig
  { maxResults :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON FilterConfig

-- TODO
data AuthenticationScheme = AuthenticationScheme
  { typ :: Text
  , name :: Text
  , description :: Text
  , specUri :: Text -- TODO: URI
  -- , documentationUri :: URI
  } deriving (Show, Eq, Generic)

instance ToJSON AuthenticationScheme

data Configuration = Configuration
  { documentationUri :: Maybe Text -- TODO: URI
  , patch :: Supported ()
  , bulk :: Supported BulkConfig
  , filter :: Supported FilterConfig
  , changePassword :: Supported ()
  , sort :: Supported ()
  , etag :: Supported ()
  , authenticationSchemes :: [AuthenticationScheme]
  } deriving (Show, Eq, Generic)

instance ToJSON Configuration

empty :: Configuration
empty = Configuration
  { documentationUri = Nothing
  , patch = Supported False ()
  , bulk = Supported False $ BulkConfig 0 0
  , filter = Supported False $ FilterConfig 0
  , changePassword = Supported False ()
  , sort = Supported False ()
  , etag = Supported False ()
  , authenticationSchemes = []
  }  

configServer :: MonadError ServantErr m =>
                Configuration -> ConfigAPI (AsServerT m)
configServer config = ConfigAPI
  { spConfig = pure config
  , schemas = pure [ userSchema
                   , spConfigSchema
                   , groupSchema
                   , metaSchema
                   , resourceSchema
                   ]
  , schema = liftEither . note err404 . getSchema
  , resourceTypes = pure ""
  }

getSchema :: Text -> Maybe Value
getSchema "urn:ietf:params:scim:schemas:core:2.0:ServiceProviderConfig" =
  pure spConfigSchema
getSchema "urn:ietf:params:scim:schemas:core:2.0:User" =
  pure userSchema
getSchema "urn:ietf:params:scim:schemas:core:2.0:Group" =
  pure groupSchema
getSchema "urn:ietf:params:scim:schemas:core:2.0:Schema" =
  pure metaSchema
getSchema "urn:ietf:params:scim:schemas:core:2.0:ResourceType" =
  pure resourceSchema
getSchema _ = Nothing 

data ConfigAPI route = ConfigAPI
  { spConfig :: route :- "ServiceProviderConfig" :> Get '[JSON] Configuration
  , schemas :: route :- "Schemas" :> Get '[JSON] [Value]
  , schema :: route :- "Schemas" :> Capture "id" Text :> Get '[JSON] Value
  , resourceTypes :: route :- "ResourceTypes" :> Get '[JSON] Text
  } deriving (Generic)

