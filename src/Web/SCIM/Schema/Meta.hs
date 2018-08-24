
module Web.SCIM.Schema.Meta where

import Prelude hiding (map)

import Data.Text (Text)
import Data.Aeson
import Web.SCIM.Schema.Common
import GHC.Generics (Generic)
import qualified Data.HashMap.Lazy as HML
import Data.Time.Clock

data ResourceType = UserResource
                  | GroupResource
  deriving (Eq, Show)

instance ToJSON ResourceType where
  toJSON UserResource = "User"
  toJSON GroupResource = "Group"

data ETag = Weak Text | Strong Text
  deriving (Eq, Show)

instance ToJSON ETag where
  toJSON (Weak tag) = String $ "W/\"" `mappend` tag `mappend` "\""
  toJSON (Strong tag) = String $ "\"" `mappend` tag `mappend` "\""

data Meta = Meta
  { resourceType :: ResourceType
  , created :: UTCTime
  , lastModified :: UTCTime
  , version :: ETag
  , location :: URI
  } deriving (Eq, Show, Generic)

instance ToJSON Meta

data WithMeta a = WithMeta
  { meta :: Meta
  , thing :: a
  } deriving (Eq, Show, Generic)

instance (ToJSON a) => ToJSON (WithMeta a) where
  toJSON (WithMeta m v) = case toJSON v of
    (Object o) -> Object (HML.insert "meta" (toJSON m) o)
    other      -> other
