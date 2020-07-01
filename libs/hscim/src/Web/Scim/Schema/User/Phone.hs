module Web.Scim.Schema.User.Phone where

import Data.Aeson
import Data.Text hiding (dropWhile)
import GHC.Generics (Generic)
import Web.Scim.Schema.Common

data Phone = Phone
  { typ :: Maybe Text,
    value :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON Phone where
  parseJSON = genericParseJSON parseOptions . jsonLower

instance ToJSON Phone where
  toJSON = genericToJSON serializeOptions
