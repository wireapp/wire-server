module Web.Scim.Schema.User.Certificate where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Web.Scim.Schema.Common

data Certificate = Certificate
  { typ :: Maybe Text,
    value :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON Certificate where
  parseJSON = genericParseJSON parseOptions . jsonLower

instance ToJSON Certificate where
  toJSON = genericToJSON serializeOptions
