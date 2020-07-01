module Web.Scim.Schema.User.Photo where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Web.Scim.Schema.Common

data Photo
  = Photo
      { typ :: Maybe Text,
        value :: Maybe URI
      }
  deriving (Show, Eq, Generic)

instance FromJSON Photo where
  parseJSON = genericParseJSON parseOptions . jsonLower

instance ToJSON Photo where
  toJSON = genericToJSON serializeOptions
