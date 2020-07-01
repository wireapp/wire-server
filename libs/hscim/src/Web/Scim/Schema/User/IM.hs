module Web.Scim.Schema.User.IM where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Web.Scim.Schema.Common

data IM
  = IM
      { typ :: Maybe Text,
        value :: Maybe Text
      }
  deriving (Show, Eq, Generic)

instance FromJSON IM where
  parseJSON = genericParseJSON parseOptions . jsonLower

instance ToJSON IM where
  toJSON = genericToJSON serializeOptions
