
module Web.Scim.Schema.User.Name where

import Data.Text (Text)
import Data.Aeson
import GHC.Generics
import Web.Scim.Schema.Common

data Name = Name
  { formatted :: Maybe Text
  , familyName :: Maybe Text
  , givenName :: Maybe Text
  , middleName :: Maybe Text
  , honorificPrefix :: Maybe Text
  , honorificSuffix :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON Name where
  parseJSON = genericParseJSON parseOptions . jsonLower

instance ToJSON Name where
  toJSON = genericToJSON serializeOptions
