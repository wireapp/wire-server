
module Web.SCIM.Schema.User.IM where

import Data.Text (Text)
import Data.Aeson
import GHC.Generics

import Web.SCIM.Schema.Common

data IM = IM
  { typ :: Maybe Text
  , value :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON IM where
  parseJSON = genericParseJSON parseOptions . jsonLower

instance ToJSON IM where
  toJSON = genericToJSON serializeOptions
