
module Web.SCIM.Schema.User.Phone where

import Data.Text hiding (dropWhile)
import Data.Aeson
import GHC.Generics (Generic)
import Web.SCIM.Schema.Common

data Phone = Phone
  { typ :: Maybe Text
  , value :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON Phone where
  parseJSON = genericParseJSON parseOptions . jsonLower

instance ToJSON Phone where
  toJSON = genericToJSON serializeOptions
