module Wire.API.CannonId where

import Data.Aeson
import Data.OpenApi
import Data.Proxy
import Imports
import Web.HttpApiData

newtype CannonId = CannonId
  { cannonId :: Text
  }
  deriving (Eq, Ord, Show, FromJSON, ToJSON)

instance ToParamSchema CannonId where
  toParamSchema _ = toParamSchema (Proxy @Text)

instance FromHttpApiData CannonId where
  parseUrlPiece = pure . CannonId
