module Wire.API.CannonId where

import Data.Aeson
import Imports
import Web.HttpApiData

newtype CannonId = CannonId
  { cannonId :: Text
  }
  deriving (Eq, Ord, Show, FromJSON, ToJSON)

instance FromHttpApiData CannonId where
  parseUrlPiece = pure . CannonId
