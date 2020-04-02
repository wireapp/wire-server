module Brig.Types.Team where

import Data.Aeson
import Imports
import Numeric.Natural

newtype TeamSize = TeamSize Natural
  deriving (Show, Eq)

instance ToJSON TeamSize where
  toJSON (TeamSize s) = object ["teamSize" .= s]

instance FromJSON TeamSize where
  parseJSON =
    withObject "TeamSize" $ \o -> TeamSize <$> o .: "teamSize"
