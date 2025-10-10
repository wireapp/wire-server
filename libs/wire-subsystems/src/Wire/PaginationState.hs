module Wire.PaginationState (PaginationState (..)) where

import Data.Time.Clock
import Imports

data PaginationState id
  = PaginationSortByName (Maybe (Text, id))
  | PaginationSortByCreatedAt (Maybe (UTCTime, id))
