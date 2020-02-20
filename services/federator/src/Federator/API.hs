{-# LANGUAGE RecordWildCards #-}

module Federator.API
  ( API (..),
  )
where

import Imports
import Servant.API
import Servant.API.Generic

data API route
  = API
      { _gapiMain ::
          route
            :- Get '[JSON] Int
      }
  deriving (Generic)
