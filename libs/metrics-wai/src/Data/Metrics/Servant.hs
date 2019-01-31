{-# LANGUAGE OverloadedStrings #-}

-- | Given a servant API type, this module gives you a 'Paths' for 'withPathTemplate'.
module Data.Metrics.Servant where

-- import Imports
import Data.Metrics.Types
-- import Servant


class RoutesToPaths routes where
  routesToPaths :: routes -> Paths
