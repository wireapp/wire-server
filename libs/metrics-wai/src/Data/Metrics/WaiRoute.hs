{-# LANGUAGE OverloadedStrings #-}

-- | wai-route-0.4.0 uses a 'Tree' type for route construction.  This module lets you
-- construct the simpler metrics routing tree from that.
module Data.Metrics.WaiRoute where

import Imports
import Data.Metrics.Types
import Network.Wai.Route.Tree


treeToPaths :: Tree a -> Paths
treeToPaths = undefined
