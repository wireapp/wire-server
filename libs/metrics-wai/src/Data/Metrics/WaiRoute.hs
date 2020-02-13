{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- | wai-route-0.4.0 uses a 'Tree' type for route construction.  This module lets you
-- construct the simpler metrics routing tree from that.
module Data.Metrics.WaiRoute where

import Data.Metrics.Types
import Imports
import Network.Wai.Route.Tree as Tree

treeToPaths :: HasCallStack => Tree a -> Paths
treeToPaths = either error id . mkTree . fmap (Tree.segments . path) . Tree.toList
