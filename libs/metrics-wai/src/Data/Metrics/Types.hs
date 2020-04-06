{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Data.Metrics.Types
  ( PathTemplate (..),
    Paths (..),
    PathSegment,
    mkTree,
    meltTree,
    treeLookup,
  )
where

import qualified Data.ByteString.Char8 as BS
import Data.Tree as Tree
import Imports

-- | The string used to represent the route within metrics e.g. the prometheus label
newtype PathTemplate = PathTemplate Text

-- | A 'Forest' of path segments.  A path segment is 'Left' if it captures a value
-- (e.g. user id).
newtype Paths = Paths (Forest PathSegment)
  deriving (Eq, Show)

type PathSegment = Either ByteString ByteString

-- | Turn a list of paths into a 'Paths' tree.  Treat all path segments that start with @':'@
-- as equal and turn them into 'Nothing'.
mkTree :: [[ByteString]] -> Either String Paths
mkTree = fmap (Paths . meltTree) . mapM mkbranch . sortBy (flip compare) . fmap (fmap mknode)
  where
    mkbranch :: [PathSegment] -> Either String (Tree PathSegment)
    mkbranch (seg : segs@(_ : _)) = Node seg . (: []) <$> mkbranch segs
    mkbranch (seg : []) = Right $ Node seg []
    mkbranch [] = Left "internal error: path with on segments."
    mknode :: ByteString -> PathSegment
    mknode seg = if BS.head seg /= ':' then Right seg else Left seg

-- | If two sibling nodes in the forest are equal, make them one node that shares their
-- subtrees.
meltTree :: Forest PathSegment -> Forest PathSegment
meltTree = go
  where
    go = fmap push . groupBy ((==) `on` rootLabel)
    push [] = error "violation of groupBy invariant: empty list!"
    push trees@(Node r _ : _) = Node r (meltTree . mconcat $ subForest <$> trees)

-- | A variant of 'Network.Wai.Route.Tree.lookup'.  The segments contain values to be captured
-- when running the 'App', here we simply replace them with their identifier;
-- e.g. @/user/1234@ might become @/user/userid@
treeLookup :: Paths -> [ByteString] -> Maybe ByteString
treeLookup (Paths forest) = go [] forest
  where
    go :: [PathSegment] -> Forest PathSegment -> [ByteString] -> Maybe ByteString
    go path _ [] = Just . ("/" <>) . BS.intercalate "/" . fmap (either id id) . reverse $ path
    go _ [] _ = Nothing
    go path trees (seg : segs) =
      find (seg `fits`) trees >>= \(Node root trees') -> go (root : path) trees' segs
    fits :: ByteString -> Tree PathSegment -> Bool
    fits _ (Node (Left _) _) = True
    fits seg (Node (Right seg') _) = seg == seg'
