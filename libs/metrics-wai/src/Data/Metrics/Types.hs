{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

import Data.ByteString.Char8 qualified as BS
import Data.Tree as Tree
import Imports

-- | The string used to represent the route within metrics e.g. the prometheus label
newtype PathTemplate = PathTemplate Text

-- | A 'Forest' of path segments.  A path segment is 'Left' if it captures a value
-- (e.g. user id).
newtype Paths = Paths (Forest PathSegment)
  deriving (Eq, Show)
  deriving newtype (Semigroup)

type PathSegment = Either ByteString ByteString

-- | Turn a list of paths into a 'Paths' tree.  Treat all path segments that start with @':'@
-- as equal and turn them into 'Nothing'.
mkTree :: [[ByteString]] -> Either String Paths
mkTree = fmap (Paths . meltTree) . mapM mkbranch . sortBy (flip compare) . fmap (fmap mknode)
  where
    mkbranch :: [PathSegment] -> Either String (Tree PathSegment)
    mkbranch (seg : segs@(_ : _)) = Node seg . (: []) <$> mkbranch segs
    mkbranch [seg] = Right $ Node seg []
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
--
-- This lookup will do its best when it sees ambiguous paths like
-- /users/:uid/clients and /users/:domain/:uid. In this case, for input
-- ["users", "some-uuid", "clients"] it will see that /users/:uid/clients has
-- more verbatim matches than /users/:domain/:uid and so, it will prefer
-- /users/:uid/clients.
--
-- This is not the case all the time, for instance if there are two paths like:
-- /users/clients/:cid and /users/:uid/clients and the input is ["users",
-- "clients", "clients"], the lookup will see exactly the same number of
-- verbatim matches and return any one of them. This may not really be a
-- problem, because we may not have such a request in our real paths.
--
-- Note [Trees for Metrics]
--
-- The use of trees hides information about which of the
-- partial paths are real paths. Consider a tree like this:
--
-- /users
--   /:uid
--   /:domain
--     /:uid
--   /clients
--     /:uid
--
-- Here, it is impossible to tell if /users or /users/:domain is a valid
-- endpoint or not, so this function will always return these as a match, even
-- if in reality there is no such API. This smells like we shouldn't be using
-- trees here and instead just try to match a given path with list of routes.
treeLookup :: Paths -> [ByteString] -> Maybe ByteString
treeLookup (Paths forest) = mungeSegments <=< go forest
  where
    go :: Forest PathSegment -> [ByteString] -> Maybe [PathSegment]
    go _trees [] = Just []
    go [] _segs = Nothing
    go trees (seg : segs) =
      let allMatches = mapMaybe (matchTree seg segs) trees
          sorted = sortOn (Down . length . filter isRight) allMatches
       in listToMaybe sorted

    matchTree :: ByteString -> [ByteString] -> Tree PathSegment -> Maybe [PathSegment]
    matchTree seg segs tree =
      if seg `fits` tree
        then (rootLabel tree :) <$> go (subForest tree) segs
        else Nothing

    fits :: ByteString -> Tree PathSegment -> Bool
    fits _ (Node (Left _) _) = True
    fits seg (Node (Right seg') _) = seg == seg'

    mungeSegments path = Just . ("/" <>) . BS.intercalate "/" . fmap (either id id) $ path
