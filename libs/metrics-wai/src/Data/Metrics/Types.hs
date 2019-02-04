{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

module Data.Metrics.Types
    ( PathTemplate(..)
    , Paths(..)
    , mkTree
    , treeLookup
    ) where

import Imports
import Data.Tree as Tree

import qualified Data.ByteString.Char8 as BS


newtype PathTemplate = PathTemplate Text

-- | A 'Forest' of path segments.  A path segment is 'Nothing' if it captures a value
-- (e.g. user id).
newtype Paths = Paths (Forest (Maybe ByteString))
  deriving (Eq, Show)

-- | Turn a list of paths into a 'Paths' tree.  Treat all path segments that start with @':'@
-- as equal and turn them into 'Nothing'.
mkTree :: forall m. (m ~ Either String) => [[ByteString]] -> m Paths
mkTree = fmap (Paths . melt) . mapM mkbranch . sortBy (flip compare) . fmap (fmap mknode)
  where
    mkbranch :: [Maybe ByteString] -> m (Tree (Maybe ByteString))
    mkbranch (seg : segs@(_:_)) = Node seg . (:[]) <$> mkbranch segs
    mkbranch (seg : [])         = Right $ Node seg []
    mkbranch []                 = Left "internal error: path with on segments."

    mknode :: ByteString -> Maybe ByteString
    mknode seg = if BS.head seg /= ':' then Just seg else Nothing

    melt :: Forest (Maybe ByteString) -> Forest (Maybe ByteString)
    melt [] = []
    melt (tree : []) = [tree]
    melt (tree : tree' : trees) = if rootLabel tree == rootLabel tree'
        then let tree'' = Node (rootLabel tree) (melt $ subForest tree <> subForest tree')
             in melt (tree'' : trees)
        else tree : melt (tree' : trees)

-- | A variant of 'Network.Wai.Route.Tree.lookup'.  The segments contain values to be captured
-- when running the 'App', but here we simply replace them with @"<>"@.
treeLookup :: Paths -> [ByteString] -> Maybe ByteString
treeLookup (Paths forest) = go [] forest
  where
    go :: [Maybe ByteString] -> Forest (Maybe ByteString) -> [ByteString] -> Maybe ByteString
    go path _  [] = Just . BS.intercalate "/" . fmap (fromMaybe "<>") . reverse $ path
    go _    [] _  = Nothing

    go path trees (seg : segs) =
        find (seg `fits`) trees >>= \(Node root trees') -> go (root : path) trees' segs

    fits :: ByteString -> Tree (Maybe ByteString) -> Bool
    fits _ (Node Nothing _) = True
    fits seg (Node (Just seg') _) = seg == seg'
