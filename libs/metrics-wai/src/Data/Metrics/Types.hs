{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

module Data.Metrics.Types
    ( PathTemplate(..)
    , Paths(..)
    , PathSegment
    , mkTree
    , treeLookup
    ) where

import Imports
import Data.Tree as Tree

import qualified Data.ByteString.Char8 as BS

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
mkTree = fmap (Paths . melt) . mapM mkbranch . sortBy (flip compare) . fmap (fmap mknode)
  where
    mkbranch :: [PathSegment] -> Either String (Tree PathSegment)
    mkbranch (seg : segs@(_:_)) = Node seg . (:[]) <$> mkbranch segs
    mkbranch (seg : [])         = Right $ Node seg []
    mkbranch []                 = Left "internal error: path with on segments."

    mknode :: ByteString -> PathSegment
    mknode seg = if BS.head seg /= ':' then Right seg else Left seg

    melt :: Forest PathSegment -> Forest PathSegment
    melt [] = []
    melt (tree : []) = [tree]
    melt (tree : tree' : trees) = if rootLabel tree == rootLabel tree'
        then let tree'' = Node (rootLabel tree) (melt $ subForest tree <> subForest tree')
             in melt (tree'' : trees)
        else tree : melt (tree' : trees)

-- | A variant of 'Network.Wai.Route.Tree.lookup'.  The segments contain values to be captured
-- when running the 'App', here we simply replace them with their identifier;
-- e.g. @/user/1234@ might become @/user/userid@
treeLookup :: Paths -> [ByteString] -> Maybe ByteString
treeLookup (Paths forest) = go [] forest
  where
    go :: [PathSegment] -> Forest PathSegment -> [ByteString] -> Maybe ByteString
    go path _  [] = Just . ("/" <>) . BS.intercalate "/" . fmap (either id id) . reverse $ path
    go _    [] _  = Nothing

    go path trees (seg : segs) =
        find (seg `fits`) trees >>= \(Node root trees') -> go (root : path) trees' segs

    fits :: ByteString -> Tree PathSegment -> Bool
    fits _ (Node (Left _) _) = True
    fits seg (Node (Right seg') _) = seg == seg'
