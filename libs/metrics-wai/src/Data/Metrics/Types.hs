{-# LANGUAGE OverloadedStrings #-}

module Data.Metrics.Types where

import Imports
import Data.Tree as Tree

import qualified Data.ByteString as BS


newtype PathTemplate = PathTemplate Text

-- | A 'Forest' of path segments.  A path segment is 'Nothing' if it captures a value
-- (e.g. user id).
newtype Paths = Paths (Forest (Maybe ByteString))
  deriving (Eq, Show)

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
