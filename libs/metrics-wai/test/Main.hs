{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main (main) where

import Imports
import Control.Exception
import Data.Metrics.Middleware.Prometheus
import Data.Metrics.Types
import Data.Metrics.WaiRoute
import Data.Tree
import Network.Wai
import Network.Wai.Routing.Route


-- | FUTUREWORK: once we have a second test case, introduce hspec or tasty or whatever.
main :: IO ()
main = do
  test_treeLookup
  test_treeLookup2
  test_normalizeWaiRequestRoute'

test_treeLookup :: IO ()
test_treeLookup = unless (want == haveNaive && want == haveReal) $ do
    error $ show (want, haveNaive, haveReal)
  where
    want = Just "/a/:f/q"

    haveNaive = treeLookupNaive a b
    haveReal  = treeLookup (fromRight undefined $ mkTree a) b

    a = [["a", "b", "c"], ["a", ":d", "c", ":w"], ["a", ":f", "q"]]
    b = ["a", "f", "q"]

test_treeLookup2 :: IO ()
test_treeLookup2 = unless (want == haveNaive && want == haveReal) $ do
    error $ show (want, haveNaive, haveReal)
  where
    want = Just "/i/users/:id/reauthenticate"

    haveNaive = treeLookupNaive a b
    haveReal  = treeLookup (fromRight undefined $ mkTree a) b

    a = fmap (either id id) <$> y
    b = ["i","users","bla","reauthenticate"]

test_normalizeWaiRequestRoute' :: IO ()
test_normalizeWaiRequestRoute' = unless (want == have) . error $ show (want, have)
  where
    want = "/i/users/:id/reauthenticate"
    have = normalizeWaiRequestRoute' brigPaths "oops" ["i","users","bla","reauthenticate"]

brigPaths' :: Paths
brigPaths' = Paths [Node {rootLabel = Right "i", subForest = [Node {rootLabel = Right "users", subForest = [Node {rootLabel = Left ":uid", subForest = [Node {rootLabel = Right "sso-id", subForest = []},Node {rootLabel = Right "rich-info", subForest = []},Node {rootLabel = Right "managed-by", subForest = []},Node {rootLabel = Right "is-team-owner", subForest = [Node {rootLabel = Left ":tid", subForest = []}]},Node {rootLabel = Right "can-be-deleted", subForest = [Node {rootLabel = Left ":tid", subForest = []}]}]},Node {rootLabel = Left ":id", subForest = [Node {rootLabel = Right "status", subForest = []},Node {rootLabel = Right "reauthenticate", subForest = []},Node {rootLabel = Right "contacts", subForest = []},Node {rootLabel = Right "auto-connect", subForest = []}]}]}]}]

brigPaths :: Paths
brigPaths = assert (good == brigPaths') smaller
  where
    Right good = mkTree $ fmap (either id id) <$> x
    Right smaller = mkTree $ fmap (either id id) <$> y


_untree :: Forest a -> [[a]]
_untree [] = []
_untree (Node r [] : f) = [r] : _untree f
_untree (Node r t : f) = ((r:) <$> _untree t) <> _untree f


x :: [[PathSegment]]
x = [ [Right "i",Right "users",Left ":uid",Right "sso-id"]
    , [Right "i",Right "users",Left ":uid",Right "rich-info"]
    , [Right "i",Right "users",Left ":uid",Right "managed-by"]
    , [Right "i",Right "users",Left ":uid",Right "is-team-owner",Left ":tid"]
    , [Right "i",Right "users",Left ":uid",Right "can-be-deleted",Left ":tid"]
    , [Right "i",Right "users",Left ":id",Right "status"]
    , [Right "i",Right "users",Left ":id",Right "reauthenticate"]
    , [Right "i",Right "users",Left ":id",Right "contacts"]
    , [Right "i",Right "users",Left ":id",Right "auto-connect"]
    ]

y :: [[PathSegment]]
y = [ [Right "i",Right "users",Left ":uid",Right "can-be-deleted",Left ":tid"]
    , [Right "i",Right "users",Left ":id",Right "reauthenticate"]
    ]
