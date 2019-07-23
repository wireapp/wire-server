{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main (main) where

import Imports
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
  test_normalizeWaiRequestRoute'

test_treeLookup :: IO ()
test_treeLookup = unless (want == haveNaive && want == haveReal) $ do
    error $ show (want, haveNaive, haveReal)
  where
    want = Just "/a/:d/c"

    haveNaive = treeLookupNaive a b
    haveReal  = treeLookup (fromRight undefined $ mkTree a) b

    a = [["a", "b", "c"], ["a", ":d", "c"]]
    b = ["a", "d", "c"]

test_normalizeWaiRequestRoute' :: IO ()
test_normalizeWaiRequestRoute' = unless (want == have) . error $ show (want, have)
  where
    want = "/i/users/:id/reauthenticate"
    have = normalizeWaiRequestRoute' brigPaths "oops" ["i","users","bla","reauthenticate"]

brigPaths :: Paths
brigPaths = Paths [Node {rootLabel = Right "i", subForest = [Node {rootLabel = Right "users", subForest = [Node {rootLabel = Left ":uid", subForest = [Node {rootLabel = Right "sso-id", subForest = []},Node {rootLabel = Right "rich-info", subForest = []},Node {rootLabel = Right "managed-by", subForest = []},Node {rootLabel = Right "is-team-owner", subForest = [Node {rootLabel = Left ":tid", subForest = []}]},Node {rootLabel = Right "can-be-deleted", subForest = [Node {rootLabel = Left ":tid", subForest = []}]}]},Node {rootLabel = Left ":id", subForest = [Node {rootLabel = Right "status", subForest = []},Node {rootLabel = Right "reauthenticate", subForest = []},Node {rootLabel = Right "contacts", subForest = []},Node {rootLabel = Right "auto-connect", subForest = []}]}]}]}]
