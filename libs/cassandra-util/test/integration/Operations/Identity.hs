module Operations.Identity where

import Imports
import Data.Text (Text)
import Data.Functor.Identity (Identity)
import Test.Tasty
import Test.Tasty.HUnit
import Cassandra (Value(CqlText), QueryParams(QueryParams), QueryString, R, Consistency(One), runClient, query, toCql)

import qualified Cassandra                         as DB

tests :: DB.ClientState -> IO TestTree
tests db = do
  return $ testGroup "Identity" [
    testCase "Identity works" $ testIdentity db @?= [ toCql "3.4.4" ] 
    ]

testIdentity :: DB.ClientState -> IO [Value]
testIdentity db = do
  let q = "SELECT cql_version from system.local" :: QueryString R () (Value)
  let p = QueryParams One False () Nothing Nothing Nothing Nothing
  runClient db (query q p)
