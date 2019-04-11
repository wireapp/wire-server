module Operations.Identity where

import Imports
import Data.Text (Text)
import Data.Functor.Identity (Identity)
import Test.Tasty
import Test.Tasty.HUnit
import Cassandra (QueryParams(QueryParams), QueryString, R, Consistency(One), runClient, query)

import qualified Cassandra as DB

tests :: DB.ClientState -> IO TestTree
tests db = do
  return $ testGroup "Identity" [
    testCase "Identity works" $ do
        out <- testIdentity db
        assertEqual "CQL version match." "Identity 3.4.4" (show out)
        --test @? "CQL version mismatch"
    ]

testIdentity :: DB.ClientState -> IO [Identity Text]
testIdentity db = do
  let identityQuery = "SELECT cql_version from system.local" :: QueryString R () (Identity Text)
  let identityParams = QueryParams One False () Nothing Nothing Nothing Nothing
  runClient db $ query identityQuery identityParams
