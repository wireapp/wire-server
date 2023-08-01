module Test.Federator where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Testlib.Prelude

runFederatorMetrics :: Service -> App ()
runFederatorMetrics service = do
  let req = submit "GET" =<< rawBaseRequest OwnDomain service Unversioned "/i/metrics"
      handleRes res = do
        res.status `shouldMatchInt` 200
        pure res
  first <- bindResponse req handleRes
  second <- bindResponse req handleRes
  assertBool "Two metric requests should never match" $ first.body /= second.body
  assertBool "Second metric response should never be 0 length (the first might be)" $ BS.length second.body /= 0
  assertBool "The seconds metric response should have text indicating that it is returning metrics" $
    BS.isInfixOf (BS8.pack expectedString) second.body
  where
    expectedString = "# TYPE http_request_duration_seconds histogram"

-- The metrics setup for both internal and external federator servers
-- are the same, so we can simply run the same test for both.
testFederatorMetricsInternal :: App ()
testFederatorMetricsInternal = runFederatorMetrics FederatorInternal

testFederatorMetricsExternal :: App ()
testFederatorMetricsExternal = runFederatorMetrics FederatorExternal
