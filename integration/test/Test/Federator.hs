module Test.Federator where

import qualified Data.ByteString as BS
import Data.String.Conversions
import qualified Network.HTTP.Client as HTTP
import Testlib.Prelude

runFederatorMetrics :: (ServiceMap -> HostPort) -> App ()
runFederatorMetrics getService = do
  let req = submit "GET" =<< rawBaseRequestF OwnDomain getService "/i/metrics"
      handleRes res = res <$ res.status `shouldMatchInt` 200
  first <- bindResponse req handleRes
  second <- bindResponse req handleRes
  assertBool "Two metric requests should never match" $ first.body /= second.body
  assertBool "Second metric response should never be 0 length (the first might be)" $ BS.length second.body /= 0
  assertBool "The seconds metric response should have text indicating that it is returning metrics" $
    BS.isInfixOf (cs expectedString) second.body
  where
    expectedString = "# TYPE http_request_duration_seconds histogram"

rawBaseRequestF :: (HasCallStack, MakesValue domain) => domain -> (ServiceMap -> HostPort) -> String -> App HTTP.Request
rawBaseRequestF domain getService path = do
  domainV <- objDomain domain
  serviceMap <- getServiceMap domainV

  liftIO . HTTP.parseRequest $
    let HostPort h p = getService serviceMap
     in "http://" <> h <> ":" <> show p <> ("/" <> joinHttpPath (splitHttpPath path))

-- The metrics setup for both internal and external federator servers
-- are the same, so we can simply run the same test for both.
testFederatorMetricsInternal :: App ()
testFederatorMetricsInternal = runFederatorMetrics federatorInternal

testFederatorMetricsExternal :: App ()
testFederatorMetricsExternal = runFederatorMetrics federatorExternal
