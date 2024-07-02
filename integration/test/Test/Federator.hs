{-# LANGUAGE OverloadedStrings #-}

module Test.Federator where

import API.Brig
import API.Federator (getMetrics)
import Data.Attoparsec.Text
import qualified Data.ByteString as BS
import Data.String.Conversions
import Data.Text
import SetupHelpers (randomUser)
import Testlib.Prelude

runFederatorMetrics :: (ServiceMap -> HostPort) -> App ()
runFederatorMetrics getService = do
  let handleRes res = res <$ res.status `shouldMatchInt` 200
  first <- bindResponse (getMetrics OwnDomain getService) handleRes
  second <- bindResponse (getMetrics OwnDomain getService) handleRes
  assertBool "Two metric requests should never match" $ first.body /= second.body
  assertBool "Second metric response should never be 0 length (the first might be)" $ BS.length second.body /= 0
  assertBool "The seconds metric response should have text indicating that it is returning metrics"
    $ BS.isInfixOf expectedString second.body
  where
    expectedString = "# TYPE http_request_duration_seconds histogram"

-- | The metrics setup for both internal and external federator servers
-- are the same, so we can simply run the same test for both.
testFederatorMetricsInternal :: App ()
testFederatorMetricsInternal = runFederatorMetrics federatorInternal

testFederatorMetricsExternal :: App ()
testFederatorMetricsExternal = runFederatorMetrics federatorExternal

testFederatorNumRequestsMetrics :: (HasCallStack) => App ()
testFederatorNumRequestsMetrics = do
  u1 <- randomUser OwnDomain def
  u2 <- randomUser OtherDomain def
  incomingBefore <- getMetric parseIncomingRequestCount OtherDomain OwnDomain
  outgoingBefore <- getMetric parseOutgoingRequestCount OwnDomain OtherDomain
  bindResponse (searchContacts u1 (u2 %. "name") OtherDomain) $ \resp ->
    resp.status `shouldMatchInt` 200
  incomingAfter <- getMetric parseIncomingRequestCount OtherDomain OwnDomain
  outgoingAfter <- getMetric parseOutgoingRequestCount OwnDomain OtherDomain
  assertBool "Incoming requests count should have increased by at least 2" $ incomingAfter >= incomingBefore + 2
  assertBool "Outgoing requests count should have increased by at least 2" $ outgoingAfter >= outgoingBefore + 2
  where
    getMetric :: (Text -> Parser Integer) -> Domain -> Domain -> App Integer
    getMetric p domain origin = do
      m <- getMetrics domain federatorInternal
      d <- cs <$> asString origin
      pure $ fromRight 0 (parseOnly (p d) (cs m.body))

    parseIncomingRequestCount :: Text -> Parser Integer
    parseIncomingRequestCount d =
      manyTill anyChar (string ("com_wire_federator_incoming_requests{origin_domain=\"" <> d <> "\"} "))
        *> decimal

    parseOutgoingRequestCount :: Text -> Parser Integer
    parseOutgoingRequestCount d =
      manyTill anyChar (string ("com_wire_federator_outgoing_requests{target_domain=\"" <> d <> "\"} "))
        *> decimal
