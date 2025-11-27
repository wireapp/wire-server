{-# LANGUAGE OverloadedStrings #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

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
runFederatorMetrics getServiceAddress = do
  let handleRes res = res <$ res.status `shouldMatchInt` 200
  first <- bindResponse (getMetrics OwnDomain getServiceAddress) handleRes
  second <- bindResponse (getMetrics OwnDomain getServiceAddress) handleRes
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
