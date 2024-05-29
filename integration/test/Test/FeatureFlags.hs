-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2023 Wire Swiss GmbH <opensource@wire.com>
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

module Test.FeatureFlags where

import qualified API.Galley as Public
import API.GalleyInternal
import qualified API.GalleyInternal as Internal
import Control.Monad.Reader
import qualified Data.Aeson as A
import SetupHelpers
import Testlib.Prelude

testLimitedEventFanout :: HasCallStack => App ()
testLimitedEventFanout = do
  let featureName = "limitedEventFanout"
  (_alice, team, _) <- createTeam OwnDomain 1
  bindResponse (getTeamFeature OwnDomain featureName team) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "status" `shouldMatch` "disabled"
  assertSuccess =<< setTeamFeatureStatus OwnDomain team featureName "enabled"
  bindResponse (getTeamFeature OwnDomain featureName team) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "status" `shouldMatch` "enabled"

disabled :: Value
disabled = object ["lockStatus" .= "unlocked", "status" .= "disabled", "ttl" .= "unlimited"]

disabledLocked :: Value
disabledLocked = object ["lockStatus" .= "locked", "status" .= "disabled", "ttl" .= "unlimited"]

enabled :: Value
enabled = object ["lockStatus" .= "unlocked", "status" .= "enabled", "ttl" .= "unlimited"]

checkFeature :: (HasCallStack, MakesValue user, MakesValue tid) => String -> user -> tid -> Value -> App ()
checkFeature feature user tid expected = do
  tidStr <- asString tid
  domain <- objDomain user
  bindResponse (Internal.getTeamFeature domain tidStr feature) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json `shouldMatch` expected
  bindResponse (Public.getTeamFeatures user tid) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. feature `shouldMatch` expected
  bindResponse (Public.getTeamFeature user tid feature) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json `shouldMatch` expected
  bindResponse (Public.getFeatureConfigs user) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. feature `shouldMatch` expected

testMlsE2EConfigCrlProxyRequired :: HasCallStack => App ()
testMlsE2EConfigCrlProxyRequired = do
  (owner, tid, _) <- createTeam OwnDomain 1
  let configWithoutCrlProxy =
        object
          [ "config"
              .= object
                [ "useProxyOnMobile" .= False,
                  "verificationExpiration" .= A.Number 86400
                ],
            "status" .= "enabled"
          ]

  -- From API version 6 onwards, the CRL proxy is required, so the request should fail when it's not provided
  bindResponse (Internal.setTeamFeatureConfig Versioned owner tid "mlsE2EId" configWithoutCrlProxy) $ \resp -> do
    resp.status `shouldMatchInt` 400
    resp.json %. "label" `shouldMatch` "mls-e2eid-missing-crl-proxy"

  configWithCrlProxy <-
    configWithoutCrlProxy
      & setField "config.useProxyOnMobile" True
      & setField "config.crlProxy" "https://crl-proxy.example.com"
      & setField "status" "enabled"

  -- The request should succeed when the CRL proxy is provided
  bindResponse (Internal.setTeamFeatureConfig Versioned owner tid "mlsE2EId" configWithCrlProxy) $ \resp -> do
    resp.status `shouldMatchInt` 200

  -- Assert that the feature config got updated correctly
  expectedResponse <- configWithCrlProxy & setField "lockStatus" "unlocked" & setField "ttl" "unlimited"
  checkFeature "mlsE2EId" owner tid expectedResponse

testMlsE2EConfigCrlProxyNotRequiredInV5 :: HasCallStack => App ()
testMlsE2EConfigCrlProxyNotRequiredInV5 = do
  (owner, tid, _) <- createTeam OwnDomain 1
  let configWithoutCrlProxy =
        object
          [ "config"
              .= object
                [ "useProxyOnMobile" .= False,
                  "verificationExpiration" .= A.Number 86400
                ],
            "status" .= "enabled"
          ]

  -- In API version 5, the CRL proxy is not required, so the request should succeed
  bindResponse (Internal.setTeamFeatureConfig (ExplicitVersion 5) owner tid "mlsE2EId" configWithoutCrlProxy) $ \resp -> do
    resp.status `shouldMatchInt` 200

  -- Assert that the feature config got updated correctly
  expectedResponse <- configWithoutCrlProxy & setField "lockStatus" "unlocked" & setField "ttl" "unlimited"
  checkFeature "mlsE2EId" owner tid expectedResponse
