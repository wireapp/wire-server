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
import qualified API.GalleyInternal as Internal
import Control.Monad.Codensity (Codensity (runCodensity))
import Control.Monad.Reader
import SetupHelpers
import Testlib.Prelude
import Testlib.ResourcePool (acquireResources)

testLimitedEventFanout :: HasCallStack => App ()
testLimitedEventFanout = do
  let featureName = "limitedEventFanout"
  (_alice, team, _) <- createTeam OwnDomain 1
  -- getTeamFeatureStatus OwnDomain team "limitedEventFanout" "enabled"
  bindResponse (Internal.getTeamFeature OwnDomain team featureName) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "status" `shouldMatch` "disabled"
  Internal.setTeamFeatureStatus OwnDomain team featureName "enabled"
  bindResponse (Internal.getTeamFeature OwnDomain team featureName) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "status" `shouldMatch` "enabled"

disabled :: Value
disabled = object ["lockStatus" .= "unlocked", "status" .= "disabled", "ttl" .= "unlimited"]

enabled :: Value
enabled = object ["lockStatus" .= "unlocked", "status" .= "enabled", "ttl" .= "unlimited"]

checkLegalholdStatus :: (HasCallStack, MakesValue user, MakesValue tid) => String -> user -> tid -> Value -> App ()
checkLegalholdStatus domain user tid expected = do
  tidStr <- asString tid
  bindResponse (Internal.getTeamFeature domain tidStr "legalhold") $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json `shouldMatch` expected
  bindResponse (Public.getFeatureConfigs user) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "legalhold" `shouldMatch` expected
  bindResponse (Public.getTeamFeatures user tid) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "legalhold" `shouldMatch` expected
  bindResponse (Public.getTeamFeature user tid "legalhold") $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json `shouldMatch` expected

-- always disabled
testLegalholdDisabledPermanently :: HasCallStack => App ()
testLegalholdDisabledPermanently = do
  let cfgLhDisabledPermanently =
        def
          { galleyCfg = setField "settings.featureFlags.legalhold" "disabled-permanently"
          }
      cfgLhDisabledByDefault =
        def
          { galleyCfg = setField "settings.featureFlags.legalhold" "disabled-by-default"
          }
  resourcePool <- asks (.resourcePool)
  runCodensity (acquireResources 1 resourcePool) $ \[testBackend] -> do
    let domain = testBackend.berDomain

    -- Happy case: DB has no config for the team
    runCodensity (startDynamicBackend testBackend cfgLhDisabledPermanently) $ \_ -> do
      (owner, tid, _) <- createTeam domain 1
      checkLegalholdStatus domain owner tid disabled
      Internal.setTeamFeatureStatusExpectHttpStatus domain tid "legalhold" "enabled" 403

    -- Inteteresting case: The team had LH enabled before backend config was
    -- changed to disabled-permanently
    (owner, tid) <- runCodensity (startDynamicBackend testBackend cfgLhDisabledByDefault) $ \_ -> do
      (owner, tid, _) <- createTeam domain 1
      checkLegalholdStatus domain owner tid disabled
      Internal.setTeamFeatureStatusExpectHttpStatus domain tid "legalhold" "enabled" 200
      checkLegalholdStatus domain owner tid enabled
      pure (owner, tid)

    runCodensity (startDynamicBackend testBackend cfgLhDisabledPermanently) $ \_ -> do
      checkLegalholdStatus domain owner tid disabled

-- can be enabled for a team, disabled if unset
testLegalholdDisabledByDefault :: HasCallStack => App ()
testLegalholdDisabledByDefault = do
  withModifiedBackend
    (def {galleyCfg = setField "settings.featureFlags.legalhold" "disabled-by-default"})
    $ \domain -> do
      (owner, tid, _) <- createTeam domain 1
      checkLegalholdStatus domain owner tid disabled
      Internal.setTeamFeatureStatus domain tid "legalhold" "enabled"
      checkLegalholdStatus domain owner tid enabled
      Internal.setTeamFeatureStatus domain tid "legalhold" "disabled"
      checkLegalholdStatus domain owner tid disabled

-- enabled if team is allow listed, disabled in any other case
testLegalholdWhitelistTeamsAndImplicitConsent :: HasCallStack => App ()
testLegalholdWhitelistTeamsAndImplicitConsent = do
  withModifiedBackend
    (def {galleyCfg = setField "settings.featureFlags.legalhold" "whitelist-teams-and-implicit-consent"})
    $ \domain -> do
      (owner, tid, _) <- createTeam domain 1
      checkLegalholdStatus domain owner tid disabled
      Internal.legalholdWhitelistTeam tid owner >>= assertSuccess
      checkLegalholdStatus domain owner tid enabled

      -- Disabling it doesn't work
      Internal.setTeamFeatureStatusExpectHttpStatus domain tid "legalhold" "disabled" 403
      checkLegalholdStatus domain owner tid enabled
