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
import qualified Data.Aeson as A
import Notifications
import SetupHelpers
import Test.FeatureFlags.Util
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
  assertSuccess =<< Internal.setTeamFeatureStatus OwnDomain team featureName "enabled"
  bindResponse (Internal.getTeamFeature OwnDomain team featureName) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "status" `shouldMatch` "enabled"

testLegalholdDisabledByDefault :: HasCallStack => App ()
testLegalholdDisabledByDefault = do
  let put uid tid st = Internal.setTeamFeatureConfig uid tid "legalhold" (object ["status" .= st]) >>= assertSuccess
  let patch uid tid st = Internal.setTeamFeatureStatus uid tid "legalhold" st >>= assertSuccess
  forM_ [put, patch] $ \setFeatureStatus -> do
    withModifiedBackend
      def {galleyCfg = setField "settings.featureFlags.legalhold" "disabled-by-default"}
      $ \domain -> do
        (owner, tid, m : _) <- createTeam domain 2
        nonMember <- randomUser domain def
        assertForbidden =<< Public.getTeamFeature nonMember tid "legalhold"
        -- Test default
        checkFeature "legalhold" m tid disabled
        -- Test override
        setFeatureStatus owner tid "enabled"
        checkFeature "legalhold" owner tid enabled
        setFeatureStatus owner tid "disabled"
        checkFeature "legalhold" owner tid disabled

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
      checkFeature "legalhold" owner tid disabled
      assertStatus 403 =<< Internal.setTeamFeatureStatus domain tid "legalhold" "enabled"
      assertStatus 403 =<< Internal.setTeamFeatureConfig domain tid "legalhold" (object ["status" .= "enabled"])

    -- Interesting case: The team had LH enabled before backend config was
    -- changed to disabled-permanently
    (owner, tid) <- runCodensity (startDynamicBackend testBackend cfgLhDisabledByDefault) $ \_ -> do
      (owner, tid, _) <- createTeam domain 1
      checkFeature "legalhold" owner tid disabled
      assertSuccess =<< Internal.setTeamFeatureStatus domain tid "legalhold" "enabled"
      checkFeature "legalhold" owner tid enabled
      pure (owner, tid)

    runCodensity (startDynamicBackend testBackend cfgLhDisabledPermanently) $ \_ -> do
      checkFeature "legalhold" owner tid disabled

-- enabled if team is allow listed, disabled in any other case
testLegalholdWhitelistTeamsAndImplicitConsent :: HasCallStack => App ()
testLegalholdWhitelistTeamsAndImplicitConsent = do
  let cfgLhWhitelistTeamsAndImplicitConsent =
        def
          { galleyCfg = setField "settings.featureFlags.legalhold" "whitelist-teams-and-implicit-consent"
          }
      cfgLhDisabledByDefault =
        def
          { galleyCfg = setField "settings.featureFlags.legalhold" "disabled-by-default"
          }
  resourcePool <- asks (.resourcePool)
  runCodensity (acquireResources 1 resourcePool) $ \[testBackend] -> do
    let domain = testBackend.berDomain

    -- Happy case: DB has no config for the team
    (owner, tid) <- runCodensity (startDynamicBackend testBackend cfgLhWhitelistTeamsAndImplicitConsent) $ \_ -> do
      (owner, tid, _) <- createTeam domain 1
      checkFeature "legalhold" owner tid disabled
      Internal.legalholdWhitelistTeam tid owner >>= assertSuccess
      checkFeature "legalhold" owner tid enabled

      -- Disabling it doesn't work
      assertStatus 403 =<< Internal.setTeamFeatureStatus domain tid "legalhold" "disabled"
      assertStatus 403 =<< Internal.setTeamFeatureConfig domain tid "legalhold" (object ["status" .= "disabled"])
      checkFeature "legalhold" owner tid enabled
      pure (owner, tid)

    -- Interesting case: The team had LH disabled before backend config was
    -- changed to "whitelist-teams-and-implicit-consent". It should still show
    -- enabled when the config gets changed.
    runCodensity (startDynamicBackend testBackend cfgLhDisabledByDefault) $ \_ -> do
      checkFeature "legalhold" owner tid disabled
      assertSuccess =<< Internal.setTeamFeatureStatus domain tid "legalhold" "disabled"
      checkFeature "legalhold" owner tid disabled

    runCodensity (startDynamicBackend testBackend cfgLhWhitelistTeamsAndImplicitConsent) $ \_ -> do
      checkFeature "legalhold" owner tid enabled

testExposeInvitationURLsToTeamAdminConfig :: HasCallStack => App ()
testExposeInvitationURLsToTeamAdminConfig = do
  let cfgExposeInvitationURLsTeamAllowlist tids =
        def
          { galleyCfg = setField "settings.exposeInvitationURLsTeamAllowlist" tids
          }
  resourcePool <- asks (.resourcePool)
  runCodensity (acquireResources 1 resourcePool) $ \[testBackend] -> do
    let domain = testBackend.berDomain

    let testNoAllowlistEntry = runCodensity (startDynamicBackend testBackend $ cfgExposeInvitationURLsTeamAllowlist ([] :: [String])) $ \_ -> do
          (owner, tid, _) <- createTeam domain 1
          checkFeature "exposeInvitationURLsToTeamAdmin" owner tid disabledLocked
          -- here we get a response with HTTP status 200 and feature status unchanged (disabled), which we find weird, but we're just testing the current behavior
          assertSuccess =<< Internal.setTeamFeatureStatus domain tid "exposeInvitationURLsToTeamAdmin" "enabled"
          assertSuccess =<< Internal.setTeamFeatureStatus domain tid "exposeInvitationURLsToTeamAdmin" "disabled"
          pure (owner, tid)

    -- Happy case: DB has no config for the team
    (owner, tid) <- testNoAllowlistEntry

    -- Interesting case: The team is in the allow list
    runCodensity (startDynamicBackend testBackend $ cfgExposeInvitationURLsTeamAllowlist [tid]) $ \_ -> do
      checkFeature "exposeInvitationURLsToTeamAdmin" owner tid disabled
      assertSuccess =<< Internal.setTeamFeatureStatus domain tid "exposeInvitationURLsToTeamAdmin" "enabled"
      checkFeature "exposeInvitationURLsToTeamAdmin" owner tid enabled
      assertSuccess =<< Internal.setTeamFeatureStatus domain tid "exposeInvitationURLsToTeamAdmin" "disabled"
      checkFeature "exposeInvitationURLsToTeamAdmin" owner tid disabled
      assertSuccess =<< Internal.setTeamFeatureStatus domain tid "exposeInvitationURLsToTeamAdmin" "enabled"
      checkFeature "exposeInvitationURLsToTeamAdmin" owner tid enabled

    -- Interesting case: The team had the feature enabled but is not in allow list
    void testNoAllowlistEntry

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
  bindResponse (Public.setTeamFeatureConfig owner tid "mlsE2EId" configWithoutCrlProxy) $ \resp -> do
    resp.status `shouldMatchInt` 400
    resp.json %. "label" `shouldMatch` "mls-e2eid-missing-crl-proxy"

  configWithCrlProxy <-
    configWithoutCrlProxy
      & setField "config.useProxyOnMobile" True
      & setField "config.crlProxy" "https://crl-proxy.example.com"
      & setField "status" "enabled"

  -- The request should succeed when the CRL proxy is provided
  bindResponse (Public.setTeamFeatureConfig owner tid "mlsE2EId" configWithCrlProxy) $ \resp -> do
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
  bindResponse (Public.setTeamFeatureConfigVersioned (ExplicitVersion 5) owner tid "mlsE2EId" configWithoutCrlProxy) $ \resp -> do
    resp.status `shouldMatchInt` 200

  -- Assert that the feature config got updated correctly
  expectedResponse <- configWithoutCrlProxy & setField "lockStatus" "unlocked" & setField "ttl" "unlimited"
  checkFeature "mlsE2EId" owner tid expectedResponse

testSSODisabledByDefault :: HasCallStack => App ()
testSSODisabledByDefault = do
  let put uid tid = Internal.setTeamFeatureConfig uid tid "sso" (object ["status" .= "enabled"]) >>= assertSuccess
  let patch uid tid = Internal.setTeamFeatureStatus uid tid "sso" "enabled" >>= assertSuccess
  forM_ [put, patch] $ \enableFeature -> do
    withModifiedBackend
      def {galleyCfg = setField "settings.featureFlags.sso" "disabled-by-default"}
      $ \domain -> do
        (owner, tid, m : _) <- createTeam domain 2
        nonMember <- randomUser domain def
        assertForbidden =<< Public.getTeamFeature nonMember tid "sso"
        -- Test default
        checkFeature "sso" m tid disabled
        -- Test override
        enableFeature owner tid
        checkFeature "sso" owner tid enabled

testSSOEnabledByDefault :: HasCallStack => App ()
testSSOEnabledByDefault = do
  withModifiedBackend
    def {galleyCfg = setField "settings.featureFlags.sso" "enabled-by-default"}
    $ \domain -> do
      (owner, tid, _m : _) <- createTeam domain 2
      nonMember <- randomUser domain def
      assertForbidden =<< Public.getTeamFeature nonMember tid "sso"
      checkFeature "sso" owner tid enabled
      -- check that the feature cannot be disabled
      assertLabel 403 "not-implemented" =<< Internal.setTeamFeatureConfig owner tid "sso" (object ["status" .= "disabled"])

testSearchVisibilityDisabledByDefault :: HasCallStack => App ()
testSearchVisibilityDisabledByDefault = do
  withModifiedBackend def {galleyCfg = setField "settings.featureFlags.teamSearchVisibility" "disabled-by-default"} $ \domain -> do
    (owner, tid, m : _) <- createTeam domain 2
    nonMember <- randomUser domain def
    assertForbidden =<< Public.getTeamFeature nonMember tid "searchVisibility"
    -- Test default
    checkFeature "searchVisibility" m tid disabled
    assertSuccess =<< Internal.setTeamFeatureStatus owner tid "searchVisibility" "enabled"
    checkFeature "searchVisibility" owner tid enabled
    assertSuccess =<< Internal.setTeamFeatureStatus owner tid "searchVisibility" "disabled"
    checkFeature "searchVisibility" owner tid disabled

testSearchVisibilityEnabledByDefault :: HasCallStack => App ()
testSearchVisibilityEnabledByDefault = do
  withModifiedBackend def {galleyCfg = setField "settings.featureFlags.teamSearchVisibility" "enabled-by-default"} $ \domain -> do
    (owner, tid, m : _) <- createTeam domain 2
    nonMember <- randomUser domain def
    assertForbidden =<< Public.getTeamFeature nonMember tid "searchVisibility"
    -- Test default
    checkFeature "searchVisibility" m tid enabled
    assertSuccess =<< Internal.setTeamFeatureStatus owner tid "searchVisibility" "disabled"
    checkFeature "searchVisibility" owner tid disabled
    assertSuccess =<< Internal.setTeamFeatureStatus owner tid "searchVisibility" "enabled"
    checkFeature "searchVisibility" owner tid enabled

testDigitalSignatures :: HasCallStack => App ()
testDigitalSignatures = _testSimpleFlag "digitalSignatures" False

_testSimpleFlag :: HasCallStack => String -> Bool -> App ()
_testSimpleFlag featureName featureEnabledByDefault = do
  let defaultStatus = if featureEnabledByDefault then "enabled" else "disabled"
  let defaultValue = if featureEnabledByDefault then enabled else disabled
  let otherStatus = if featureEnabledByDefault then "disabled" else "enabled"
  let otherValue = if featureEnabledByDefault then disabled else enabled

  (_, tid, m : _) <- createTeam OwnDomain 2
  nonTeamMember <- randomUser OwnDomain def
  assertForbidden =<< Public.getTeamFeature nonTeamMember tid featureName
  checkFeature featureName m tid defaultValue
  -- should receive an event
  void $ withWebSockets [m] $ \wss -> do
    assertSuccess =<< Internal.setTeamFeatureStatus OwnDomain tid featureName otherStatus
    for_ wss $ \ws -> do
      notif <- awaitMatch isFeatureConfigUpdateNotif ws
      notif %. "payload.0.name" `shouldMatch` featureName
      notif %. "payload.0.data" `shouldMatch` otherValue

    checkFeature featureName m tid otherValue
    assertSuccess =<< Internal.setTeamFeatureStatus OwnDomain tid featureName defaultStatus
    for_ wss $ \ws -> do
      notif <- awaitMatch isFeatureConfigUpdateNotif ws
      notif %. "payload.0.name" `shouldMatch` featureName
      notif %. "payload.0.data" `shouldMatch` defaultValue
    checkFeature featureName m tid defaultValue
