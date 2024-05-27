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
import qualified Data.Aeson.Key as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Set as Set
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

testValidateSAMLEmails :: HasCallStack => App ()
testValidateSAMLEmails = _testSimpleFlag "validateSAMLemails" True

testConferenceCalling :: HasCallStack => App ()
testConferenceCalling = _testSimpleFlag "conferenceCalling" True

testSearchVisibilityInbound :: HasCallStack => App ()
testSearchVisibilityInbound = _testSimpleFlag "searchVisibilityInbound" False

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

testConversationGuestLinks :: HasCallStack => App ()
testConversationGuestLinks = _testSimpleFlagWithLockStatus "conversationGuestLinks" True True

testFileSharing :: HasCallStack => App ()
testFileSharing = _testSimpleFlagWithLockStatus "fileSharing" True True

testSndFactorPasswordChallenge :: HasCallStack => App ()
testSndFactorPasswordChallenge = _testSimpleFlagWithLockStatus "sndFactorPasswordChallenge" False False

testOutlookCalIntegration :: HasCallStack => App ()
testOutlookCalIntegration = _testSimpleFlagWithLockStatus "outlookCalIntegration" False False

_testSimpleFlagWithLockStatus :: HasCallStack => String -> Bool -> Bool -> App ()
_testSimpleFlagWithLockStatus featureName featureEnabledByDefault featureUnlockedByDefault = do
  -- let defaultStatus = if featureEnabledByDefault then "enabled" else "disabled"
  defaultValue <- (if featureEnabledByDefault then enabled else disabled) & setField "lockStatus" (if featureUnlockedByDefault then "unlocked" else "locked")
  let otherStatus = if featureEnabledByDefault then "disabled" else "enabled"

  (owner, tid, m : _) <- createTeam OwnDomain 2
  nonTeamMember <- randomUser OwnDomain def
  assertForbidden =<< Public.getTeamFeature nonTeamMember tid featureName

  checkFeature featureName m tid defaultValue

  -- unlock feature if it is locked
  unless featureUnlockedByDefault $ Internal.setTeamFeatureLockStatus OwnDomain tid featureName "unlocked"

  -- change the status
  let otherValue = if featureEnabledByDefault then disabled else enabled
  void $ withWebSockets [m] $ \wss -> do
    assertSuccess =<< Public.setTeamFeatureConfig owner tid featureName (object ["status" .= otherStatus])
    for_ wss $ \ws -> do
      notif <- awaitMatch isFeatureConfigUpdateNotif ws
      notif %. "payload.0.name" `shouldMatch` featureName
      notif %. "payload.0.data" `shouldMatch` otherValue

  checkFeature featureName m tid otherValue

  -- lock feature
  Internal.setTeamFeatureLockStatus OwnDomain tid featureName "locked"

  -- feature status should be the default again
  checkFeature featureName m tid =<< setField "lockStatus" "locked" defaultValue
  assertStatus 409 =<< Public.setTeamFeatureConfig owner tid featureName (object ["status" .= otherStatus])

  -- unlock again
  Internal.setTeamFeatureLockStatus OwnDomain tid featureName "unlocked"

  -- feature status should be the previously set status again
  checkFeature featureName m tid =<< setField "lockStatus" "unlocked" otherValue

testClassifiedDomainsEnabled :: HasCallStack => App ()
testClassifiedDomainsEnabled = do
  (_, tid, m : _) <- createTeam OwnDomain 2
  expected <- enabled & setField "config.domains" ["example.com"]
  checkFeature "classifiedDomains" m tid expected

testClassifiedDomainsDisabled :: HasCallStack => App ()
testClassifiedDomainsDisabled = do
  withModifiedBackend def {galleyCfg = setField "settings.featureFlags.classifiedDomains.status" "disabled"} $ \domain -> do
    (_, tid, m : _) <- createTeam domain 2
    expected <- disabled & setField "config.domains" ["d1.example.com"]
    checkFeature "classifiedDomains" m tid expected

-- | Call 'GET /teams/:tid/features' and 'GET /feature-configs', and check if all
-- features are there.
testAllFeatures :: HasCallStack => App ()
testAllFeatures = do
  (_, tid, m : _) <- createTeam OwnDomain 2
  let expected =
        object $
          [ "legalhold" .= disabled,
            "sso" .= disabled,
            "searchVisibility" .= disabled,
            "validateSAMLemails" .= enabled,
            "digitalSignatures" .= disabled,
            "appLock" .= object ["lockStatus" .= "unlocked", "status" .= "enabled", "ttl" .= "unlimited", "config" .= object ["enforceAppLock" .= False, "inactivityTimeoutSecs" .= A.Number 60]],
            "fileSharing" .= enabled,
            "classifiedDomains" .= object ["lockStatus" .= "unlocked", "status" .= "enabled", "ttl" .= "unlimited", "config" .= object ["domains" .= ["example.com"]]],
            "conferenceCalling" .= enabled,
            "selfDeletingMessages" .= object ["lockStatus" .= "unlocked", "status" .= "enabled", "ttl" .= "unlimited", "config" .= object ["enforcedTimeoutSeconds" .= A.Number 0]],
            "conversationGuestLinks" .= enabled,
            "sndFactorPasswordChallenge" .= disabledLocked,
            "mls"
              .= object
                [ "lockStatus" .= "unlocked",
                  "status" .= "disabled",
                  "ttl" .= "unlimited",
                  "config"
                    .= object
                      [ "protocolToggleUsers" .= ([] :: [String]),
                        "defaultProtocol" .= "proteus",
                        "supportedProtocols" .= ["proteus", "mls"],
                        "allowedCipherSuites" .= ([1] :: [Int]),
                        "defaultCipherSuite" .= A.Number 1
                      ]
                ],
            "searchVisibilityInbound" .= disabled,
            "exposeInvitationURLsToTeamAdmin" .= disabledLocked,
            "outlookCalIntegration" .= disabledLocked,
            "mlsE2EId"
              .= object
                [ "lockStatus" .= "unlocked",
                  "status" .= "disabled",
                  "ttl" .= "unlimited",
                  "config"
                    .= object
                      [ "verificationExpiration" .= A.Number 86400,
                        "useProxyOnMobile" .= False
                      ]
                ],
            "mlsMigration"
              .= object
                [ "lockStatus" .= "locked",
                  "status" .= "enabled",
                  "ttl" .= "unlimited",
                  "config"
                    .= object
                      [ "startTime" .= "2029-05-16T10:11:12.123Z",
                        "finaliseRegardlessAfter" .= "2029-10-17T00:00:00Z"
                      ]
                ],
            "enforceFileDownloadLocation" .= object ["lockStatus" .= "locked", "status" .= "disabled", "ttl" .= "unlimited", "config" .= object []],
            "limitedEventFanout" .= disabled
          ]
  bindResponse (Public.getTeamFeatures m tid) $ \resp -> do
    resp.status `shouldMatchInt` 200
    expected `shouldMatch` resp.json

  -- This block catches potential errors in the logic that reverts to default if there is a distinction made between
  -- 1. there is no row for a team_id in galley.team_features
  -- 2. there is a row for team_id in galley.team_features but the feature has a no entry (null value)
  Internal.setTeamFeatureConfig OwnDomain tid "conversationGuestLinks" enabled >>= assertSuccess

  bindResponse (Public.getTeamFeatures m tid) $ \resp -> do
    resp.status `shouldMatchInt` 200
    expected `shouldMatch` resp.json

  bindResponse (Public.getFeatureConfigs m) $ \resp -> do
    resp.status `shouldMatchInt` 200
    expected `shouldMatch` resp.json

  randomPersonalUser <- randomUser OwnDomain def

  bindResponse (Public.getFeatureConfigs randomPersonalUser) $ \resp -> do
    resp.status `shouldMatchInt` 200
    expected `shouldMatch` resp.json

testFeatureConfigConsistency :: HasCallStack => App ()
testFeatureConfigConsistency = do
  (_, tid, m : _) <- createTeam OwnDomain 2

  allFeaturesRes <- Public.getFeatureConfigs m >>= parseObjectKeys

  allTeamFeaturesRes <- Public.getTeamFeatures m tid >>= parseObjectKeys

  unless (allTeamFeaturesRes `Set.isSubsetOf` allFeaturesRes) $
    assertFailure (show allTeamFeaturesRes <> " is not a subset of " <> show allFeaturesRes)
  where
    parseObjectKeys :: Response -> App (Set.Set String)
    parseObjectKeys res = do
      val <- res.json
      case val of
        (A.Object hm) -> pure (Set.fromList . map (show . A.toText) . KM.keys $ hm)
        x -> assertFailure ("JSON was not an object, but " <> show x)

testSelfDeletingMessages :: HasCallStack => App ()
testSelfDeletingMessages =
  _testLockStatusWithConfig
    "selfDeletingMessages"
    (object ["lockStatus" .= "unlocked", "status" .= "enabled", "ttl" .= "unlimited", "config" .= object ["enforcedTimeoutSeconds" .= A.Number 0]])
    (object ["status" .= "enabled", "config" .= object ["enforcedTimeoutSeconds" .= A.Number 30]])

_testLockStatusWithConfig :: HasCallStack => String -> Value -> Value -> App ()
_testLockStatusWithConfig featureName defaultSetting configUpdate = do
  -- personal user
  randomPersonalUser <- randomUser OwnDomain def

  bindResponse (Public.getFeatureConfigs randomPersonalUser) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. featureName `shouldMatch` defaultSetting

  -- team user
  defaultConfig <- defaultSetting %. "config"

  featureUnlockedByDefault <- do
    ls <- defaultSetting %. "lockStatus" >>= asString
    pure $ ls == "unlocked"

  featureEnabledByDefault <- do
    s <- defaultSetting %. "status" >>= asString
    pure $ s == "enabled"

  (owner, tid, m : _) <- createTeam OwnDomain 2
  nonTeamMember <- randomUser OwnDomain def
  assertForbidden =<< Public.getTeamFeature nonTeamMember tid featureName

  checkFeature featureName m tid defaultSetting

  -- unlock feature if it is locked
  unless featureUnlockedByDefault $ Internal.setTeamFeatureLockStatus OwnDomain tid featureName "unlocked"

  -- change the status
  let otherStatus = if featureEnabledByDefault then "disabled" else "enabled"
  defaultSettingWithOtherStatus <- defaultSetting & setField "status" otherStatus

  void $ withWebSockets [m] $ \wss -> do
    assertSuccess =<< Public.setTeamFeatureConfig owner tid featureName (object ["status" .= otherStatus, "config" .= defaultConfig])
    for_ wss $ \ws -> do
      notif <- awaitMatch isFeatureConfigUpdateNotif ws
      notif %. "payload.0.name" `shouldMatch` featureName
      notif %. "payload.0.data" `shouldMatch` defaultSettingWithOtherStatus

  checkFeature featureName m tid defaultSettingWithOtherStatus

  -- lock feature
  Internal.setTeamFeatureLockStatus OwnDomain tid featureName "locked"

  -- feature status should be the default again
  checkFeature featureName m tid =<< setField "lockStatus" "locked" defaultSetting
  assertStatus 409 =<< Public.setTeamFeatureConfig owner tid featureName (object ["status" .= otherStatus, "config" .= defaultConfig])

  -- unlock again
  Internal.setTeamFeatureLockStatus OwnDomain tid featureName "unlocked"

  -- feature status should be the previously set status again
  checkFeature featureName m tid defaultSettingWithOtherStatus

  bindResponse (Public.setTeamFeatureConfig owner tid featureName configUpdate) $ \resp -> do
    resp.status `shouldMatchInt` 200
  checkFeature featureName m tid =<< (configUpdate & setField "lockStatus" "unlocked" & setField "ttl" "unlimited")

  -- after locking once again it should be the default again
  Internal.setTeamFeatureLockStatus OwnDomain tid featureName "locked"
  checkFeature featureName m tid =<< setField "lockStatus" "locked" defaultSetting
