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
import Control.Concurrent (threadDelay)
import Control.Monad.Reader
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Set as Set
import Data.String.Conversions (cs)
import Notifications
import SetupHelpers
import Test.FeatureFlags.Util
import Testlib.Prelude

testLimitedEventFanout :: HasCallStack => App ()
testLimitedEventFanout = do
  let featureName = "limitedEventFanout"
  (_alice, team, _) <- createTeam OwnDomain 1
  bindResponse (Internal.getTeamFeature OwnDomain featureName team) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "status" `shouldMatch` "disabled"
  assertSuccess =<< Internal.setTeamFeatureStatus OwnDomain team featureName "enabled"
  bindResponse (Internal.getTeamFeature OwnDomain featureName team) $ \resp -> do
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

testSearchVisibilityInbound :: HasCallStack => App ()
testSearchVisibilityInbound = _testSimpleFlag "searchVisibilityInbound" Public.setTeamFeatureConfig False

testDigitalSignaturesInternal :: HasCallStack => App ()
testDigitalSignaturesInternal = _testSimpleFlag "digitalSignatures" Internal.setTeamFeatureConfig False

testValidateSAMLEmailsInternal :: HasCallStack => App ()
testValidateSAMLEmailsInternal = _testSimpleFlag "validateSAMLemails" Internal.setTeamFeatureConfig True

testConferenceCallingInternal :: HasCallStack => App ()
testConferenceCallingInternal = _testSimpleFlag "conferenceCalling" Internal.setTeamFeatureConfig True

testSearchVisibilityInboundInternal :: HasCallStack => App ()
testSearchVisibilityInboundInternal = _testSimpleFlag "searchVisibilityInbound" Internal.setTeamFeatureConfig False

_testSimpleFlag :: HasCallStack => String -> (Value -> String -> String -> Value -> App Response) -> Bool -> App ()
_testSimpleFlag featureName setFeatureConfig featureEnabledByDefault = do
  let defaultStatus = if featureEnabledByDefault then "enabled" else "disabled"
  let defaultValue = if featureEnabledByDefault then enabled else disabled
  let otherStatus = if featureEnabledByDefault then "disabled" else "enabled"
  let otherValue = if featureEnabledByDefault then disabled else enabled

  (owner, tid, m : _) <- createTeam OwnDomain 2
  nonTeamMember <- randomUser OwnDomain def
  assertForbidden =<< Public.getTeamFeature nonTeamMember tid featureName
  checkFeature featureName m tid defaultValue
  -- should receive an event
  void $ withWebSockets [m] $ \wss -> do
    assertSuccess =<< setFeatureConfig owner tid featureName (object ["status" .= otherStatus])
    for_ wss $ \ws -> do
      notif <- awaitMatch isFeatureConfigUpdateNotif ws
      notif %. "payload.0.name" `shouldMatch` featureName
      notif %. "payload.0.data" `shouldMatch` otherValue

    checkFeature featureName m tid otherValue
    assertSuccess =<< setFeatureConfig owner tid featureName (object ["status" .= defaultStatus])
    for_ wss $ \ws -> do
      notif <- awaitMatch isFeatureConfigUpdateNotif ws
      notif %. "payload.0.name" `shouldMatch` featureName
      notif %. "payload.0.data" `shouldMatch` defaultValue
    checkFeature featureName m tid defaultValue

testConversationGuestLinks :: HasCallStack => App ()
testConversationGuestLinks = _testSimpleFlagWithLockStatus "conversationGuestLinks" Public.setTeamFeatureConfig True True

testFileSharing :: HasCallStack => App ()
testFileSharing = _testSimpleFlagWithLockStatus "fileSharing" Public.setTeamFeatureConfig True True

testSndFactorPasswordChallenge :: HasCallStack => App ()
testSndFactorPasswordChallenge = _testSimpleFlagWithLockStatus "sndFactorPasswordChallenge" Public.setTeamFeatureConfig False False

testOutlookCalIntegration :: HasCallStack => App ()
testOutlookCalIntegration = _testSimpleFlagWithLockStatus "outlookCalIntegration" Public.setTeamFeatureConfig False False

testConversationGuestLinksInternal :: HasCallStack => App ()
testConversationGuestLinksInternal = _testSimpleFlagWithLockStatus "conversationGuestLinks" Internal.setTeamFeatureConfig True True

testFileSharingInternal :: HasCallStack => App ()
testFileSharingInternal = _testSimpleFlagWithLockStatus "fileSharing" Internal.setTeamFeatureConfig True True

testSndFactorPasswordChallengeInternal :: HasCallStack => App ()
testSndFactorPasswordChallengeInternal = _testSimpleFlagWithLockStatus "sndFactorPasswordChallenge" Internal.setTeamFeatureConfig False False

testOutlookCalIntegrationInternal :: HasCallStack => App ()
testOutlookCalIntegrationInternal = _testSimpleFlagWithLockStatus "outlookCalIntegration" Internal.setTeamFeatureConfig False False

_testSimpleFlagWithLockStatus ::
  HasCallStack =>
  String ->
  (Value -> String -> String -> Value -> App Response) ->
  Bool ->
  Bool ->
  App ()
_testSimpleFlagWithLockStatus featureName setFeatureConfig featureEnabledByDefault featureUnlockedByDefault = do
  -- let defaultStatus = if featureEnabledByDefault then "enabled" else "disabled"
  defaultValue <- (if featureEnabledByDefault then enabled else disabled) & setField "lockStatus" (if featureUnlockedByDefault then "unlocked" else "locked")
  let thisStatus = if featureEnabledByDefault then "enabled" else "disabled"
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
    assertSuccess =<< setFeatureConfig owner tid featureName (object ["status" .= otherStatus])
    for_ wss $ \ws -> do
      notif <- awaitMatch isFeatureConfigUpdateNotif ws
      notif %. "payload.0.name" `shouldMatch` featureName
      notif %. "payload.0.data" `shouldMatch` otherValue

  checkFeature featureName m tid otherValue

  bindResponse (setFeatureConfig owner tid featureName (object ["status" .= thisStatus])) $ \resp -> do
    resp.status `shouldMatchInt` 200
    checkFeature featureName m tid (object ["status" .= thisStatus, "lockStatus" .= "unlocked", "ttl" .= "unlimited"])

  bindResponse (setFeatureConfig owner tid featureName (object ["status" .= otherStatus])) $ \resp -> do
    resp.status `shouldMatchInt` 200
    checkFeature featureName m tid (object ["status" .= otherStatus, "lockStatus" .= "unlocked", "ttl" .= "unlimited"])

  -- lock feature
  Internal.setTeamFeatureLockStatus OwnDomain tid featureName "locked"

  -- feature status should be the default again
  checkFeature featureName m tid =<< setField "lockStatus" "locked" defaultValue
  assertStatus 409 =<< setFeatureConfig owner tid featureName (object ["status" .= otherStatus])

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
  withModifiedBackend def {galleyCfg = setField "settings.featureFlags.classifiedDomains" (object ["status" .= "disabled", "config" .= object ["domains" .= ["example.com"]]])} $ \domain -> do
    (_, tid, m : _) <- createTeam domain 2
    expected <- disabled & setField "config.domains" ["example.com"]
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
    Public.setTeamFeatureConfig
    (object ["lockStatus" .= "unlocked", "status" .= "enabled", "ttl" .= "unlimited", "config" .= object ["enforcedTimeoutSeconds" .= A.Number 0]])
    (object ["status" .= "disabled", "config" .= object ["enforcedTimeoutSeconds" .= A.Number 0]])
    (object ["status" .= "enabled", "config" .= object ["enforcedTimeoutSeconds" .= A.Number 30]])
    (object ["status" .= "enabled", "config" .= object ["enforcedTimeoutSeconds" .= ""]])

testSelfDeletingMessagesInternal :: HasCallStack => App ()
testSelfDeletingMessagesInternal =
  _testLockStatusWithConfig
    "selfDeletingMessages"
    Internal.setTeamFeatureConfig
    (object ["lockStatus" .= "unlocked", "status" .= "enabled", "ttl" .= "unlimited", "config" .= object ["enforcedTimeoutSeconds" .= A.Number 0]])
    (object ["status" .= "disabled", "config" .= object ["enforcedTimeoutSeconds" .= A.Number 0]])
    (object ["status" .= "enabled", "config" .= object ["enforcedTimeoutSeconds" .= A.Number 30]])
    (object ["status" .= "enabled", "config" .= object ["enforcedTimeoutSeconds" .= ""]])

testMls :: HasCallStack => App ()
testMls = do
  user <- randomUser OwnDomain def
  uid <- asString $ user %. "id"
  _testLockStatusWithConfig
    "mls"
    Public.setTeamFeatureConfig
    mlsDefaultConfig
    (mlsConfig1 uid)
    mlsConfig2
    mlsInvalidConfig

testMlsInternal :: HasCallStack => App ()
testMlsInternal = do
  user <- randomUser OwnDomain def
  uid <- asString $ user %. "id"
  _testLockStatusWithConfig
    "mls"
    Internal.setTeamFeatureConfig
    mlsDefaultConfig
    (mlsConfig1 uid)
    mlsConfig2
    mlsInvalidConfig

mlsDefaultConfig :: Value
mlsDefaultConfig =
  object
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
    ]

mlsConfig1 :: String -> Value
mlsConfig1 uid =
  object
    [ "status" .= "enabled",
      "config"
        .= object
          [ "protocolToggleUsers" .= [uid],
            "defaultProtocol" .= "mls",
            "supportedProtocols" .= ["proteus", "mls"],
            "allowedCipherSuites" .= ([1] :: [Int]),
            "defaultCipherSuite" .= A.Number 1
          ]
    ]

mlsConfig2 :: Value
mlsConfig2 =
  object
    [ "status" .= "enabled",
      "config"
        .= object
          [ "protocolToggleUsers" .= ([] :: [String]),
            "defaultProtocol" .= "mls",
            "supportedProtocols" .= ["mls"],
            "allowedCipherSuites" .= ([1] :: [Int]),
            "defaultCipherSuite" .= A.Number 1
          ]
    ]

mlsInvalidConfig :: Value
mlsInvalidConfig =
  object
    [ "status" .= "enabled",
      "config"
        .= object
          [ "protocolToggleUsers" .= ([] :: [String]),
            "defaultProtocol" .= "mls",
            "supportedProtocols" .= ["proteus"],
            "allowedCipherSuites" .= ([1] :: [Int]),
            "defaultCipherSuite" .= A.Number 1
          ]
    ]

testEnforceDownloadLocation :: HasCallStack => App ()
testEnforceDownloadLocation =
  _testLockStatusWithConfig
    "enforceFileDownloadLocation"
    Public.setTeamFeatureConfig
    (object ["lockStatus" .= "locked", "status" .= "disabled", "ttl" .= "unlimited", "config" .= object []])
    (object ["status" .= "enabled", "config" .= object ["enforcedDownloadLocation" .= "/tmp"]])
    (object ["status" .= "disabled", "config" .= object []])
    (object ["status" .= "enabled", "config" .= object ["enforcedDownloadLocation" .= object []]])

testEnforceDownloadLocationInternal :: HasCallStack => App ()
testEnforceDownloadLocationInternal =
  _testLockStatusWithConfig
    "enforceFileDownloadLocation"
    Internal.setTeamFeatureConfig
    (object ["lockStatus" .= "locked", "status" .= "disabled", "ttl" .= "unlimited", "config" .= object []])
    (object ["status" .= "enabled", "config" .= object ["enforcedDownloadLocation" .= "/tmp"]])
    (object ["status" .= "disabled", "config" .= object []])
    (object ["status" .= "enabled", "config" .= object ["enforcedDownloadLocation" .= object []]])

testMlsMigration :: HasCallStack => App ()
testMlsMigration = do
  -- first we have to enable mls
  (owner, tid, m : _) <- createTeam OwnDomain 2
  assertSuccess =<< Public.setTeamFeatureConfig owner tid "mls" mlsEnableConfig
  _testLockStatusWithConfigWithTeam
    (owner, tid, m)
    "mlsMigration"
    Public.setTeamFeatureConfig
    mlsMigrationDefaultConfig
    mlsMigrationConfig1
    mlsMigrationConfig2
    mlsMigrationInvalidConfig

testMlsMigrationInternal :: HasCallStack => App ()
testMlsMigrationInternal = do
  -- first we have to enable mls
  (owner, tid, m : _) <- createTeam OwnDomain 2
  assertSuccess =<< Public.setTeamFeatureConfig owner tid "mls" mlsEnableConfig
  _testLockStatusWithConfigWithTeam
    (owner, tid, m)
    "mlsMigration"
    Internal.setTeamFeatureConfig
    mlsMigrationDefaultConfig
    mlsMigrationConfig1
    mlsMigrationConfig2
    mlsMigrationInvalidConfig

mlsEnableConfig :: Value
mlsEnableConfig =
  object
    [ "status" .= "enabled",
      "config"
        .= object
          [ "protocolToggleUsers" .= ([] :: [String]),
            "defaultProtocol" .= "mls",
            "supportedProtocols" .= ["mls"],
            "allowedCipherSuites" .= ([1] :: [Int]),
            "defaultCipherSuite" .= A.Number 1
          ]
    ]

mlsMigrationDefaultConfig :: Value
mlsMigrationDefaultConfig =
  object
    [ "lockStatus" .= "locked",
      "status" .= "enabled",
      "ttl" .= "unlimited",
      "config"
        .= object
          [ "startTime" .= "2029-05-16T10:11:12.123Z",
            "finaliseRegardlessAfter" .= "2029-10-17T00:00:00Z"
          ]
    ]

mlsMigrationConfig1 :: Value
mlsMigrationConfig1 =
  object
    [ "status" .= "enabled",
      "config"
        .= object
          [ "startTime" .= "2029-05-16T10:11:12.123Z",
            "finaliseRegardlessAfter" .= "2030-10-17T00:00:00Z"
          ]
    ]

mlsMigrationConfig2 :: Value
mlsMigrationConfig2 =
  object
    [ "status" .= "enabled",
      "config"
        .= object
          [ "startTime" .= "2030-05-16T10:11:12.123Z",
            "finaliseRegardlessAfter" .= "2031-10-17T00:00:00Z"
          ]
    ]

mlsMigrationInvalidConfig :: Value
mlsMigrationInvalidConfig =
  object
    [ "status" .= "enabled",
      "config"
        .= object
          [ "startTime" .= A.Number 1
          ]
    ]

mlsE2EIdConfig :: App (Value, Value, Value, Value)
mlsE2EIdConfig = do
  cfg2 <-
    mlsE2EIdConfig1
      & setField "config.verificationExpiration" (A.Number 86401)
      & setField "config.useProxyOnMobile" True
  invalidConfig <- cfg2 & removeField "config.crlProxy"
  pure (mlsE2EIdDefConfig, mlsE2EIdConfig1, cfg2, invalidConfig)
  where
    mlsE2EIdDefConfig :: Value
    mlsE2EIdDefConfig =
      object
        [ "lockStatus" .= "unlocked",
          "status" .= "disabled",
          "ttl" .= "unlimited",
          "config"
            .= object
              [ "verificationExpiration" .= A.Number 86400,
                "useProxyOnMobile" .= False
              ]
        ]
    mlsE2EIdConfig1 :: Value
    mlsE2EIdConfig1 =
      object
        [ "status" .= "enabled",
          "config"
            .= object
              [ "crlProxy" .= "https://example.com",
                "verificationExpiration" .= A.Number 86400,
                "useProxyOnMobile" .= False
              ]
        ]

testMLSE2EId :: HasCallStack => App ()
testMLSE2EId = do
  (defCfg, cfg1, cfg2, invalidCfg) <- mlsE2EIdConfig
  _testLockStatusWithConfig
    "mlsE2EId"
    Public.setTeamFeatureConfig
    defCfg
    cfg1
    cfg2
    invalidCfg

testMLSE2EIdInternal :: HasCallStack => App ()
testMLSE2EIdInternal = do
  (defCfg, cfg1, cfg2, invalidCfg) <- mlsE2EIdConfig
  -- the internal API is not as strict as the public one, so we need to tweak the invalid config some more
  invalidCfg' <- invalidCfg & setField "config.crlProxy" (object [])
  _testLockStatusWithConfig
    "mlsE2EId"
    Internal.setTeamFeatureConfig
    defCfg
    cfg1
    cfg2
    invalidCfg'

_testLockStatusWithConfig ::
  HasCallStack =>
  String ->
  (Value -> String -> String -> Value -> App Response) ->
  -- | the default feature config (should include the lock status and ttl, as it is returned by the API)
  Value ->
  -- | a valid config used to update the feature setting (should not include the lock status and ttl, as these are not part of the request payload)
  Value ->
  -- | another valid config
  Value ->
  -- | an invalid config
  Value ->
  App ()
_testLockStatusWithConfig featureName setTeamFeatureConfig defaultFeatureConfig config1 config2 invalidConfig = do
  (owner, tid, m : _) <- createTeam OwnDomain 2
  _testLockStatusWithConfigWithTeam (owner, tid, m) featureName setTeamFeatureConfig defaultFeatureConfig config1 config2 invalidConfig

_testLockStatusWithConfigWithTeam ::
  HasCallStack =>
  -- | (owner, tid, member)
  (Value, String, Value) ->
  String ->
  (Value -> String -> String -> Value -> App Response) ->
  -- | the default feature config (should include the lock status and ttl, as it is returned by the API)
  Value ->
  -- | a valid config used to update the feature setting (should not include the lock status and ttl, as these are not part of the request payload)
  Value ->
  -- | another valid config
  Value ->
  -- | an invalid config
  Value ->
  App ()
_testLockStatusWithConfigWithTeam (owner, tid, m) featureName setTeamFeatureConfig defaultFeatureConfig config1 config2 invalidConfig = do
  -- personal user
  randomPersonalUser <- randomUser OwnDomain def

  bindResponse (Public.getFeatureConfigs randomPersonalUser) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. featureName `shouldMatch` defaultFeatureConfig

  -- team user
  nonTeamMember <- randomUser OwnDomain def
  assertForbidden =<< Public.getTeamFeature nonTeamMember tid featureName

  checkFeature featureName m tid defaultFeatureConfig

  -- lock the feature
  Internal.setTeamFeatureLockStatus OwnDomain tid featureName "locked"
  assertStatus 409 =<< setTeamFeatureConfig owner tid featureName config1
  Internal.setTeamFeatureLockStatus OwnDomain tid featureName "unlocked"

  void $ withWebSockets [m] $ \wss -> do
    assertSuccess =<< setTeamFeatureConfig owner tid featureName config1
    for_ wss $ \ws -> do
      notif <- awaitMatch isFeatureConfigUpdateNotif ws
      notif %. "payload.0.name" `shouldMatch` featureName
      notif %. "payload.0.data" `shouldMatch` (config1 & setField "lockStatus" "unlocked" & setField "ttl" "unlimited")

  checkFeature featureName m tid =<< (config1 & setField "lockStatus" "unlocked" & setField "ttl" "unlimited")

  Internal.setTeamFeatureLockStatus OwnDomain tid featureName "locked"
  checkFeature featureName m tid =<< setField "lockStatus" "locked" defaultFeatureConfig
  Internal.setTeamFeatureLockStatus OwnDomain tid featureName "unlocked"

  void $ withWebSockets [m] $ \wss -> do
    assertStatus 400 =<< setTeamFeatureConfig owner tid featureName invalidConfig
    for_ wss $ assertNoEvent 2

  checkFeature featureName m tid =<< (config1 & setField "lockStatus" "unlocked" & setField "ttl" "unlimited")

  void $ withWebSockets [m] $ \wss -> do
    assertSuccess =<< setTeamFeatureConfig owner tid featureName config2
    for_ wss $ \ws -> do
      notif <- awaitMatch isFeatureConfigUpdateNotif ws
      notif %. "payload.0.name" `shouldMatch` featureName
      notif %. "payload.0.data" `shouldMatch` (config2 & setField "lockStatus" "unlocked" & setField "ttl" "unlimited")

  checkFeature featureName m tid =<< (config2 & setField "lockStatus" "unlocked" & setField "ttl" "unlimited")

testFeatureNoConfigMultiSearchVisibilityInbound :: HasCallStack => App ()
testFeatureNoConfigMultiSearchVisibilityInbound = do
  (_owner1, team1, _) <- createTeam OwnDomain 0
  (_owner2, team2, _) <- createTeam OwnDomain 0

  assertSuccess =<< Internal.setTeamFeatureStatus OwnDomain team2 "searchVisibilityInbound" "enabled"

  response <- Internal.getFeatureStatusMulti OwnDomain "searchVisibilityInbound" [team1, team2]

  statuses <- response.json %. "default_status" >>= asList
  length statuses `shouldMatchInt` 2
  statuses `shouldMatchSet` [object ["team" .= team1, "status" .= "disabled"], object ["team" .= team2, "status" .= "enabled"]]

testConferenceCallingTTLIncreaseToUnlimited :: HasCallStack => App ()
testConferenceCallingTTLIncreaseToUnlimited = _testSimpleFlagTTLOverride "conferenceCalling" True (Just 2) Nothing

testConferenceCallingTTLIncrease :: HasCallStack => App ()
testConferenceCallingTTLIncrease = _testSimpleFlagTTLOverride "conferenceCalling" True (Just 2) (Just 4)

testConferenceCallingTTLReduceFromUnlimited :: HasCallStack => App ()
testConferenceCallingTTLReduceFromUnlimited = _testSimpleFlagTTLOverride "conferenceCalling" True Nothing (Just 2)

testConferenceCallingTTLReduce :: HasCallStack => App ()
testConferenceCallingTTLReduce = _testSimpleFlagTTLOverride "conferenceCalling" True (Just 5) (Just 2)

testConferenceCallingTTLUnlimitedToUnlimited :: HasCallStack => App ()
testConferenceCallingTTLUnlimitedToUnlimited = _testSimpleFlagTTLOverride "conferenceCalling" True Nothing Nothing

_testSimpleFlagTTLOverride :: HasCallStack => String -> Bool -> Maybe Int -> Maybe Int -> App ()
_testSimpleFlagTTLOverride featureName enabledByDefault mTtl mTtlAfter = do
  let ttl = maybe (A.String . cs $ "unlimited") (A.Number . fromIntegral) mTtl
  let ttlAfter = maybe (A.String . cs $ "unlimited") (A.Number . fromIntegral) mTtlAfter
  (owner, tid, _) <- createTeam OwnDomain 0
  let (defaultValue, otherValue) = if enabledByDefault then ("enabled", "disabled") else ("disabled", "enabled")

  -- Initial value should be the default value
  let defFeatureStatus = object ["status" .= defaultValue, "ttl" .= "unlimited", "lockStatus" .= "unlocked"]
  checkFeature featureName owner tid defFeatureStatus

  -- Setting should work
  assertSuccess =<< Internal.setTeamFeatureConfig OwnDomain tid featureName (object ["status" .= otherValue, "ttl" .= ttl])
  checkFeatureLenientTtl featureName owner tid (object ["status" .= otherValue, "ttl" .= ttl, "lockStatus" .= "unlocked"])

  case (mTtl, mTtlAfter) of
    (Just d, Just d') -> do
      -- wait less than expiration, override and recheck.
      liftIO $ threadDelay (d * 1000000 `div` 2) -- waiting half of TTL
      --       setFlagInternal otherValue ttlAfter
      assertSuccess =<< Internal.setTeamFeatureConfig OwnDomain tid featureName (object ["status" .= otherValue, "ttl" .= ttlAfter])
      -- value is still correct
      checkFeatureLenientTtl featureName owner tid (object ["status" .= otherValue, "ttl" .= ttlAfter, "lockStatus" .= "unlocked"])

      liftIO $ threadDelay (d' * 1000000) -- waiting for new TTL
      checkFeatureLenientTtl featureName owner tid defFeatureStatus
    (Just d, Nothing) -> do
      -- wait less than expiration, override and recheck.
      liftIO $ threadDelay (d * 1000000 `div` 2) -- waiting half of TTL
      assertSuccess =<< Internal.setTeamFeatureConfig OwnDomain tid featureName (object ["status" .= otherValue, "ttl" .= ttlAfter])
      -- value is still correct
      checkFeatureLenientTtl featureName owner tid (object ["status" .= otherValue, "ttl" .= ttlAfter, "lockStatus" .= "unlocked"])
    (Nothing, Nothing) -> do
      -- overriding in this case should have no effect.
      assertSuccess =<< Internal.setTeamFeatureConfig OwnDomain tid featureName (object ["status" .= otherValue, "ttl" .= ttl])
      checkFeatureLenientTtl featureName owner tid (object ["status" .= otherValue, "ttl" .= ttl, "lockStatus" .= "unlocked"])
    (Nothing, Just d) -> do
      assertSuccess =<< Internal.setTeamFeatureConfig OwnDomain tid featureName (object ["status" .= otherValue, "ttl" .= ttlAfter])
      checkFeatureLenientTtl featureName owner tid (object ["status" .= otherValue, "ttl" .= ttlAfter, "lockStatus" .= "unlocked"])
      liftIO $ threadDelay (d * 1000000) -- waiting it out
      -- value reverts back
      checkFeatureLenientTtl featureName owner tid defFeatureStatus

--------------------------------------------------------------------------------
-- Simple flags with implicit lock status

testPatchSearchVisibility :: HasCallStack => App ()
testPatchSearchVisibility = _testPatch "searchVisibility" False disabled enabled

testPatchValidateSAMLEmails :: HasCallStack => App ()
testPatchValidateSAMLEmails = _testPatch "validateSAMLemails" False enabled disabled

testPatchDigitalSignatures :: HasCallStack => App ()
testPatchDigitalSignatures = _testPatch "digitalSignatures" False disabled enabled

testPatchConferenceCalling :: HasCallStack => App ()
testPatchConferenceCalling = _testPatch "conferenceCalling" False enabled disabled

--------------------------------------------------------------------------------
-- Simple flags with explicit lock status

testPatchFileSharing :: HasCallStack => App ()
testPatchFileSharing = _testPatch "fileSharing" True enabled disabled

testPatchGuestLinks :: HasCallStack => App ()
testPatchGuestLinks = _testPatch "conversationGuestLinks" True enabled disabled

testPatchSndFactorPasswordChallenge :: HasCallStack => App ()
testPatchSndFactorPasswordChallenge = _testPatch "sndFactorPasswordChallenge" True disabledLocked enabled

testPatchOutlookCalIntegration :: HasCallStack => App ()
testPatchOutlookCalIntegration = _testPatch "outlookCalIntegration" True disabledLocked enabled

--------------------------------------------------------------------------------
-- Flags with config & implicit lock status

testPatchAppLock :: HasCallStack => App ()
testPatchAppLock = do
  let defCfg =
        object
          [ "lockStatus" .= "unlocked",
            "status" .= "enabled",
            "ttl" .= "unlimited",
            "config" .= object ["enforceAppLock" .= False, "inactivityTimeoutSecs" .= A.Number 60]
          ]
  _testPatch "appLock" False defCfg (object ["lockStatus" .= "locked"])
  _testPatch "appLock" False defCfg (object ["status" .= "disabled"])
  _testPatch "appLock" False defCfg (object ["lockStatus" .= "locked", "status" .= "disabled"])
  _testPatch "appLock" False defCfg (object ["lockStatus" .= "unlocked", "config" .= object ["enforceAppLock" .= True, "inactivityTimeoutSecs" .= A.Number 120]])
  _testPatch "appLock" False defCfg (object ["config" .= object ["enforceAppLock" .= True, "inactivityTimeoutSecs" .= A.Number 240]])

--------------------------------------------------------------------------------
-- Flags with config & explicit lock status

testPatchSelfDeletingMessages :: HasCallStack => App ()
testPatchSelfDeletingMessages = do
  let defCfg =
        object
          [ "lockStatus" .= "unlocked",
            "status" .= "enabled",
            "ttl" .= "unlimited",
            "config" .= object ["enforcedTimeoutSeconds" .= A.Number 0]
          ]
  _testPatch "selfDeletingMessages" True defCfg (object ["lockStatus" .= "locked"])
  _testPatch "selfDeletingMessages" True defCfg (object ["status" .= "disabled"])
  _testPatch "selfDeletingMessages" True defCfg (object ["lockStatus" .= "locked", "status" .= "disabled"])
  _testPatch "selfDeletingMessages" True defCfg (object ["lockStatus" .= "unlocked", "config" .= object ["enforcedTimeoutSeconds" .= A.Number 30]])
  _testPatch "selfDeletingMessages" True defCfg (object ["config" .= object ["enforcedTimeoutSeconds" .= A.Number 60]])

testPatchEnforceFileDownloadLocation :: HasCallStack => App ()
testPatchEnforceFileDownloadLocation = do
  let defCfg =
        object
          [ "lockStatus" .= "locked",
            "status" .= "disabled",
            "ttl" .= "unlimited",
            "config" .= object []
          ]
  _testPatch "enforceFileDownloadLocation" True defCfg (object ["lockStatus" .= "unlocked"])
  _testPatch "enforceFileDownloadLocation" True defCfg (object ["status" .= "enabled"])
  _testPatch "enforceFileDownloadLocation" True defCfg (object ["lockStatus" .= "unlocked", "status" .= "enabled"])
  _testPatch "enforceFileDownloadLocation" True defCfg (object ["lockStatus" .= "locked", "config" .= object []])
  _testPatch "enforceFileDownloadLocation" True defCfg (object ["config" .= object ["enforcedDownloadLocation" .= "/tmp"]])

testPatchE2EId :: HasCallStack => App ()
testPatchE2EId = do
  let defCfg =
        object
          [ "lockStatus" .= "unlocked",
            "status" .= "disabled",
            "ttl" .= "unlimited",
            "config"
              .= object
                [ "verificationExpiration" .= A.Number 86400,
                  "useProxyOnMobile" .= False
                ]
          ]
  _testPatch "mlsE2EId" True defCfg (object ["lockStatus" .= "locked"])
  _testPatch "mlsE2EId" True defCfg (object ["status" .= "enabled"])
  _testPatch "mlsE2EId" True defCfg (object ["lockStatus" .= "locked", "status" .= "enabled"])
  _testPatch
    "mlsE2EId"
    True
    defCfg
    ( object
        [ "lockStatus" .= "unlocked",
          "config"
            .= object
              [ "crlProxy" .= "https://example.com",
                "verificationExpiration" .= A.Number 86401,
                "useProxyOnMobile" .= True
              ]
        ]
    )
  _testPatch
    "mlsE2EId"
    True
    defCfg
    ( object
        [ "config"
            .= object
              [ "crlProxy" .= "https://example.com",
                "verificationExpiration" .= A.Number 86401,
                "useProxyOnMobile" .= True
              ]
        ]
    )

testPatchMLS :: HasCallStack => App ()
testPatchMLS = do
  dom <- asString OwnDomain
  (_, tid, _) <- createTeam dom 0
  assertSuccess
    =<< Internal.patchTeamFeature
      dom
      tid
      "mlsMigration"
      (object ["status" .= "disabled", "lockStatus" .= "unlocked"])
  let defCfg =
        object
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
          ]
  _testPatchWithSetup mlsMigrationSetup dom "mls" True defCfg (object ["lockStatus" .= "locked"])
  _testPatchWithSetup mlsMigrationSetup dom "mls" True defCfg (object ["status" .= "enabled"])
  _testPatchWithSetup mlsMigrationSetup dom "mls" True defCfg (object ["lockStatus" .= "locked", "status" .= "enabled"])
  _testPatchWithSetup
    mlsMigrationSetup
    dom
    "mls"
    True
    defCfg
    ( object
        [ "status" .= "enabled",
          "config"
            .= object
              [ "protocolToggleUsers" .= ([] :: [String]),
                "defaultProtocol" .= "mls",
                "supportedProtocols" .= ["proteus", "mls"],
                "allowedCipherSuites" .= ([1] :: [Int]),
                "defaultCipherSuite" .= A.Number 1
              ]
        ]
    )
  _testPatchWithSetup
    mlsMigrationSetup
    dom
    "mls"
    True
    defCfg
    ( object
        [ "config"
            .= object
              [ "protocolToggleUsers" .= ([] :: [String]),
                "defaultProtocol" .= "mls",
                "supportedProtocols" .= ["proteus", "mls"],
                "allowedCipherSuites" .= ([1] :: [Int]),
                "defaultCipherSuite" .= A.Number 1
              ]
        ]
    )
  where
    mlsMigrationSetup :: HasCallStack => String -> String -> App ()
    mlsMigrationSetup dom tid =
      assertSuccess
        =<< Internal.patchTeamFeature
          dom
          tid
          "mlsMigration"
          (object ["status" .= "disabled", "lockStatus" .= "unlocked"])

_testPatch :: HasCallStack => String -> Bool -> Value -> Value -> App ()
_testPatch featureName hasExplicitLockStatus defaultFeatureConfig patch = do
  dom <- asString OwnDomain
  _testPatchWithSetup
    (\_ _ -> pure ())
    dom
    featureName
    hasExplicitLockStatus
    defaultFeatureConfig
    patch

_testPatchWithSetup ::
  HasCallStack =>
  (String -> String -> App ()) ->
  String ->
  String ->
  Bool ->
  Value ->
  Value ->
  App ()
_testPatchWithSetup setup domain featureName hasExplicitLockStatus defaultFeatureConfig patch = do
  (owner, tid, _) <- createTeam domain 0
  -- run a feature-specific setup. For most features this is a no-op.
  setup domain tid

  checkFeature featureName owner tid defaultFeatureConfig
  assertSuccess =<< Internal.patchTeamFeature domain tid featureName patch
  patched <- (.json) =<< Internal.getTeamFeature domain tid featureName
  checkFeature featureName owner tid patched
  lockStatus <- patched %. "lockStatus" >>= asString
  if lockStatus == "locked"
    then do
      -- if lock status is locked the feature status should fall back to the default
      patched `shouldMatch` (defaultFeatureConfig & setField "lockStatus" "locked")
      -- if lock status is locked, it was either locked before or changed by the patch
      mPatchedLockStatus <- lookupField patch "lockStatus"
      case mPatchedLockStatus of
        Just ls -> ls `shouldMatch` "locked"
        Nothing -> defaultFeatureConfig %. "lockStatus" `shouldMatch` "locked"
    else do
      patched %. "status" `shouldMatch` valueOrDefault "status"
      mPatchedConfig <- lookupField patched "config"
      case mPatchedConfig of
        Just patchedConfig -> patchedConfig `shouldMatch` valueOrDefault "config"
        Nothing -> do
          mDefConfig <- lookupField defaultFeatureConfig "config"
          assertBool "patch had an unexpected config field" (isNothing mDefConfig)

      when hasExplicitLockStatus $ do
        -- if lock status is unlocked, it was either unlocked before or changed by the patch
        mPatchedLockStatus <- lookupField patch "lockStatus"
        case mPatchedLockStatus of
          Just ls -> ls `shouldMatch` "unlocked"
          Nothing -> defaultFeatureConfig %. "lockStatus" `shouldMatch` "unlocked"
  where
    valueOrDefault :: String -> App Value
    valueOrDefault key = do
      mValue <- lookupField patch key
      maybe (defaultFeatureConfig %. key) pure mValue
