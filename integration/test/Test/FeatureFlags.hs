{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

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
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Set as Set
import Notifications
import SetupHelpers
import Test.FeatureFlags.Util
import Testlib.Prelude

testLimitedEventFanout :: (HasCallStack) => App ()
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

_testSimpleFlag :: (HasCallStack) => String -> (Value -> String -> String -> Value -> App Response) -> Bool -> App ()
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
  void $ withWebSocket m $ \ws -> do
    assertSuccess =<< setFeatureConfig owner tid featureName (object ["status" .= otherStatus])
    do
      notif <- awaitMatch isFeatureConfigUpdateNotif ws
      notif %. "payload.0.name" `shouldMatch` featureName
      notif %. "payload.0.data" `shouldMatch` otherValue

    checkFeature featureName m tid otherValue
    assertSuccess =<< setFeatureConfig owner tid featureName (object ["status" .= defaultStatus])
    do
      notif <- awaitMatch isFeatureConfigUpdateNotif ws
      notif %. "payload.0.name" `shouldMatch` featureName
      notif %. "payload.0.data" `shouldMatch` defaultValue
    checkFeature featureName m tid defaultValue

_testSimpleFlagWithLockStatus ::
  (HasCallStack) =>
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
  void $ withWebSocket m $ \ws -> do
    assertSuccess =<< setFeatureConfig owner tid featureName (object ["status" .= otherStatus])
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

-- | Call 'GET /teams/:tid/features' and 'GET /feature-configs', and check if all
-- features are there.
testAllFeatures :: (HasCallStack) => App ()
testAllFeatures = do
  (_, tid, m : _) <- createTeam OwnDomain 2
  let defEnabledObj :: Value -> Value
      defEnabledObj conf = object ["lockStatus" .= "unlocked", "status" .= "enabled", "ttl" .= "unlimited", "config" .= conf]
      expected =
        object
          $ [ "legalhold" .= disabled,
              "sso" .= disabled,
              "searchVisibility" .= disabled,
              "validateSAMLemails" .= enabled,
              "digitalSignatures" .= disabled,
              "appLock" .= defEnabledObj (object ["enforceAppLock" .= False, "inactivityTimeoutSecs" .= A.Number 60]),
              "fileSharing" .= enabled,
              "classifiedDomains" .= defEnabledObj (object ["domains" .= ["example.com"]]),
              "conferenceCalling" .= confCalling def {lockStatus = Just "locked"},
              "selfDeletingMessages"
                .= defEnabledObj (object ["enforcedTimeoutSeconds" .= A.Number 0]),
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
                          "useProxyOnMobile" .= False,
                          "crlProxy" .= "https://crlproxy.example.com"
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
              "enforceFileDownloadLocation"
                .= object
                  [ "lockStatus" .= "unlocked",
                    "status" .= "disabled",
                    "ttl" .= "unlimited",
                    "config"
                      .= object
                        [ "enforcedDownloadLocation" .= "downloads"
                        ]
                  ],
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

testFeatureConfigConsistency :: (HasCallStack) => App ()
testFeatureConfigConsistency = do
  (_, tid, m : _) <- createTeam OwnDomain 2

  allFeaturesRes <- Public.getFeatureConfigs m >>= parseObjectKeys

  allTeamFeaturesRes <- Public.getTeamFeatures m tid >>= parseObjectKeys

  unless (allTeamFeaturesRes `Set.isSubsetOf` allFeaturesRes)
    $ assertFailure (show allTeamFeaturesRes <> " is not a subset of " <> show allFeaturesRes)
  where
    parseObjectKeys :: Response -> App (Set.Set String)
    parseObjectKeys res = do
      val <- res.json
      case val of
        (A.Object hm) -> pure (Set.fromList . map (show . A.toText) . KM.keys $ hm)
        x -> assertFailure ("JSON was not an object, but " <> show x)
