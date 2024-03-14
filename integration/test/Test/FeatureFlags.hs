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

import API.Galley
import API.GalleyCommon
import qualified API.GalleyInternal as I
import qualified Data.Aeson as Aeson
import Notifications
import SetupHelpers
import Testlib.HTTP
import Testlib.Prelude

--------------------------------------------------------------------------------
-- Helpers

expectedStatus :: (ToJSON cfg, HasCallStack) => FeatureStatus -> cfg -> String -> Aeson.Value
expectedStatus fs cfg lock =
  Aeson.object ["lockStatus" .= lock, "status" .= show fs, "ttl" .= "unlimited", "config" .= toJSON cfg]

assertFeatureLock ::
  ( HasCallStack,
    MakesValue user,
    MakesValue team,
    ToJSON cfg
  ) =>
  String ->
  cfg ->
  FeatureStatus ->
  String ->
  user ->
  team ->
  App ()
assertFeatureLock lock cfg status featureName user team = do
  tf <- getTeamFeature featureName user team
  assertSuccessMatchBody (expectedStatus status cfg lock) tf

assertFeature ::
  ( HasCallStack,
    MakesValue user,
    MakesValue team,
    ToJSON cfg
  ) =>
  cfg ->
  FeatureStatus ->
  String ->
  user ->
  team ->
  App ()
assertFeature = assertFeatureLock "unlocked"

assertFeatureFromAll ::
  (HasCallStack, MakesValue user) =>
  String ->
  user ->
  FeatureStatus ->
  App ()
assertFeatureFromAll = assertFeatureFromAllLock Nothing

assertFeatureFromAllLock ::
  (HasCallStack, MakesValue user) =>
  Maybe String ->
  String ->
  user ->
  FeatureStatus ->
  App ()
assertFeatureFromAllLock mLock featureName user expected = do
  actual <- extractTeamFeatureFromAll featureName user
  actual %. "status" `shouldMatch` show expected
  for_ mLock (actual %. "lockStatus" `shouldMatch`)

assertFeatureInternalLock ::
  (HasCallStack, MakesValue domain, ToJSON cfg) =>
  String ->
  cfg ->
  FeatureStatus ->
  String ->
  domain ->
  String ->
  App ()
assertFeatureInternalLock lock cfg status featureName dom team = do
  tf <- I.getTeamFeature dom featureName team
  assertSuccessMatchBody (expectedStatus status cfg lock) tf

assertFeatureInternal ::
  (HasCallStack, MakesValue domain, ToJSON cfg) =>
  cfg ->
  FeatureStatus ->
  String ->
  domain ->
  String ->
  App ()
assertFeatureInternal = assertFeatureInternalLock "unlocked"

--------------------------------------------------------------------------------

testLimitedEventFanout :: HasCallStack => App ()
testLimitedEventFanout = do
  let featureName = "limitedEventFanout"
      cfg = ()
  (_alice, team, _) <- createTeam OwnDomain 1
  assertFeatureInternal cfg Disabled featureName OwnDomain team
  I.setTeamFeatureStatus OwnDomain team featureName Enabled
  assertFeatureInternal cfg Enabled featureName OwnDomain team

testSSOPut :: HasCallStack => FeatureStatus -> App ()
testSSOPut = genericTestSSO putInternal
  where
    putInternal domain team status = do
      let st = WithStatusNoLock status () 0
      I.putTeamFeatureStatus domain team "sso" st

testSSOPatch :: HasCallStack => FeatureStatus -> App ()
testSSOPatch = genericTestSSO patchInternal
  where
    patchInternal domain team status =
      I.patchTeamFeatureStatus domain team "sso" status

genericTestSSO ::
  HasCallStack =>
  (String -> String -> FeatureStatus -> App ()) ->
  FeatureStatus ->
  App ()
genericTestSSO setter status = withModifiedBackend cnf $ \domain -> do
  (_alice, team, alex : _) <- createTeam domain 2
  nonMember <- randomUser domain def

  getTeamFeature featureName nonMember team `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "no-team-member"

  assertFeature cfg status featureName alex team
  assertFeatureInternal cfg status featureName domain team
  assertFeatureFromAll featureName alex status

  when (status == Disabled) $ do
    let opposite = Enabled
    setter domain team opposite
    assertFeature cfg opposite featureName alex team
    assertFeatureInternal cfg opposite featureName domain team
    assertFeatureFromAll featureName alex opposite
  where
    featureName = "sso"
    setting = "settings.featureFlags." <> featureName
    cnf =
      def
        { galleyCfg = \conf ->
            conf
              & setField setting (show status <> "-by-default")
        }
    cfg = ()

legalholdAssertions ::
  (HasCallStack, MakesValue user) =>
  String ->
  String ->
  user ->
  App ()
legalholdAssertions domain team mem = do
  assertFeature cfg Disabled featureName mem team
  assertFeatureInternal cfg Disabled featureName domain team
  assertFeatureFromAll featureName mem Disabled
  nonMem <- randomUser domain def
  getTeamFeature featureName nonMem team `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "no-team-member"
  where
    featureName = "legalhold"
    cfg = ()

legalholdDisabledByDefault ::
  HasCallStack =>
  (String -> String -> FeatureStatus -> App ()) ->
  App ()
legalholdDisabledByDefault setter = withModifiedBackend cnf $ \domain -> do
  (_alice, team, alex : _) <- createTeam domain 2
  legalholdAssertions domain team alex
  setter domain team Enabled
  assertFeature cfg Enabled featureName alex team
  assertFeatureInternal cfg Enabled featureName domain team
  assertFeatureFromAll featureName alex Enabled
  where
    cnf =
      def
        { galleyCfg = \conf ->
            conf
              -- & setField setting ("whitelist-teams-and-implicit-consent")
              & setField setting ("disabled-by-default")
        }
    setting = "settings.featureFlags." <> featureName
    featureName = "legalhold"
    cfg = ()

testLegalholdDisabledByDefaultPut :: HasCallStack => App ()
testLegalholdDisabledByDefaultPut = legalholdDisabledByDefault putInternal
  where
    putInternal domain team status = do
      let st = WithStatusNoLock status () 0
      I.putTeamFeatureStatus domain team "legalhold" st

testLegalholdDisabledByDefaultPatch :: HasCallStack => App ()
testLegalholdDisabledByDefaultPatch = legalholdDisabledByDefault patchInternal
  where
    patchInternal domain team status =
      I.patchTeamFeatureStatus domain team "legalhold" status

legalholdFailToEnable ::
  HasCallStack =>
  (String -> String -> FeatureStatus -> App ()) ->
  String ->
  App ()
legalholdFailToEnable setter confValue = withModifiedBackend cnf $ \domain -> do
  (_alice, team, alex : _) <- createTeam domain 2
  legalholdAssertions domain team alex
  setter domain team Enabled
  where
    cnf =
      def
        { galleyCfg = \conf ->
            conf
              & setField setting confValue
        }
    setting = "settings.featureFlags." <> featureName
    featureName = "legalhold"

testLegalholdDisabledPermanentlyPatch :: HasCallStack => App ()
testLegalholdDisabledPermanentlyPatch =
  legalholdFailToEnable patchInternal "disabled-permanently"
  where
    patchInternal domain team status =
      I.failToPatchTeamFeatureStatus domain team "legalhold" status

testLegalholdDisabledPermanentlyPut :: HasCallStack => App ()
testLegalholdDisabledPermanentlyPut =
  legalholdFailToEnable putInternal "disabled-permanently"
  where
    putInternal domain team status = do
      let st = WithStatusNoLock status () 0
      I.failToPutTeamFeatureStatus domain team "legalhold" st

testLegalholdWhitelistImplicitConsentPatch :: HasCallStack => App ()
testLegalholdWhitelistImplicitConsentPatch =
  legalholdFailToEnable patchInternal "whitelist-teams-and-implicit-consent"
  where
    patchInternal domain team status =
      I.failToPatchTeamFeatureStatus domain team "legalhold" status

testLegalholdWhitelistImplicitConsentPut :: HasCallStack => App ()
testLegalholdWhitelistImplicitConsentPut =
  legalholdFailToEnable putInternal "whitelist-teams-and-implicit-consent"
  where
    putInternal domain team status = do
      let st = WithStatusNoLock status () 0
      I.failToPutTeamFeatureStatus domain team "legalhold" st

genericSearchVisibility ::
  HasCallStack =>
  (String -> String -> FeatureStatus -> App ()) ->
  FeatureStatus ->
  App ()
genericSearchVisibility setter status = withModifiedBackend cnf $ \domain -> do
  (alice, team, alex : _) <- createTeam domain 2
  nonMem <- randomUser domain def
  getTeamFeature featurePath nonMem team `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "no-team-member"

  assertFeature cfg status featurePath alice team
  assertFeatureInternal cfg status featurePath domain team
  assertFeatureFromAll featurePath alex status

  let opposite = oppositeStatus status
  setter domain team opposite
  assertFeature cfg opposite featurePath alice team
  assertFeatureInternal cfg opposite featurePath domain team
  assertFeatureFromAll featurePath alex opposite

  setter domain team status
  assertFeature cfg status featurePath alice team
  assertFeatureInternal cfg status featurePath domain team
  assertFeatureFromAll featurePath alex status
  where
    cnf =
      def
        { galleyCfg = \conf ->
            conf
              & setField setting (show status <> "-by-default")
        }
    setting = "settings.featureFlags." <> featureName
    featureName = "teamSearchVisibility"
    featurePath = "searchVisibility"
    cfg = ()

testSearchVisibilityDisabledPatch :: HasCallStack => App ()
testSearchVisibilityDisabledPatch =
  genericSearchVisibility patchInternal Disabled
  where
    featurePath = "searchVisibility"
    patchInternal domain team status =
      I.patchTeamFeatureStatus domain team featurePath status

testSearchVisibilityDisabledPut :: HasCallStack => App ()
testSearchVisibilityDisabledPut =
  genericSearchVisibility putInternal Disabled
  where
    featurePath = "searchVisibility"
    putInternal domain team status = do
      let st = WithStatusNoLock status () 0
      I.putTeamFeatureStatus domain team featurePath st

testSearchVisibilityEnabledPatch :: HasCallStack => App ()
testSearchVisibilityEnabledPatch =
  genericSearchVisibility patchInternal Enabled
  where
    featurePath = "searchVisibility"
    patchInternal domain team status =
      I.patchTeamFeatureStatus domain team featurePath status

testSearchVisibilityEnabledPut :: HasCallStack => App ()
testSearchVisibilityEnabledPut =
  genericSearchVisibility putInternal Enabled
  where
    featurePath = "searchVisibility"
    putInternal domain team status = do
      let st = WithStatusNoLock status () 0
      I.putTeamFeatureStatus domain team featurePath st

checkSimpleFlag ::
  HasCallStack =>
  String ->
  String ->
  FeatureStatus ->
  App ()
checkSimpleFlag path name defStatus = do
  let domain = OwnDomain
      opposite = oppositeStatus defStatus
      cfg = ()
  (owner, team, mem : _) <- createTeam domain 2
  do
    nonMem <- randomUser domain def
    getTeamFeature path nonMem team `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 403
      resp.json %. "label" `shouldMatch` "no-team-member"
  do
    let status = defStatus
    assertFeature cfg status path owner team
    assertFeatureInternal cfg status path domain team
    assertFeatureFromAll path mem status
  do
    let expStatus =
          Aeson.object
            [ "status" .= opposite,
              "lockStatus" .= "unlocked",
              "ttl" .= "unlimited"
            ]
    withWebSocket owner $ \ws -> do
      I.setTeamFeatureStatus domain team path opposite
      n <- awaitMatch isFeatureConfigUpdateNotif ws
      n %. "payload.0.name" `shouldMatch` name
      n %. "payload.0.data" `shouldMatch` expStatus
  do
    let status = opposite
    assertFeature cfg status path owner team
    assertFeatureInternal cfg status path domain team
    assertFeatureFromAll path mem status

checkSimpleFlagWithLockStatus ::
  HasCallStack =>
  String ->
  String ->
  FeatureStatus ->
  String ->
  App ()
checkSimpleFlagWithLockStatus path name featStatus lockStatus = do
  let domain = OwnDomain
      cfg = ()
  (owner, team, mem : _) <- createTeam domain 2
  let assertFlag st lock = do
        assertFeatureLock lock cfg st path owner team
        assertFeatureInternalLock lock cfg st path domain team
        assertFeatureFromAllLock (Just lock) path mem st
  do
    nonMem <- randomUser domain def
    getTeamFeature path nonMem team `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 403
      resp.json %. "label" `shouldMatch` "no-team-member"
  assertFlag featStatus lockStatus
  -- unlock feature if it is locked
  when (lockStatus == "locked") . void $
    I.setTeamFeatureLockStatusInternal domain team path "unlocked" >>= getBody 200

  let opposite = oppositeStatus featStatus
  withWebSocket mem $ \ws -> do
    let st = WithStatusNoLock opposite () 0
    void $ setTeamFeature path owner team st >>= getBody 200
    let expStatus =
          Aeson.object
            [ "status" .= opposite,
              "lockStatus" .= "unlocked",
              "ttl" .= "unlimited"
            ]
    n <- awaitMatch isFeatureConfigUpdateNotif ws
    n %. "payload.0.name" `shouldMatch` name
    n %. "payload.0.data" `shouldMatch` expStatus

  assertFlag opposite "unlocked"

testFileSharing :: HasCallStack => App ()
testFileSharing =
  checkSimpleFlagWithLockStatus "fileSharing" "fileSharing" Enabled "unlocked"

testDigitalSignatures :: HasCallStack => App ()
testDigitalSignatures = do
  checkSimpleFlag "digitalSignatures" "digitalSignatures" Disabled

testValidateSAMLEmails :: HasCallStack => App ()
testValidateSAMLEmails = do
  checkSimpleFlag "validateSAMLemails" "validateSAMLemails" Enabled
