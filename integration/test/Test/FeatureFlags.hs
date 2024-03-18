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
import qualified Data.Text as T
import qualified Data.Vector as Vector
import Notifications
import SetupHelpers
import Testlib.HTTP
import Testlib.Prelude

--------------------------------------------------------------------------------
-- Helpers

expectedStatus ::
  (ToJSON cfg, HasCallStack) =>
  WithStatusNoLock cfg ->
  String ->
  Aeson.Value
expectedStatus WithStatusNoLock {..} lock =
  let cfg = toJSON config
   in Aeson.object $
        [ "lockStatus" .= lock,
          "status" .= show status,
          "ttl" .= if ttl == 0 then "unlimited" else show ttl
        ]
          <> ( guard (cfg /= Aeson.Array Vector.empty)
                 $> "config"
                 .= cfg
             )

assertFeatureLock ::
  ( HasCallStack,
    MakesValue user,
    MakesValue team,
    ToJSON cfg
  ) =>
  String ->
  WithStatusNoLock cfg ->
  String ->
  user ->
  team ->
  App ()
assertFeatureLock lock ws featureName user team = do
  tf <- getTeamFeature featureName user team
  assertSuccessMatchBody (expectedStatus ws lock) tf

assertFeature ::
  ( HasCallStack,
    MakesValue user,
    MakesValue team,
    ToJSON cfg
  ) =>
  WithStatusNoLock cfg ->
  String ->
  user ->
  team ->
  App ()
assertFeature = assertFeatureLock "unlocked"

assertFeatureFromAll ::
  (HasCallStack, MakesValue user) =>
  FeatureStatus ->
  String ->
  user ->
  App ()
assertFeatureFromAll = assertFeatureFromAllLock Nothing Nothing

assertFeatureFromAllLock ::
  (HasCallStack, MakesValue user) =>
  Maybe String ->
  Maybe Value ->
  FeatureStatus ->
  String ->
  user ->
  App ()
assertFeatureFromAllLock mLock meConfig eStatus featureName user = do
  actual <- extractTeamFeatureFromAll featureName user
  actual %. "status" `shouldMatch` show eStatus
  for_ meConfig (actual %. "config" `shouldMatch`)
  for_ mLock (actual %. "lockStatus" `shouldMatch`)

assertFeatureInternalLock ::
  (HasCallStack, MakesValue domain, ToJSON cfg) =>
  String ->
  WithStatusNoLock cfg ->
  String ->
  domain ->
  String ->
  App ()
assertFeatureInternalLock lock ws featureName dom team = do
  tf <- I.getTeamFeature dom featureName team
  assertSuccessMatchBody (expectedStatus ws lock) tf

assertFeatureInternal ::
  (HasCallStack, MakesValue domain, ToJSON cfg) =>
  WithStatusNoLock cfg ->
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
      ws s = WithStatusNoLock s cfg 0
  (_alice, team, _) <- createTeam OwnDomain 1
  assertFeatureInternal (ws Disabled) featureName OwnDomain team
  I.setTeamFeatureStatus OwnDomain team featureName Enabled
  assertFeatureInternal (ws Enabled) featureName OwnDomain team

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

  assertFeature (ws status) featureName alex team
  assertFeatureInternal (ws status) featureName domain team
  assertFeatureFromAll status featureName alex

  when (status == Disabled) $ do
    let opposite = Enabled
    setter domain team opposite
    assertFeature (ws opposite) featureName alex team
    assertFeatureInternal (ws opposite) featureName domain team
    assertFeatureFromAll opposite featureName alex
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
    ws s = WithStatusNoLock s cfg 0

legalholdAssertions ::
  (HasCallStack, MakesValue user) =>
  String ->
  String ->
  user ->
  App ()
legalholdAssertions domain team mem = do
  assertFeature ws featureName mem team
  assertFeatureInternal ws featureName domain team
  assertFeatureFromAll Disabled featureName mem
  nonMem <- randomUser domain def
  getTeamFeature featureName nonMem team `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "no-team-member"
  where
    featureName = "legalhold"
    cfg = ()
    ws = WithStatusNoLock Disabled cfg 0

legalholdDisabledByDefault ::
  HasCallStack =>
  (String -> String -> FeatureStatus -> App ()) ->
  App ()
legalholdDisabledByDefault setter = withModifiedBackend cnf $ \domain -> do
  (_alice, team, alex : _) <- createTeam domain 2
  legalholdAssertions domain team alex
  setter domain team Enabled
  assertFeature ws featureName alex team
  assertFeatureInternal ws featureName domain team
  assertFeatureFromAll Enabled featureName alex
  where
    cnf =
      def
        { galleyCfg = \conf ->
            conf
              & setField setting ("disabled-by-default")
        }
    setting = "settings.featureFlags." <> featureName
    featureName = "legalhold"
    cfg = ()
    ws = WithStatusNoLock Enabled cfg 0

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

  assertFeature (ws status) featurePath alice team
  assertFeatureInternal (ws status) featurePath domain team
  assertFeatureFromAll status featurePath alex

  let opposite = oppositeStatus status
  setter domain team opposite
  assertFeature (ws opposite) featurePath alice team
  assertFeatureInternal (ws opposite) featurePath domain team
  assertFeatureFromAll opposite featurePath alex

  setter domain team status
  assertFeature (ws status) featurePath alice team
  assertFeatureInternal (ws status) featurePath domain team
  assertFeatureFromAll status featurePath alex
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
    ws s = WithStatusNoLock s cfg 0

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
      withStatus s = WithStatusNoLock s cfg 0
  (owner, team, mem : _) <- createTeam domain 2
  do
    nonMem <- randomUser domain def
    getTeamFeature path nonMem team `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 403
      resp.json %. "label" `shouldMatch` "no-team-member"
  do
    let status = defStatus
    assertFeature (withStatus status) path owner team
    assertFeatureInternal (withStatus status) path domain team
    assertFeatureFromAll status path mem
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
    assertFeature (withStatus status) path owner team
    assertFeatureInternal (withStatus status) path domain team
    assertFeatureFromAll status path mem

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
      withStatus s = WithStatusNoLock s cfg 0
  (owner, team, mem : _) <- createTeam domain 2
  let assertFlag st lock = do
        assertFeatureLock lock (withStatus st) path owner team
        assertFeatureInternalLock lock (withStatus st) path domain team
        assertFeatureFromAllLock (Just lock) Nothing st path mem
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

genericClassifiedDomains ::
  HasCallStack =>
  FeatureStatus ->
  ClassifiedDomainsConfig ->
  App ()
genericClassifiedDomains status config = withModifiedBackend cnf $ \domain -> do
  (_owner, team, mem : _) <- createTeam domain 2

  getClassifiedDomains mem team status
  getClassifiedDomainsInternal mem team status
  getClassifiedDomainsFeatureConfig mem status
  where
    ws s = WithStatusNoLock s config 0
    featureName = "classifiedDomains"
    setting = "settings.featureFlags." <> featureName
    cnf =
      def
        { galleyCfg = \conf ->
            conf
              & setField
                setting
                ( object
                    [ "status" .= show status,
                      "config" .= toJSON config
                    ]
                )
        }
    getClassifiedDomains u t s = assertFeature (ws s) featureName u t
    getClassifiedDomainsInternal u t s = assertFeatureInternal (ws s) featureName u t
    getClassifiedDomainsFeatureConfig mem s =
      assertFeatureFromAllLock Nothing (Just . toJSON $ config) s featureName mem

testClassifiedDomainsEnabled :: HasCallStack => App ()
testClassifiedDomainsEnabled =
  genericClassifiedDomains
    Enabled
    (ClassifiedDomainsConfig [T.pack "example.com"])

testClassifiedDomainsDisabled :: HasCallStack => App ()
testClassifiedDomainsDisabled =
  genericClassifiedDomains
    Disabled
    (ClassifiedDomainsConfig [])
