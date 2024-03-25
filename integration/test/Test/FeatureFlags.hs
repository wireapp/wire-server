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
import Control.Concurrent
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as Vector
import Notifications
import SetupHelpers
import Testlib.HTTP
import Testlib.Prelude

--------------------------------------------------------------------------------
-- Helpers

expectedWithStatus ::
  (ToJSON cfg, HasCallStack) =>
  WithStatusNoLock cfg ->
  LockStatus ->
  Aeson.Value
expectedWithStatus WithStatusNoLock {..} lock =
  let cfg = toJSON config
   in Aeson.object $
        [ "lockStatus" .= lock,
          "status" .= show status,
          "ttl" .= ttl
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
  LockStatus ->
  WithStatusNoLock cfg ->
  String ->
  user ->
  team ->
  App ()
assertFeatureLock lock ws featureName user team = do
  tf <- getTeamFeature featureName user team
  assertSuccessMatchBody (expectedWithStatus ws lock) tf

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
assertFeature = assertFeatureLock LockStatusUnlocked

assertFeatureFromAll ::
  (HasCallStack, MakesValue user) =>
  FeatureStatus ->
  String ->
  user ->
  App ()
assertFeatureFromAll = assertFeatureFromAllLock Nothing Nothing

assertFeatureFromAllLock ::
  (HasCallStack, MakesValue user) =>
  Maybe LockStatus ->
  Maybe Value ->
  FeatureStatus ->
  String ->
  user ->
  App ()
assertFeatureFromAllLock mLock meConfig eStatus featureName user = do
  actual <- extractTeamFeatureFromAllPersonal featureName user
  actual %. "status" `shouldMatch` show eStatus
  for_ meConfig (actual %. "config" `shouldMatch`)
  for_ mLock (actual %. "lockStatus" `shouldMatch`)

assertFeatureInternalLock ::
  (HasCallStack, MakesValue domain, ToJSON cfg) =>
  LockStatus ->
  WithStatusNoLock cfg ->
  String ->
  domain ->
  String ->
  App ()
assertFeatureInternalLock lock ws featureName dom team = do
  tf <- I.getTeamFeature dom featureName team
  assertSuccessMatchBody (expectedWithStatus ws lock) tf

assertFeatureInternal ::
  (HasCallStack, MakesValue domain, ToJSON cfg) =>
  WithStatusNoLock cfg ->
  String ->
  domain ->
  String ->
  App ()
assertFeatureInternal = assertFeatureInternalLock LockStatusUnlocked

assertFlagForbidden :: HasCallStack => App Response -> App ()
assertFlagForbidden res =
  res `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "no-team-member"

assertFlagNoConfigWithLockStatus ::
  HasCallStack =>
  App Response ->
  FeatureStatus ->
  LockStatus ->
  App ()
assertFlagNoConfigWithLockStatus res expectedStatus expectedLockStatus =
  let ws =
        WithStatus
          expectedStatus
          expectedLockStatus
          TrivialConfig
          FeatureTTLUnlimited
   in res `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json `shouldMatch` ws

--------------------------------------------------------------------------------

testLimitedEventFanout :: HasCallStack => App ()
testLimitedEventFanout = do
  let featureName = "limitedEventFanout"
      cfg = ()
      ws s = WithStatusNoLock s cfg FeatureTTLUnlimited
  (_alice, team, _) <- createTeam OwnDomain 1
  assertFeatureInternal (ws Disabled) featureName OwnDomain team
  I.setTeamFeatureStatus OwnDomain team featureName Enabled
  assertFeatureInternal (ws Enabled) featureName OwnDomain team

testSSOPut :: HasCallStack => FeatureStatus -> App ()
testSSOPut = genericTestSSO putInternal
  where
    putInternal domain team status = do
      let st = WithStatusNoLock status () FeatureTTLUnlimited
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
    ws s = WithStatusNoLock s cfg FeatureTTLUnlimited

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
    ws = WithStatusNoLock Disabled cfg FeatureTTLUnlimited

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
    ws = WithStatusNoLock Enabled cfg FeatureTTLUnlimited

testLegalholdDisabledByDefaultPut :: HasCallStack => App ()
testLegalholdDisabledByDefaultPut = legalholdDisabledByDefault putInternal
  where
    putInternal domain team status = do
      let st = WithStatusNoLock status () FeatureTTLUnlimited
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
      let st = WithStatusNoLock status () FeatureTTLUnlimited
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
      let st = WithStatusNoLock status () FeatureTTLUnlimited
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
    ws s = WithStatusNoLock s cfg FeatureTTLUnlimited

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
      let st = WithStatusNoLock status () FeatureTTLUnlimited
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
      let st = WithStatusNoLock status () FeatureTTLUnlimited
      I.putTeamFeatureStatus domain team featurePath st

checkSimpleFlag ::
  HasCallStack =>
  String ->
  String ->
  FeatureStatus ->
  App ()
checkSimpleFlag = checkSimpleFlagTTL FeatureTTLUnlimited

checkSimpleFlagTTL ::
  HasCallStack =>
  FeatureTTL ->
  String ->
  String ->
  FeatureStatus ->
  App ()
checkSimpleFlagTTL ttl path name defStatus = do
  (_owner, team, [mem]) <- createTeam domain 2
  let assertUnlimited :: HasCallStack => App ()
      assertUnlimited =
        I.getTeamFeature domain name team `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 200
          val <- resp.json %. "ttl"
          case fromJSON @FeatureTTL val of
            Error e -> assertFailure $ "Could not parse a TTL value: " <> show e
            Success (FeatureTTLSeconds v) ->
              let msg = "The obtained TTL is limited, but expected an unlimited TTL: " <> show v
               in assertFailure msg
            Success FeatureTTLUnlimited -> pure ()

      assertLimited :: HasCallStack => Word -> App ()
      assertLimited upperTTL = do
        I.getTeamFeature domain name team `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 200
          val <- resp.json %. "ttl"
          case fromJSON @FeatureTTL val of
            Error e -> assertFailure $ "Could not parse a TTL value: " <> show e
            Success FeatureTTLUnlimited ->
              let msg = "The obtained TTL is unlimited, but expected a limited TTL: " <> show upperTTL
               in assertFailure msg
            Success (FeatureTTLSeconds v) ->
              let msg =
                    unlines
                      [ "The obtained TTL of ",
                        show v,
                        " s is above the expected value of ",
                        show upperTTL,
                        " s"
                      ]
               in assertBool msg (v <= upperTTL)
        liftIO $ threadDelay (fromIntegral upperTTL * 1000000)
        assertUnlimited

  do
    nonMem <- randomUser domain def
    assertFlagForbidden $ getTeamFeature path nonMem team
  do
    let status = defStatus
    assertFeature (withStatus status) path mem team
    assertFeatureInternal (withStatus status) path domain team
    assertFeatureFromAll status path mem
  do
    let expStatus =
          Aeson.object
            [ "status" .= opposite,
              "lockStatus" .= LockStatusUnlocked,
              "ttl" .= ttl
            ]
    withWebSocket mem $ \ws -> do
      let st = WithStatusNoLock opposite cfg ttl
       in I.putTeamFeatureStatus domain team name st
      n <- awaitMatch isFeatureConfigUpdateNotif ws
      n %. "payload.0.name" `shouldMatch` name
      n %. "payload.0.data" `shouldMatch` expStatus
  do
    let status = opposite
    assertFeature (WithStatusNoLock status cfg ttl) path mem team
    assertFeatureInternal (WithStatusNoLock status cfg ttl) path domain team
    assertFeatureFromAll status path mem

  case ttl of
    FeatureTTLSeconds d -> do
      -- should revert back after TTL expires
      assertLimited d
      assertFeature (withStatus defStatus) path mem team
    FeatureTTLUnlimited -> do
      -- TTL should be NULL inside cassandra
      assertUnlimited

  -- Clean up
  let st = WithStatusNoLock defStatus cfg FeatureTTLUnlimited
   in I.putTeamFeatureStatus domain team name st
  assertFeature (withStatus defStatus) path mem team
  where
    domain = OwnDomain
    opposite = oppositeStatus defStatus
    cfg = ()
    withStatus s = WithStatusNoLock s cfg FeatureTTLUnlimited

checkSimpleFlagWithLockStatus ::
  HasCallStack =>
  String ->
  String ->
  FeatureStatus ->
  LockStatus ->
  App ()
checkSimpleFlagWithLockStatus path name featStatus lockStatus = do
  let domain = OwnDomain
      cfg = ()
      withStatus s = WithStatusNoLock s cfg FeatureTTLUnlimited
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
  when (lockStatus == LockStatusLocked) $
    I.setTeamFeatureLockStatus domain team path LockStatusUnlocked >>= assertStatus 200

  let opposite = oppositeStatus featStatus
  withWebSocket mem $ \ws -> do
    let st = WithStatusNoLock opposite () FeatureTTLUnlimited
    setTeamFeature path owner team st >>= assertStatus 200
    let expStatus =
          Aeson.object
            [ "status" .= opposite,
              "lockStatus" .= LockStatusUnlocked,
              "ttl" .= FeatureTTLUnlimited
            ]
    n <- awaitMatch isFeatureConfigUpdateNotif ws
    n %. "payload.0.name" `shouldMatch` name
    n %. "payload.0.data" `shouldMatch` expStatus

  assertFlag opposite LockStatusUnlocked

testFileSharing :: HasCallStack => App ()
testFileSharing =
  checkSimpleFlagWithLockStatus "fileSharing" "fileSharing" Enabled LockStatusUnlocked

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
    ws s = WithStatusNoLock s config FeatureTTLUnlimited
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
                      "config" .= config
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

-- | Call 'GET /teams/:tid/features' and 'GET /feature-configs', and check if all
-- features are there.
testAllFeatures :: HasCallStack => App ()
testAllFeatures = do
  let domain = OwnDomain
  (_owner, team, mem : _) <- createTeam domain 2
  defLockStatus <-
    readServiceConfig Galley
      & (%. "settings.featureFlags.selfDeletingMessages.defaults.lockStatus")
      & asText
  getAllTeamFeatures mem team `bindResponse` \resp -> do
    bdy <- getJSON 200 resp
    bdy `shouldMatch` expected Enabled defLockStatus {- determined by default in galley -}

  -- This block catches potential errors in the logic that reverts to default if there is a distinction made between
  -- 1. there is no row for a team_id in galley.team_features
  -- 2. there is a row for team_id in galley.team_features but the feature has a no entry (null value)
  let st = WithStatusNoLock Enabled () FeatureTTLUnlimited
   in -- this sets the guest links config to its default value thereby creating a row for the team in galley.team_features
      I.putTeamFeatureStatus domain team "conversationGuestLinks" st
  getAllTeamFeatures mem team `bindResponse` \resp -> do
    bdy <- getJSON 200 resp
    bdy `shouldMatch` expected Enabled defLockStatus {- determined by default in galley -}
  getAllFeatureConfigsPersonal mem `bindResponse` \resp -> do
    bdy <- getJSON 200 resp
    bdy `shouldMatch` expected Enabled defLockStatus {- determined by default in galley -}
  nonMember <- randomUser domain def
  getAllFeatureConfigsPersonal nonMember `bindResponse` \resp -> do
    bdy <- getJSON 200 resp
    bdy `shouldMatch` expected Enabled defLockStatus {- determined by 'getAfcConferenceCallingDefNew' in brig -}
  where
    expected confCalling lockStateSelfDeleting =
      Aeson.object $
        [ "appLock"
            .= ( Aeson.object
                   [ "config"
                       .= Aeson.object
                         [ "enforceAppLock" .= False,
                           "inactivityTimeoutSecs" .= Aeson.Number 60
                         ],
                     "lockStatus" .= LockStatusUnlocked,
                     "status" .= Enabled,
                     "ttl" .= FeatureTTLUnlimited
                   ]
               ),
          "classifiedDomains"
            .= ( Aeson.object
                   [ "config" .= (ClassifiedDomainsConfig [T.pack "example.com"]),
                     "lockStatus" .= LockStatusUnlocked,
                     "status" .= Enabled,
                     "ttl" .= FeatureTTLUnlimited
                   ]
               ),
          "conferenceCalling"
            .= ( Aeson.object
                   [ "lockStatus" .= LockStatusUnlocked,
                     "status" .= confCalling,
                     "ttl" .= FeatureTTLUnlimited
                   ]
               ),
          "conversationGuestLinks"
            .= ( Aeson.object
                   [ "lockStatus" .= LockStatusUnlocked,
                     "status" .= Enabled,
                     "ttl" .= FeatureTTLUnlimited
                   ]
               ),
          "digitalSignatures"
            .= ( Aeson.object
                   [ "lockStatus" .= LockStatusUnlocked,
                     "status" .= Disabled,
                     "ttl" .= FeatureTTLUnlimited
                   ]
               ),
          "enforceFileDownloadLocation"
            .= ( Aeson.object
                   [ "config" .= Aeson.object [],
                     "lockStatus" .= LockStatusLocked,
                     "status" .= Disabled,
                     "ttl" .= FeatureTTLUnlimited
                   ]
               ),
          "exposeInvitationURLsToTeamAdmin"
            .= ( Aeson.object
                   [ "lockStatus" .= LockStatusLocked,
                     "status" .= Disabled,
                     "ttl" .= FeatureTTLUnlimited
                   ]
               ),
          "fileSharing"
            .= ( Aeson.object
                   [ "lockStatus" .= LockStatusUnlocked,
                     "status" .= Enabled,
                     "ttl" .= FeatureTTLUnlimited
                   ]
               ),
          "legalhold"
            .= ( Aeson.object
                   [ "lockStatus" .= LockStatusUnlocked,
                     "status" .= Disabled,
                     "ttl" .= FeatureTTLUnlimited
                   ]
               ),
          "limitedEventFanout"
            .= ( Aeson.object
                   [ "lockStatus" .= LockStatusUnlocked,
                     "status" .= Disabled,
                     "ttl" .= FeatureTTLUnlimited
                   ]
               ),
          "mls"
            .= ( Aeson.object
                   [ "config"
                       .= Aeson.object
                         [ "allowedCipherSuites" .= Aeson.Array (Vector.fromList [Aeson.Number 1]),
                           "defaultCipherSuite" .= Aeson.Number 1,
                           "defaultProtocol" .= "proteus",
                           "protocolToggleUsers" .= Aeson.Array Vector.empty,
                           "supportedProtocols"
                             .= Aeson.Array (Aeson.String . T.pack <$> Vector.fromList ["proteus", "mls"])
                         ],
                     "lockStatus" .= LockStatusUnlocked,
                     "status" .= Disabled,
                     "ttl" .= FeatureTTLUnlimited
                   ]
               ),
          "mlsE2EId"
            .= ( Aeson.object
                   [ "config" .= Aeson.object ["verificationExpiration" .= Aeson.Number 86400],
                     "lockStatus" .= LockStatusUnlocked,
                     "status" .= Disabled,
                     "ttl" .= FeatureTTLUnlimited
                   ]
               ),
          "mlsMigration"
            .= ( Aeson.object
                   [ "config"
                       .= Aeson.object
                         [ "finaliseRegardlessAfter" .= "2029-10-17T00:00:00Z",
                           "startTime" .= "2029-05-16T10:11:12.123Z"
                         ],
                     "lockStatus" .= LockStatusLocked,
                     "status" .= Enabled,
                     "ttl" .= FeatureTTLUnlimited
                   ]
               ),
          "outlookCalIntegration"
            .= ( Aeson.object
                   [ "lockStatus" .= LockStatusLocked,
                     "status" .= Disabled,
                     "ttl" .= FeatureTTLUnlimited
                   ]
               ),
          "searchVisibility"
            .= ( Aeson.object
                   [ "lockStatus" .= LockStatusUnlocked,
                     "status" .= Disabled,
                     "ttl" .= FeatureTTLUnlimited
                   ]
               ),
          "searchVisibilityInbound"
            .= ( Aeson.object
                   [ "lockStatus" .= LockStatusUnlocked,
                     "status" .= Disabled,
                     "ttl" .= FeatureTTLUnlimited
                   ]
               ),
          "selfDeletingMessages"
            .= ( Aeson.object
                   [ "config" .= Aeson.object ["enforcedTimeoutSeconds" .= Aeson.Number 0],
                     "lockStatus" .= lockStateSelfDeleting,
                     "status" .= Enabled,
                     "ttl" .= FeatureTTLUnlimited
                   ]
               ),
          "sndFactorPasswordChallenge"
            .= ( Aeson.object
                   [ "lockStatus" .= LockStatusLocked,
                     "status" .= Disabled,
                     "ttl" .= FeatureTTLUnlimited
                   ]
               ),
          "sso"
            .= ( Aeson.object
                   [ "lockStatus" .= LockStatusUnlocked,
                     "status" .= Disabled,
                     "ttl" .= FeatureTTLUnlimited
                   ]
               ),
          "validateSAMLemails"
            .= ( Aeson.object
                   [ "lockStatus" .= LockStatusUnlocked,
                     "status" .= Enabled,
                     "ttl" .= FeatureTTLUnlimited
                   ]
               )
        ]

testFeatureConfigConsistency :: HasCallStack => App ()
testFeatureConfigConsistency = do
  let domain = OwnDomain
  (_owner, team, mem : _) <- createTeam domain 2
  allFeaturesRes <-
    getAllFeatureConfigsPersonal mem `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      getJSON 200 resp >>= parseObjectKeys
  allTeamFeaturesRes <-
    getAllTeamFeatures mem team `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      getJSON 200 resp >>= parseObjectKeys
  unless (allTeamFeaturesRes `Set.isSubsetOf` allFeaturesRes) $
    assertFailure (show allTeamFeaturesRes <> " is not a subset of " <> show allFeaturesRes)
  where
    parseObjectKeys = \case
      Aeson.Object hm -> pure . Set.fromList . map Aeson.toText . Aeson.keys $ hm
      x -> assertFailure ("JSON was not an object, but " <> show x)

testConferenceCalling :: HasCallStack => App ()
testConferenceCalling = do
  checkSimpleFlag "conferenceCalling" "conferenceCalling" Enabled

testSelfDeletingMessages :: HasCallStack => App ()
testSelfDeletingMessages = do
  defLockStatus <- do
    v <-
      readServiceConfig Galley
        & (%. "settings.featureFlags.selfDeletingMessages.defaults.lockStatus")
    case fromJSON v of
      Error e ->
        assertFailure $ "The lock status of self deleting messages cannot be parsed: " <> show e
      Success r -> pure r
  do
    nonMem <- randomUser domain def
    result <- extractTeamFeatureFromAllPersonal featureName nonMem
    result `shouldMatch` settingWithLockStatus Enabled 0 defLockStatus

  (owner, team, []) <- createTeam domain 1
  -- test that the default lock status comes from `galley.yaml`.
  -- use this to change `galley.integration.yaml` locally and manually test that conf file
  -- parsing works as expected.
  checkGet owner team Enabled 0 defLockStatus

  case defLockStatus of
    LockStatusLocked -> do
      checkSet team Disabled 0 409
    LockStatusUnlocked -> do
      checkSet team Disabled 0 200
      checkGet owner team Disabled 0 LockStatusUnlocked
      checkSet team Enabled 0 200
      checkGet owner team Enabled 0 LockStatusUnlocked

  -- now don't worry about what's in the config, write something to cassandra, and test with that.
  checkSetLockStatus team LockStatusLocked
  checkGet owner team Enabled 0 LockStatusLocked
  checkSet team Disabled 0 409
  checkGet owner team Enabled 0 LockStatusLocked
  checkSet team Enabled 30 409
  checkGet owner team Enabled 0 LockStatusLocked
  checkSetLockStatus team LockStatusUnlocked
  checkGet owner team Enabled 0 LockStatusUnlocked
  checkSet team Disabled 0 200
  checkGet owner team Disabled 0 LockStatusUnlocked
  checkSet team Enabled 30 200
  checkGet owner team Enabled 30 LockStatusUnlocked
  checkSet team Disabled 30 200
  checkGet owner team Disabled 30 LockStatusUnlocked
  checkSetLockStatus team LockStatusLocked
  checkGet owner team Enabled 0 LockStatusLocked
  checkSet team Enabled 50 409
  checkSetLockStatus team LockStatusUnlocked
  checkGet owner team Disabled 30 LockStatusUnlocked
  where
    domain = OwnDomain
    featureName = "selfDeletingMessages"
    -- personal users
    settingWithoutLockStatus ::
      HasCallStack =>
      FeatureStatus ->
      Int32 ->
      WithStatusNoLock SelfDeletingMessagesConfig
    settingWithoutLockStatus stat tout =
      WithStatusNoLock
        stat
        (SelfDeletingMessagesConfig tout)
        FeatureTTLUnlimited

    settingWithLockStatus ::
      HasCallStack =>
      FeatureStatus ->
      Int32 ->
      LockStatus ->
      WithStatus SelfDeletingMessagesConfig
    settingWithLockStatus stat tout lockStatus =
      WithStatus
        stat
        lockStatus
        (SelfDeletingMessagesConfig tout)
        FeatureTTLUnlimited

    -- internal, public (/team/:tid/features), and team-agnostic (/feature-configs).
    checkGet ::
      (HasCallStack, MakesValue user, MakesValue team) =>
      user ->
      team ->
      FeatureStatus ->
      Int32 ->
      LockStatus ->
      App ()
    checkGet user team stat tout lockStatus = do
      let expected = toJSON $ settingWithLockStatus stat tout lockStatus
      teamStr <- asString team
      forM_
        [ I.getTeamFeature domain featureName teamStr,
          getTeamFeature featureName user team
        ]
        $ \r -> r `bindResponse` \resp -> resp.json `shouldMatch` expected
      result <- extractTeamFeatureFromAllPersonal featureName user
      result `shouldMatch` expected

    checkSet ::
      (HasCallStack, MakesValue team) =>
      team ->
      FeatureStatus ->
      Int32 ->
      Int ->
      App ()
    checkSet team stat tout expectedHttpCode = do
      let st = settingWithoutLockStatus stat tout
      I.putTeamFeatureStatusRaw domain team featureName st `bindResponse` \resp ->
        resp.status `shouldMatchInt` expectedHttpCode

    checkSetLockStatus :: (HasCallStack, MakesValue team) => team -> LockStatus -> App ()
    checkSetLockStatus team status =
      I.setTeamFeatureLockStatus domain team featureName status >>= assertStatus 200

genericGuestLinks ::
  HasCallStack =>
  Domain ->
  (Value -> String -> App Response) ->
  (Value -> String -> WithStatusNoLock GuestLinksConfig -> App Response) ->
  App ()
genericGuestLinks domain getStatus putStatus = do
  (owner, team, []) <- createTeam domain 1
  checkGet owner team Enabled LockStatusUnlocked
  checkSet owner team Disabled 200
  checkGet owner team Disabled LockStatusUnlocked
  checkSet owner team Enabled 200
  checkGet owner team Enabled LockStatusUnlocked
  checkSet owner team Disabled 200
  checkGet owner team Disabled LockStatusUnlocked
  -- when locks status is locked the team default feature status should be returned
  -- and the team feature status can not be changed
  checkSetLockStatusInternal team LockStatusLocked
  checkGet owner team Enabled LockStatusLocked
  checkSet owner team Disabled 409
  -- when lock status is unlocked again the previously set feature status is restored
  checkSetLockStatusInternal team LockStatusUnlocked
  checkGet owner team Disabled LockStatusUnlocked
  where
    featureName = "conversationGuestLinks"
    checkGet ::
      HasCallStack =>
      Value ->
      String ->
      FeatureStatus ->
      LockStatus ->
      App ()
    checkGet owner team status lock = do
      getStatus owner team `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json `shouldMatch` (WithStatus status lock GuestLinksConfig FeatureTTLUnlimited)

    checkSet :: HasCallStack => Value -> String -> FeatureStatus -> Int -> App ()
    checkSet owner team status httpStatusCode =
      void $
        putStatus owner team (WithStatusNoLock status GuestLinksConfig FeatureTTLUnlimited)
          >>= getBody httpStatusCode

    checkSetLockStatusInternal :: HasCallStack => String -> LockStatus -> App ()
    checkSetLockStatusInternal team lockStatus =
      I.setTeamFeatureLockStatus domain team featureName lockStatus >>= assertStatus 200

testGuestLinksInternal :: HasCallStack => App ()
testGuestLinksInternal =
  genericGuestLinks
    domain
    (\_user team -> I.getTeamFeature domain featureName team)
    (\_user team ws -> I.putTeamFeatureStatusRaw domain team featureName ws)
  where
    domain = OwnDomain
    featureName = "conversationGuestLinks"

testGuestLinksPublic :: HasCallStack => App ()
testGuestLinksPublic =
  genericGuestLinks
    domain
    (getTeamFeature featureName)
    (setTeamFeature featureName)
  where
    domain = OwnDomain
    featureName = "conversationGuestLinks"

genericSimpleFlagWithLockStatus ::
  HasCallStack =>
  String ->
  FeatureStatus ->
  LockStatus ->
  App ()
genericSimpleFlagWithLockStatus featureName defaultStatus defaultLockStatus = do
  (owner, team, [mem]) <- createTeam domain 2
  nonMem <- randomUser domain def
  assertFlagForbidden $ getTeamFeature featureName nonMem team

  let getFlag :: HasCallStack => FeatureStatus -> LockStatus -> App ()
      getFlag expectedStatus expectedLockStatus = do
        let flag = getTeamFeature featureName mem team
        assertFlagNoConfigWithLockStatus flag expectedStatus expectedLockStatus

      getFlagInternal :: HasCallStack => FeatureStatus -> LockStatus -> App ()
      getFlagInternal expectedStatus expectedLockStatus = do
        let flag = I.getTeamFeature domain featureName team
        assertFlagNoConfigWithLockStatus flag expectedStatus expectedLockStatus

      getFeatureConfig :: HasCallStack => FeatureStatus -> LockStatus -> App ()
      getFeatureConfig expectedStatus expectedLockStatus = do
        actual <- extractTeamFeatureFromAllPersonal featureName mem
        actual %. "status" `shouldMatch` expectedStatus
        actual %. "lockStatus" `shouldMatch` expectedLockStatus

      getFlags :: HasCallStack => FeatureStatus -> LockStatus -> App ()
      getFlags expectedStatus expectedLockStatus = do
        getFlag expectedStatus expectedLockStatus
        getFeatureConfig expectedStatus expectedLockStatus
        getFlagInternal expectedStatus expectedLockStatus

      setLockStatus :: HasCallStack => LockStatus -> App ()
      setLockStatus lockStatus =
        I.setTeamFeatureLockStatus domain team featureName lockStatus
          >>= assertStatus 200

      setFlag :: HasCallStack => FeatureStatus -> App ()
      setFlag status =
        let st = (WithStatusNoLock status TrivialConfig FeatureTTLUnlimited)
         in setTeamFeature featureName owner team st >>= assertStatus 200

      assertSetStatusForbidden :: HasCallStack => FeatureStatus -> App ()
      assertSetStatusForbidden status =
        let ws = WithStatusNoLock status TrivialConfig FeatureTTLUnlimited
         in setTeamFeature featureName owner team ws >>= assertStatus 409

  let opposite = oppositeStatus defaultStatus

  -- Initial status and lock status should be the defaults
  getFlags defaultStatus defaultLockStatus

  -- unlock feature if it is locked
  when (defaultLockStatus == LockStatusLocked) $ setLockStatus LockStatusUnlocked

  -- setting should work. The member should receive an event
  withWebSocket mem $ \ws -> do
    setFlag opposite
    let expected =
          WithStatus opposite LockStatusUnlocked TrivialConfig FeatureTTLUnlimited
    n <- awaitMatch isFeatureConfigUpdateNotif ws
    n %. "payload.0.name" `shouldMatch` featureName
    n %. "payload.0.data" `shouldMatch` expected

  getFlags opposite LockStatusUnlocked

  -- lock feature
  setLockStatus LockStatusLocked
  -- feature status should now be the default again
  getFlags defaultStatus LockStatusLocked
  assertSetStatusForbidden defaultStatus
  -- unlock feature
  setLockStatus LockStatusUnlocked
  -- feature status should be the previously set value
  getFlags opposite LockStatusUnlocked

  -- clean up
  setFlag defaultStatus
  setLockStatus defaultLockStatus
  getFlags defaultStatus defaultLockStatus
  where
    domain = OwnDomain

testGuestLinksLockStatus :: HasCallStack => App ()
testGuestLinksLockStatus =
  genericSimpleFlagWithLockStatus "conversationGuestLinks" Enabled LockStatusUnlocked

testSndFactorPasswordChallenge :: HasCallStack => App ()
testSndFactorPasswordChallenge =
  genericSimpleFlagWithLockStatus "sndFactorPasswordChallenge" Disabled LockStatusLocked

testOutlookCalIntegration :: HasCallStack => App ()
testOutlookCalIntegration =
  genericSimpleFlagWithLockStatus "outlookCalIntegration" Disabled LockStatusLocked

testSearchVisibilityInbound :: HasCallStack => App ()
testSearchVisibilityInbound =
  checkSimpleFlag "searchVisibility" "searchVisibility" Disabled

testFeatureNoConfigMultiSearchVisibilityInbound :: HasCallStack => App ()
testFeatureNoConfigMultiSearchVisibilityInbound = do
  (_owner1, team1, []) <- createTeam domain 1
  (_owner2, team2, []) <- createTeam domain 1
  t1 <- make team1
  t2 <- make team2

  let ws = WithStatusNoLock Enabled TrivialConfig FeatureTTLUnlimited
   in I.putTeamFeatureStatus domain team2 featureName ws

  I.TeamFeatureNoConfigMultiResponse statuses <-
    I.getFeatureStatusMulti domain featureName (I.TeamFeatureNoConfigMultiRequest [t1, t2])
      `bindResponse` \resp -> do
        bdy <- getJSON 200 resp
        case fromJSON @I.TeamFeatureNoConfigMultiResponse bdy of
          Error e -> assertFailure $ "Could not parse response from an internal endpoint: " <> show e
          Success v -> pure v

  (length statuses) `shouldMatchInt` 2

  I.TeamStatus _ team1Status <- assertOne (filter ((== t1) . I.tsTeam) statuses)
  team1Status `shouldMatch` Disabled

  I.TeamStatus _ team2Status <- assertOne (filter ((== t2) . I.tsTeam) statuses)
  team2Status `shouldMatch` Enabled
  where
    domain = OwnDomain
    featureName = "searchVisibilityInbound"
