-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module API.Teams.Feature (tests) where

import API.Util (HasGalley, getFeatureStatusMulti, withSettingsOverrides)
import qualified API.Util as Util
import API.Util.TeamFeature (patchFeatureStatusInternal)
import qualified API.Util.TeamFeature as Util
import Bilge
import Bilge.Assert
import Brig.Types.Test.Arbitrary (Arbitrary (arbitrary))
import Cassandra as Cql
import Control.Lens (over, to, view)
import Control.Monad.Catch (MonadCatch)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString.Char8 (unpack)
import Data.Domain (Domain (..))
import Data.Id
import Data.List1 (list1)
import qualified Data.List1 as List1
import Data.Schema (ToSchema)
import qualified Data.Set as Set
import Data.Timeout (TimeoutUnit (Second), (#))
import GHC.TypeLits (KnownSymbol)
import Galley.Options (optSettings, setFeatureFlags)
import Galley.Types.Teams
import Imports
import Network.Wai.Utilities (label)
import Test.Hspec (expectationFailure)
import Test.QuickCheck (generate)
import Test.Tasty
import qualified Test.Tasty.Cannon as WS
import Test.Tasty.HUnit (assertFailure, (@?=))
import TestHelpers (test)
import TestSetup
import Wire.API.Conversation.Protocol (ProtocolTag (ProtocolMLSTag, ProtocolProteusTag))
import qualified Wire.API.Event.FeatureConfig as FeatureConfig
import Wire.API.Internal.Notification (Notification)
import Wire.API.MLS.CipherSuite
import Wire.API.Routes.Internal.Galley.TeamFeatureNoConfigMulti as Multi
import Wire.API.Team.Feature (FeatureStatus (..), FeatureTTL (..), LockStatus (LockStatusUnlocked), MLSConfig (MLSConfig))
import qualified Wire.API.Team.Feature as Public
import Wire.API.Team.Role

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "Feature Config API and Team Features API"
    [ test s "SSO" testSSO,
      test s "LegalHold" testLegalHold,
      test s "SearchVisibility" testSearchVisibility,
      test s "DigitalSignatures" $ testSimpleFlag @Public.DigitalSignaturesConfig Public.FeatureStatusDisabled,
      test s "ValidateSAMLEmails" $ testSimpleFlag @Public.ValidateSAMLEmailsConfig Public.FeatureStatusEnabled,
      test s "FileSharing with lock status" $ testSimpleFlagWithLockStatus @Public.FileSharingConfig Public.FeatureStatusEnabled Public.LockStatusUnlocked,
      test s "Classified Domains (enabled)" testClassifiedDomainsEnabled,
      test s "Classified Domains (disabled)" testClassifiedDomainsDisabled,
      test s "All features" testAllFeatures,
      test s "Feature Configs / Team Features Consistency" testFeatureConfigConsistency,
      test s "ConferenceCalling" $ testSimpleFlag @Public.ConferenceCallingConfig Public.FeatureStatusEnabled,
      test s "SelfDeletingMessages" testSelfDeletingMessages,
      test s "ConversationGuestLinks - public API" testGuestLinksPublic,
      test s "ConversationGuestLinks - internal API" testGuestLinksInternal,
      test s "ConversationGuestLinks - lock status" $ testSimpleFlagWithLockStatus @Public.GuestLinksConfig Public.FeatureStatusEnabled Public.LockStatusUnlocked,
      test s "SndFactorPasswordChallenge - lock status" $ testSimpleFlagWithLockStatus @Public.SndFactorPasswordChallengeConfig Public.FeatureStatusDisabled Public.LockStatusLocked,
      test s "SearchVisibilityInbound - internal API" testSearchVisibilityInbound,
      test s "SearchVisibilityInbound - internal multi team API" testFeatureNoConfigMultiSearchVisibilityInbound,
      testGroup
        "Conference calling"
        [ test s "ConferenceCalling unlimited TTL" $ testSimpleFlagTTL @Public.ConferenceCallingConfig Public.FeatureStatusEnabled FeatureTTLUnlimited,
          test s "ConferenceCalling 2s TTL" $ testSimpleFlagTTL @Public.ConferenceCallingConfig Public.FeatureStatusEnabled (FeatureTTLSeconds 2)
        ],
      testGroup
        "Overrides"
        [ test s "increase to unlimited" $ testSimpleFlagTTLOverride @Public.ConferenceCallingConfig Public.FeatureStatusEnabled (FeatureTTLSeconds 2) FeatureTTLUnlimited,
          test s "increase" $ testSimpleFlagTTLOverride @Public.ConferenceCallingConfig Public.FeatureStatusEnabled (FeatureTTLSeconds 2) (FeatureTTLSeconds 4),
          test s "reduce from unlimited" $ testSimpleFlagTTLOverride @Public.ConferenceCallingConfig Public.FeatureStatusEnabled FeatureTTLUnlimited (FeatureTTLSeconds 2),
          test s "reduce" $ testSimpleFlagTTLOverride @Public.ConferenceCallingConfig Public.FeatureStatusEnabled (FeatureTTLSeconds 5) (FeatureTTLSeconds 2),
          test s "Unlimited to unlimited" $ testSimpleFlagTTLOverride @Public.ConferenceCallingConfig Public.FeatureStatusEnabled FeatureTTLUnlimited FeatureTTLUnlimited
        ],
      test s "MLS feature config" testMLS,
      test s "SearchVisibilityInbound" $ testSimpleFlag @Public.SearchVisibilityInboundConfig Public.FeatureStatusDisabled,
      testGroup
        "Patch"
        [ test s (unpack $ Public.featureNameBS @Public.FileSharingConfig) $
            testPatch @Public.FileSharingConfig Public.FeatureStatusEnabled Public.FileSharingConfig,
          test s (unpack $ Public.featureNameBS @Public.GuestLinksConfig) $
            testPatch @Public.GuestLinksConfig Public.FeatureStatusEnabled Public.GuestLinksConfig,
          test s (unpack $ Public.featureNameBS @Public.SndFactorPasswordChallengeConfig) $
            testPatch @Public.SndFactorPasswordChallengeConfig Public.FeatureStatusDisabled Public.SndFactorPasswordChallengeConfig,
          test s (unpack $ Public.featureNameBS @Public.SelfDeletingMessagesConfig) $
            testPatch @Public.SelfDeletingMessagesConfig Public.FeatureStatusEnabled (Public.SelfDeletingMessagesConfig 0)
        ]
    ]

testPatch ::
  forall cfg.
  ( HasCallStack,
    Public.IsFeatureConfig cfg,
    Typeable cfg,
    ToSchema cfg,
    Eq cfg,
    Show cfg,
    KnownSymbol (Public.FeatureSymbol cfg),
    Arbitrary (Public.WithStatus cfg),
    Arbitrary (Public.WithStatusPatch cfg)
  ) =>
  Public.FeatureStatus ->
  cfg ->
  TestM ()
testPatch defStatus defConfig = do
  (_, tid) <- Util.createBindingTeam
  Just original <- responseJsonMaybe <$> Util.getFeatureStatusInternal @cfg tid
  rndFeatureConfig :: Public.WithStatusPatch cfg <- liftIO (generate arbitrary)
  patchFeatureStatusInternal tid rndFeatureConfig !!! statusCode === const 200
  Just actual <- responseJsonMaybe <$> Util.getFeatureStatusInternal @cfg tid
  liftIO $
    if Public.wsLockStatus actual == Public.LockStatusLocked
      then do
        Public.wsStatus actual @?= defStatus
        Public.wsConfig actual @?= defConfig
      else do
        Public.wsStatus actual @?= fromMaybe (Public.wsStatus original) (Public.wspStatus rndFeatureConfig)
        Public.wsLockStatus actual @?= fromMaybe (Public.wsLockStatus original) (Public.wspLockStatus rndFeatureConfig)
        Public.wsConfig actual @?= fromMaybe (Public.wsConfig original) (Public.wspConfig rndFeatureConfig)

testSSO :: TestM ()
testSSO = do
  owner <- Util.randomUser
  member <- Util.randomUser
  nonMember <- Util.randomUser
  tid <- Util.createNonBindingTeam "foo" owner []
  Util.connectUsers owner (list1 member [])
  Util.addTeamMember owner tid member (rolePermissions RoleMember) Nothing

  let getSSO :: HasCallStack => Public.FeatureStatus -> TestM ()
      getSSO = assertFlagNoConfig @Public.SSOConfig $ Util.getTeamFeatureFlag @Public.SSOConfig member tid
      getSSOFeatureConfig :: HasCallStack => Public.FeatureStatus -> TestM ()
      getSSOFeatureConfig expectedStatus = do
        actual <- Util.getFeatureConfig @Public.SSOConfig member
        liftIO $ Public.wsStatus actual @?= expectedStatus
      getSSOInternal :: HasCallStack => Public.FeatureStatus -> TestM ()
      getSSOInternal = assertFlagNoConfig @Public.SSOConfig $ Util.getTeamFeatureFlagInternal @Public.SSOConfig tid
      setSSOInternal :: HasCallStack => Public.FeatureStatus -> TestM ()
      setSSOInternal = void . Util.putTeamFeatureFlagInternal @Public.SSOConfig expect2xx tid . (`Public.WithStatusNoLock` Public.SSOConfig)

  assertFlagForbidden $ Util.getTeamFeatureFlag @Public.SSOConfig nonMember tid

  featureSSO <- view (tsGConf . optSettings . setFeatureFlags . flagSSO)
  case featureSSO of
    FeatureSSODisabledByDefault -> do
      -- Test default
      getSSO Public.FeatureStatusDisabled
      getSSOInternal Public.FeatureStatusDisabled
      getSSOFeatureConfig Public.FeatureStatusDisabled

      -- Test override
      setSSOInternal Public.FeatureStatusEnabled
      getSSO Public.FeatureStatusEnabled
      getSSOInternal Public.FeatureStatusEnabled
      getSSOFeatureConfig Public.FeatureStatusEnabled
    FeatureSSOEnabledByDefault -> do
      -- since we don't allow to disable (see 'disableSsoNotImplemented'), we can't test
      -- much here.  (disable failure is covered in "enable/disable SSO" above.)
      getSSO Public.FeatureStatusEnabled
      getSSOInternal Public.FeatureStatusEnabled
      getSSOFeatureConfig Public.FeatureStatusEnabled

testLegalHold :: TestM ()
testLegalHold = do
  owner <- Util.randomUser
  member <- Util.randomUser
  nonMember <- Util.randomUser
  tid <- Util.createNonBindingTeam "foo" owner []
  Util.connectUsers owner (list1 member [])
  Util.addTeamMember owner tid member (rolePermissions RoleMember) Nothing

  let getLegalHold :: HasCallStack => Public.FeatureStatus -> TestM ()
      getLegalHold = assertFlagNoConfig @Public.LegalholdConfig $ Util.getTeamFeatureFlag @Public.LegalholdConfig member tid
      getLegalHoldInternal :: HasCallStack => Public.FeatureStatus -> TestM ()
      getLegalHoldInternal = assertFlagNoConfig @Public.LegalholdConfig $ Util.getTeamFeatureFlagInternal @Public.LegalholdConfig tid
      getLegalHoldFeatureConfig expectedStatus = do
        actual <- Util.getFeatureConfig @Public.LegalholdConfig member
        liftIO $ Public.wsStatus actual @?= expectedStatus

      setLegalHoldInternal :: HasCallStack => Public.FeatureStatus -> TestM ()
      setLegalHoldInternal = void . Util.putTeamFeatureFlagInternal @Public.LegalholdConfig expect2xx tid . (`Public.WithStatusNoLock` Public.LegalholdConfig)
  getLegalHold Public.FeatureStatusDisabled
  getLegalHoldInternal Public.FeatureStatusDisabled

  assertFlagForbidden $ Util.getTeamFeatureFlag @Public.LegalholdConfig nonMember tid

  -- FUTUREWORK: run two galleys, like below for custom search visibility.
  featureLegalHold <- view (tsGConf . optSettings . setFeatureFlags . flagLegalHold)
  case featureLegalHold of
    FeatureLegalHoldDisabledByDefault -> do
      -- Test default
      getLegalHold Public.FeatureStatusDisabled
      getLegalHoldInternal Public.FeatureStatusDisabled
      getLegalHoldFeatureConfig Public.FeatureStatusDisabled

      -- Test override
      setLegalHoldInternal Public.FeatureStatusEnabled
      getLegalHold Public.FeatureStatusEnabled
      getLegalHoldInternal Public.FeatureStatusEnabled
      getLegalHoldFeatureConfig Public.FeatureStatusEnabled

    -- turned off for instance
    FeatureLegalHoldDisabledPermanently -> do
      Util.putLegalHoldEnabledInternal' expect4xx tid Public.FeatureStatusEnabled

    -- turned off but for whitelisted teams with implicit consent
    FeatureLegalHoldWhitelistTeamsAndImplicitConsent -> do
      Util.putLegalHoldEnabledInternal' expect4xx tid Public.FeatureStatusEnabled

testSearchVisibility :: TestM ()
testSearchVisibility = do
  owner <- Util.randomUser
  member <- Util.randomUser
  nonMember <- Util.randomUser
  tid <- Util.createNonBindingTeam "foo" owner []
  Util.connectUsers owner (list1 member [])
  Util.addTeamMember owner tid member (rolePermissions RoleMember) Nothing

  let getTeamSearchVisibility :: TeamId -> Public.FeatureStatus -> TestM ()
      getTeamSearchVisibility teamid expected = do
        g <- view tsGalley
        Util.getTeamSearchVisibilityAvailable g owner teamid !!! do
          statusCode === const 200
          responseJsonEither === const (Right (Public.WithStatusNoLock expected Public.SearchVisibilityAvailableConfig))

  let getTeamSearchVisibilityInternal :: TeamId -> Public.FeatureStatus -> TestM ()
      getTeamSearchVisibilityInternal teamid expected = do
        g <- view tsGalley
        Util.getTeamSearchVisibilityAvailableInternal g teamid !!! do
          statusCode === const 200
          responseJsonEither === const (Right (Public.WithStatusNoLock expected Public.SearchVisibilityAvailableConfig))

  let getTeamSearchVisibilityFeatureConfig :: UserId -> Public.FeatureStatus -> TestM ()
      getTeamSearchVisibilityFeatureConfig uid expected = do
        actual <- Util.getFeatureConfig @Public.SearchVisibilityAvailableConfig uid
        liftIO $ Public.wsStatus actual @?= expected

  let setTeamSearchVisibilityInternal :: TeamId -> Public.FeatureStatus -> TestM ()
      setTeamSearchVisibilityInternal teamid val = do
        g <- view tsGalley
        Util.putTeamSearchVisibilityAvailableInternal g teamid val

  assertFlagForbidden $ Util.getTeamFeatureFlag @Public.SearchVisibilityAvailableConfig nonMember tid

  tid2 <- Util.createNonBindingTeam "foo" owner []
  team2member <- Util.randomUser
  Util.connectUsers owner (list1 team2member [])
  Util.addTeamMember owner tid2 team2member (rolePermissions RoleMember) Nothing

  Util.withCustomSearchFeature FeatureTeamSearchVisibilityUnavailableByDefault $ do
    getTeamSearchVisibility tid2 Public.FeatureStatusDisabled
    getTeamSearchVisibilityInternal tid2 Public.FeatureStatusDisabled
    getTeamSearchVisibilityFeatureConfig team2member Public.FeatureStatusDisabled

    setTeamSearchVisibilityInternal tid2 Public.FeatureStatusEnabled
    getTeamSearchVisibility tid2 Public.FeatureStatusEnabled
    getTeamSearchVisibilityInternal tid2 Public.FeatureStatusEnabled
    getTeamSearchVisibilityFeatureConfig team2member Public.FeatureStatusEnabled

    setTeamSearchVisibilityInternal tid2 Public.FeatureStatusDisabled
    getTeamSearchVisibility tid2 Public.FeatureStatusDisabled
    getTeamSearchVisibilityInternal tid2 Public.FeatureStatusDisabled
    getTeamSearchVisibilityFeatureConfig team2member Public.FeatureStatusDisabled

  tid3 <- Util.createNonBindingTeam "foo" owner []
  team3member <- Util.randomUser
  Util.connectUsers owner (list1 team3member [])
  Util.addTeamMember owner tid3 team3member (rolePermissions RoleMember) Nothing

  Util.withCustomSearchFeature FeatureTeamSearchVisibilityAvailableByDefault $ do
    getTeamSearchVisibility tid3 Public.FeatureStatusEnabled
    getTeamSearchVisibilityInternal tid3 Public.FeatureStatusEnabled
    getTeamSearchVisibilityFeatureConfig team3member Public.FeatureStatusEnabled

    setTeamSearchVisibilityInternal tid3 Public.FeatureStatusDisabled
    getTeamSearchVisibility tid3 Public.FeatureStatusDisabled
    getTeamSearchVisibilityInternal tid3 Public.FeatureStatusDisabled
    getTeamSearchVisibilityFeatureConfig team3member Public.FeatureStatusDisabled

    setTeamSearchVisibilityInternal tid3 Public.FeatureStatusEnabled
    getTeamSearchVisibility tid3 Public.FeatureStatusEnabled
    getTeamSearchVisibilityInternal tid3 Public.FeatureStatusEnabled
    getTeamSearchVisibilityFeatureConfig team3member Public.FeatureStatusEnabled

getClassifiedDomains ::
  (HasCallStack, HasGalley m, MonadIO m, MonadHttp m, MonadCatch m) =>
  UserId ->
  TeamId ->
  Public.WithStatusNoLock Public.ClassifiedDomainsConfig ->
  m ()
getClassifiedDomains member tid =
  assertFlagWithConfig @Public.ClassifiedDomainsConfig $
    Util.getTeamFeatureFlag @Public.ClassifiedDomainsConfig member tid

getClassifiedDomainsInternal ::
  (HasCallStack, HasGalley m, MonadIO m, MonadHttp m, MonadCatch m) =>
  TeamId ->
  Public.WithStatusNoLock Public.ClassifiedDomainsConfig ->
  m ()
getClassifiedDomainsInternal tid =
  assertFlagWithConfig @Public.ClassifiedDomainsConfig $
    Util.getTeamFeatureFlagInternal @Public.ClassifiedDomainsConfig tid

testClassifiedDomainsEnabled :: TestM ()
testClassifiedDomainsEnabled = do
  (_owner, tid, member : _) <- Util.createBindingTeamWithNMembers 1
  let expected =
        Public.WithStatusNoLock Public.FeatureStatusEnabled (Public.ClassifiedDomainsConfig [Domain "example.com"])

  let getClassifiedDomainsFeatureConfig ::
        (HasCallStack, HasGalley m, MonadIO m, MonadHttp m, MonadCatch m) =>
        UserId ->
        Public.WithStatusNoLock Public.ClassifiedDomainsConfig ->
        m ()
      getClassifiedDomainsFeatureConfig uid expected' = do
        result <- Util.getFeatureConfig @Public.ClassifiedDomainsConfig uid
        liftIO $ Public.wsStatus result @?= Public.wssStatus expected'
        liftIO $ Public.wsConfig result @?= Public.wssConfig expected'

  getClassifiedDomains member tid expected
  getClassifiedDomainsInternal tid expected
  getClassifiedDomainsFeatureConfig member expected

testClassifiedDomainsDisabled :: TestM ()
testClassifiedDomainsDisabled = do
  (_owner, tid, member : _) <- Util.createBindingTeamWithNMembers 1
  let expected =
        Public.WithStatusNoLock Public.FeatureStatusDisabled (Public.ClassifiedDomainsConfig [])

  let getClassifiedDomainsFeatureConfig ::
        (HasCallStack, HasGalley m, MonadIO m, MonadHttp m, MonadCatch m) =>
        UserId ->
        Public.WithStatusNoLock Public.ClassifiedDomainsConfig ->
        m ()
      getClassifiedDomainsFeatureConfig uid expected' = do
        result <- Util.getFeatureConfig @Public.ClassifiedDomainsConfig uid
        liftIO $ Public.wsStatus result @?= Public.wssStatus expected'
        liftIO $ Public.wsConfig result @?= Public.wssConfig expected'

  let classifiedDomainsDisabled = \opts ->
        opts
          & over
            (optSettings . setFeatureFlags . flagClassifiedDomains)
            (\(ImplicitLockStatus s) -> ImplicitLockStatus (s & Public.setStatus Public.FeatureStatusDisabled & Public.setConfig (Public.ClassifiedDomainsConfig [])))
  withSettingsOverrides classifiedDomainsDisabled $ do
    getClassifiedDomains member tid expected
    getClassifiedDomainsInternal tid expected
    getClassifiedDomainsFeatureConfig member expected

testSimpleFlag ::
  forall cfg.
  ( HasCallStack,
    Typeable cfg,
    Public.IsFeatureConfig cfg,
    KnownSymbol (Public.FeatureSymbol cfg),
    Public.FeatureTrivialConfig cfg,
    ToSchema cfg,
    FromJSON (Public.WithStatusNoLock cfg),
    ToJSON (Public.WithStatusNoLock cfg)
  ) =>
  Public.FeatureStatus ->
  TestM ()
testSimpleFlag defaultValue = testSimpleFlagTTL @cfg defaultValue FeatureTTLUnlimited

testSimpleFlagTTLOverride ::
  forall cfg.
  ( HasCallStack,
    Typeable cfg,
    Public.IsFeatureConfig cfg,
    KnownSymbol (Public.FeatureSymbol cfg),
    Public.FeatureTrivialConfig cfg,
    ToSchema cfg,
    FromJSON (Public.WithStatusNoLock cfg),
    ToJSON (Public.WithStatusNoLock cfg)
  ) =>
  Public.FeatureStatus ->
  FeatureTTL ->
  FeatureTTL ->
  TestM ()
testSimpleFlagTTLOverride defaultValue ttl ttlAfter = do
  owner <- Util.randomUser
  member <- Util.randomUser
  nonMember <- Util.randomUser
  tid <- Util.createNonBindingTeam "foo" owner []
  Util.connectUsers owner (list1 member [])
  Util.addTeamMember owner tid member (rolePermissions RoleMember) Nothing

  let getFlag :: HasCallStack => Public.FeatureStatus -> TestM ()
      getFlag expected =
        flip (assertFlagNoConfig @cfg) expected $ Util.getTeamFeatureFlag @cfg member tid

      getFeatureConfig :: HasCallStack => Public.FeatureStatus -> TestM ()
      getFeatureConfig expected = do
        actual <- Util.getFeatureConfig @cfg member
        liftIO $ Public.wsStatus actual @?= expected

      getFlagInternal :: HasCallStack => Public.FeatureStatus -> TestM ()
      getFlagInternal expected =
        flip (assertFlagNoConfig @cfg) expected $ Util.getTeamFeatureFlagInternal @cfg tid

      setFlagInternal :: Public.FeatureStatus -> FeatureTTL -> TestM ()
      setFlagInternal statusValue ttl' =
        void $ Util.putTeamFeatureFlagInternalTTL @cfg expect2xx tid (Public.WithStatusNoLock statusValue (Public.trivialConfig @cfg)) ttl'

      select :: PrepQuery R (Identity TeamId) (Identity (Maybe FeatureTTL))
      select = fromString "select ttl(conference_calling) from team_features where team_id = ?"

      assertUnlimited = do
        -- TTL should be NULL inside cassandra
        cassState <- view tsCass
        liftIO $ do
          storedTTL <- maybe Nothing runIdentity <$> Cql.runClient cassState (Cql.query1 select $ params LocalQuorum (Identity tid))
          storedTTL @?= Nothing

      toMicros secs = fromIntegral secs * 1000000

  assertFlagForbidden $ Util.getTeamFeatureFlag @cfg nonMember tid

  let otherValue = case defaultValue of
        Public.FeatureStatusDisabled -> Public.FeatureStatusEnabled
        Public.FeatureStatusEnabled -> Public.FeatureStatusDisabled

  -- Initial value should be the default value
  getFlag defaultValue
  getFlagInternal defaultValue
  getFeatureConfig defaultValue

  -- Setting should work
  setFlagInternal otherValue ttl
  getFlag otherValue
  getFeatureConfig otherValue
  getFlagInternal otherValue

  case (ttl, ttlAfter) of
    (FeatureTTLSeconds d, FeatureTTLSeconds d') -> do
      -- wait less than expiration, override and recheck.
      liftIO $ threadDelay (toMicros d `div` 2) -- waiting half of TTL
      setFlagInternal otherValue ttlAfter
      -- value is still correct
      getFlag otherValue

      liftIO $ threadDelay (toMicros d') -- waiting for new TTL
      getFlag defaultValue
      assertUnlimited -- TTL should be NULL after expiration.
    (FeatureTTLSeconds d, FeatureTTLUnlimited) -> do
      -- wait less than expiration, override and recheck.
      liftIO $ threadDelay (fromIntegral d `div` 2) -- waiting half of TTL
      setFlagInternal otherValue ttlAfter
      -- value is still correct
      getFlag otherValue
      assertUnlimited
    (FeatureTTLUnlimited, FeatureTTLUnlimited) -> do
      -- overriding in this case should have no effect.
      setFlagInternal otherValue ttl
      getFlag otherValue
      getFeatureConfig otherValue
      getFlagInternal otherValue

      assertUnlimited
    (FeatureTTLUnlimited, FeatureTTLSeconds d) -> do
      assertUnlimited

      setFlagInternal otherValue ttlAfter
      getFlag otherValue
      getFeatureConfig otherValue
      getFlagInternal otherValue

      liftIO $ threadDelay (toMicros d) -- waiting it out
      -- value reverts back
      getFlag defaultValue
      -- TTL should be NULL inside cassandra
      assertUnlimited

  -- Clean up
  setFlagInternal defaultValue FeatureTTLUnlimited
  getFlag defaultValue

testSimpleFlagTTL ::
  forall cfg.
  ( HasCallStack,
    Typeable cfg,
    Public.IsFeatureConfig cfg,
    KnownSymbol (Public.FeatureSymbol cfg),
    Public.FeatureTrivialConfig cfg,
    ToSchema cfg,
    FromJSON (Public.WithStatusNoLock cfg),
    ToJSON (Public.WithStatusNoLock cfg)
  ) =>
  Public.FeatureStatus ->
  FeatureTTL ->
  TestM ()
testSimpleFlagTTL defaultValue ttl = do
  owner <- Util.randomUser
  member <- Util.randomUser
  nonMember <- Util.randomUser
  tid <- Util.createNonBindingTeam "foo" owner []
  Util.connectUsers owner (list1 member [])
  Util.addTeamMember owner tid member (rolePermissions RoleMember) Nothing

  let getFlag :: HasCallStack => Public.FeatureStatus -> TestM ()
      getFlag expected =
        flip (assertFlagNoConfig @cfg) expected $ Util.getTeamFeatureFlag @cfg member tid

      getFeatureConfig :: HasCallStack => Public.FeatureStatus -> TestM ()
      getFeatureConfig expected = do
        actual <- Util.getFeatureConfig @cfg member
        liftIO $ Public.wsStatus actual @?= expected

      getFlagInternal :: HasCallStack => Public.FeatureStatus -> TestM ()
      getFlagInternal expected =
        flip (assertFlagNoConfig @cfg) expected $ Util.getTeamFeatureFlagInternal @cfg tid

      setFlagInternal :: Public.FeatureStatus -> FeatureTTL -> TestM ()
      setFlagInternal statusValue ttl' =
        void $ Util.putTeamFeatureFlagInternalTTL @cfg expect2xx tid (Public.WithStatusNoLock statusValue (Public.trivialConfig @cfg)) ttl'

      select :: PrepQuery R (Identity TeamId) (Identity (Maybe FeatureTTL))
      select = fromString "select ttl(conference_calling) from team_features where team_id = ?"

  assertFlagForbidden $ Util.getTeamFeatureFlag @cfg nonMember tid

  let otherValue = case defaultValue of
        Public.FeatureStatusDisabled -> Public.FeatureStatusEnabled
        Public.FeatureStatusEnabled -> Public.FeatureStatusDisabled

  -- Initial value should be the default value
  getFlag defaultValue
  getFlagInternal defaultValue
  getFeatureConfig defaultValue

  -- Setting should work
  cannon <- view tsCannon
  -- should receive an event
  WS.bracketR cannon member $ \ws -> do
    setFlagInternal otherValue ttl
    void . liftIO $
      WS.assertMatch (5 # Second) ws $
        wsAssertFeatureTrivialConfigUpdate @cfg otherValue
  getFlag otherValue
  getFeatureConfig otherValue
  getFlagInternal otherValue

  case ttl of
    FeatureTTLSeconds d -> do
      -- should revert back after TTL expires
      liftIO $ threadDelay (fromIntegral d * 1000000)
      getFlag defaultValue
    FeatureTTLUnlimited -> do
      -- TTL should be NULL inside cassandra
      cassState <- view tsCass
      liftIO $ do
        storedTTL <- Cql.runClient cassState $ Cql.query1 select $ params LocalQuorum (Identity tid)
        runIdentity <$> storedTTL @?= Just Nothing

  -- Clean up
  setFlagInternal defaultValue FeatureTTLUnlimited
  getFlag defaultValue

testSimpleFlagWithLockStatus ::
  forall cfg.
  ( HasCallStack,
    Typeable cfg,
    Eq cfg,
    Show cfg,
    Public.FeatureTrivialConfig cfg,
    Public.IsFeatureConfig cfg,
    KnownSymbol (Public.FeatureSymbol cfg),
    ToSchema cfg,
    FromJSON (Public.WithStatusNoLock cfg),
    ToJSON (Public.WithStatusNoLock cfg)
  ) =>
  Public.FeatureStatus ->
  Public.LockStatus ->
  TestM ()
testSimpleFlagWithLockStatus defaultStatus defaultLockStatus = do
  galley <- view tsGalley
  -- let feature = Public.knownTeamFeatureName @a
  owner <- Util.randomUser
  member <- Util.randomUser
  nonMember <- Util.randomUser
  tid <- Util.createNonBindingTeam "foo" owner []
  Util.connectUsers owner (list1 member [])
  Util.addTeamMember owner tid member (rolePermissions RoleMember) Nothing
  let getFlag :: HasCallStack => Public.FeatureStatus -> Public.LockStatus -> TestM ()
      getFlag expectedStatus expectedLockStatus = do
        let flag = Util.getTeamFeatureFlag @cfg member tid
        assertFlagNoConfigWithLockStatus @cfg flag expectedStatus expectedLockStatus

      getFeatureConfig :: HasCallStack => Public.FeatureStatus -> Public.LockStatus -> TestM ()
      getFeatureConfig expectedStatus expectedLockStatus = do
        actual <- Util.getFeatureConfig @cfg member
        liftIO $ Public.wsStatus actual @?= expectedStatus
        liftIO $ Public.wsLockStatus actual @?= expectedLockStatus

      getFlagInternal :: HasCallStack => Public.FeatureStatus -> Public.LockStatus -> TestM ()
      getFlagInternal expectedStatus expectedLockStatus = do
        let flag = Util.getTeamFeatureFlagInternal @cfg tid
        assertFlagNoConfigWithLockStatus @cfg flag expectedStatus expectedLockStatus

      getFlags expectedStatus expectedLockStatus = do
        getFlag expectedStatus expectedLockStatus
        getFeatureConfig expectedStatus expectedLockStatus
        getFlagInternal expectedStatus expectedLockStatus

      setFlagWithGalley :: Public.FeatureStatus -> TestM ()
      setFlagWithGalley statusValue =
        Util.putTeamFeatureFlagWithGalley @cfg galley owner tid (Public.WithStatusNoLock statusValue (Public.trivialConfig @cfg))
          !!! statusCode === const 200

      assertSetStatusForbidden :: Public.FeatureStatus -> TestM ()
      assertSetStatusForbidden statusValue =
        Util.putTeamFeatureFlagWithGalley @cfg galley owner tid (Public.WithStatusNoLock statusValue (Public.trivialConfig @cfg))
          !!! statusCode === const 409

      setLockStatus :: Public.LockStatus -> TestM ()
      setLockStatus lockStatus =
        Util.setLockStatusInternal @cfg galley tid lockStatus
          !!! statusCode === const 200

  assertFlagForbidden $ Util.getTeamFeatureFlag @cfg nonMember tid

  let otherStatus = case defaultStatus of
        Public.FeatureStatusDisabled -> Public.FeatureStatusEnabled
        Public.FeatureStatusEnabled -> Public.FeatureStatusDisabled

  -- Initial status and lock status should be the defaults
  getFlags defaultStatus defaultLockStatus

  -- unlock feature if it is locked
  when (defaultLockStatus == Public.LockStatusLocked) $ setLockStatus Public.LockStatusUnlocked

  -- setting should work
  cannon <- view tsCannon
  -- should receive an event
  WS.bracketR cannon member $ \ws -> do
    setFlagWithGalley otherStatus
    void . liftIO $
      WS.assertMatch (5 # Second) ws $
        wsAssertFeatureConfigWithLockStatusUpdate @cfg otherStatus Public.LockStatusUnlocked

  getFlags otherStatus Public.LockStatusUnlocked

  -- lock feature
  setLockStatus Public.LockStatusLocked
  -- feature status should now be the default again
  getFlags defaultStatus Public.LockStatusLocked
  assertSetStatusForbidden defaultStatus
  -- unlock feature
  setLockStatus Public.LockStatusUnlocked
  -- feature status should be the previously set value
  getFlags otherStatus Public.LockStatusUnlocked

  -- clean up
  setFlagWithGalley defaultStatus
  setLockStatus defaultLockStatus
  getFlags defaultStatus defaultLockStatus

testSelfDeletingMessages :: TestM ()
testSelfDeletingMessages = do
  defLockStatus :: Public.LockStatus <-
    view
      ( tsGConf
          . optSettings
          . setFeatureFlags
          . flagSelfDeletingMessages
          . unDefaults
          . to Public.wsLockStatus
      )

  -- personal users
  let settingWithoutLockStatus :: FeatureStatus -> Int32 -> Public.WithStatusNoLock Public.SelfDeletingMessagesConfig
      settingWithoutLockStatus stat tout =
        Public.WithStatusNoLock
          stat
          (Public.SelfDeletingMessagesConfig tout)
      settingWithLockStatus :: FeatureStatus -> Int32 -> Public.LockStatus -> Public.WithStatus Public.SelfDeletingMessagesConfig
      settingWithLockStatus stat tout lockStatus =
        Public.withStatus
          stat
          lockStatus
          (Public.SelfDeletingMessagesConfig tout)

  personalUser <- Util.randomUser
  do
    result <- Util.getFeatureConfig @Public.SelfDeletingMessagesConfig personalUser
    liftIO $ result @?= settingWithLockStatus FeatureStatusEnabled 0 defLockStatus

  -- team users
  galley <- view tsGalley
  (owner, tid, []) <- Util.createBindingTeamWithNMembers 0

  let checkSet :: FeatureStatus -> Int32 -> Int -> TestM ()
      checkSet stat tout expectedStatusCode =
        do
          Util.putTeamFeatureFlagInternal @Public.SelfDeletingMessagesConfig
            galley
            tid
            (settingWithoutLockStatus stat tout)
          !!! statusCode === const expectedStatusCode

      -- internal, public (/team/:tid/features), and team-agnostic (/feature-configs).
      checkGet :: HasCallStack => FeatureStatus -> Int32 -> Public.LockStatus -> TestM ()
      checkGet stat tout lockStatus = do
        let expected = settingWithLockStatus stat tout lockStatus
        forM_
          [ Util.getTeamFeatureFlagInternal @Public.SelfDeletingMessagesConfig tid,
            Util.getTeamFeatureFlagWithGalley @Public.SelfDeletingMessagesConfig galley owner tid
          ]
          (!!! responseJsonEither === const (Right expected))
        result <- Util.getFeatureConfig @Public.SelfDeletingMessagesConfig owner
        liftIO $ result @?= expected

      checkSetLockStatus :: HasCallStack => Public.LockStatus -> TestM ()
      checkSetLockStatus status =
        do
          Util.setLockStatusInternal @Public.SelfDeletingMessagesConfig galley tid status
          !!! statusCode === const 200

  -- test that the default lock status comes from `galley.yaml`.
  -- use this to change `galley.integration.yaml` locally and manually test that conf file
  -- parsing works as expected.
  checkGet FeatureStatusEnabled 0 defLockStatus

  case defLockStatus of
    Public.LockStatusLocked -> do
      checkSet FeatureStatusDisabled 0 409
    Public.LockStatusUnlocked -> do
      checkSet FeatureStatusDisabled 0 200
      checkGet FeatureStatusDisabled 0 Public.LockStatusUnlocked
      checkSet FeatureStatusEnabled 0 200
      checkGet FeatureStatusEnabled 0 Public.LockStatusUnlocked

  -- now don't worry about what's in the config, write something to cassandra, and test with that.
  checkSetLockStatus Public.LockStatusLocked
  checkGet FeatureStatusEnabled 0 Public.LockStatusLocked
  checkSet FeatureStatusDisabled 0 409
  checkGet FeatureStatusEnabled 0 Public.LockStatusLocked
  checkSet FeatureStatusEnabled 30 409
  checkGet FeatureStatusEnabled 0 Public.LockStatusLocked
  checkSetLockStatus Public.LockStatusUnlocked
  checkGet FeatureStatusEnabled 0 Public.LockStatusUnlocked
  checkSet FeatureStatusDisabled 0 200
  checkGet FeatureStatusDisabled 0 Public.LockStatusUnlocked
  checkSet FeatureStatusEnabled 30 200
  checkGet FeatureStatusEnabled 30 Public.LockStatusUnlocked
  checkSet FeatureStatusDisabled 30 200
  checkGet FeatureStatusDisabled 30 Public.LockStatusUnlocked
  checkSetLockStatus Public.LockStatusLocked
  checkGet FeatureStatusEnabled 0 Public.LockStatusLocked
  checkSet FeatureStatusEnabled 50 409
  checkSetLockStatus Public.LockStatusUnlocked
  checkGet FeatureStatusDisabled 30 Public.LockStatusUnlocked

testGuestLinksInternal :: TestM ()
testGuestLinksInternal = do
  galley <- view tsGalley
  testGuestLinks
    (const $ Util.getTeamFeatureFlagInternal @Public.GuestLinksConfig)
    (const $ Util.putTeamFeatureFlagInternal @Public.GuestLinksConfig galley)
    (Util.setLockStatusInternal @Public.GuestLinksConfig galley)

testGuestLinksPublic :: TestM ()
testGuestLinksPublic = do
  galley <- view tsGalley
  testGuestLinks
    (Util.getTeamFeatureFlagWithGalley @Public.GuestLinksConfig galley)
    (Util.putTeamFeatureFlagWithGalley @Public.GuestLinksConfig galley)
    (Util.setLockStatusInternal @Public.GuestLinksConfig galley)

testGuestLinks ::
  (UserId -> TeamId -> TestM ResponseLBS) ->
  (UserId -> TeamId -> Public.WithStatusNoLock Public.GuestLinksConfig -> TestM ResponseLBS) ->
  (TeamId -> Public.LockStatus -> TestM ResponseLBS) ->
  TestM ()
testGuestLinks getStatus putStatus setLockStatusInternal = do
  (owner, tid, []) <- Util.createBindingTeamWithNMembers 0
  let checkGet :: HasCallStack => Public.FeatureStatus -> Public.LockStatus -> TestM ()
      checkGet status lock =
        getStatus owner tid !!! do
          statusCode === const 200
          responseJsonEither === const (Right (Public.withStatus status lock Public.GuestLinksConfig))

      checkSet :: HasCallStack => Public.FeatureStatus -> Int -> TestM ()
      checkSet status expectedStatusCode =
        putStatus owner tid (Public.WithStatusNoLock status Public.GuestLinksConfig) !!! statusCode === const expectedStatusCode

      checkSetLockStatusInternal :: HasCallStack => Public.LockStatus -> TestM ()
      checkSetLockStatusInternal lockStatus =
        setLockStatusInternal tid lockStatus !!! statusCode === const 200

  checkGet Public.FeatureStatusEnabled Public.LockStatusUnlocked
  checkSet Public.FeatureStatusDisabled 200
  checkGet Public.FeatureStatusDisabled Public.LockStatusUnlocked
  checkSet Public.FeatureStatusEnabled 200
  checkGet Public.FeatureStatusEnabled Public.LockStatusUnlocked
  checkSet Public.FeatureStatusDisabled 200
  checkGet Public.FeatureStatusDisabled Public.LockStatusUnlocked
  -- when locks status is locked the team default feature status should be returned
  -- and the team feature status can not be changed
  checkSetLockStatusInternal Public.LockStatusLocked
  checkGet Public.FeatureStatusEnabled Public.LockStatusLocked
  checkSet Public.FeatureStatusDisabled 409
  -- when lock status is unlocked again the previously set feature status is restored
  checkSetLockStatusInternal Public.LockStatusUnlocked
  checkGet Public.FeatureStatusDisabled Public.LockStatusUnlocked

-- | Call 'GET /teams/:tid/features' and 'GET /feature-configs', and check if all
-- features are there.
testAllFeatures :: TestM ()
testAllFeatures = do
  defLockStatus :: Public.LockStatus <-
    view
      ( tsGConf
          . optSettings
          . setFeatureFlags
          . flagSelfDeletingMessages
          . unDefaults
          . to Public.wsLockStatus
      )

  (_owner, tid, member : _) <- Util.createBindingTeamWithNMembers 1
  Util.getAllTeamFeatures member tid !!! do
    statusCode === const 200
    responseJsonMaybe === const (Just (expected FeatureStatusEnabled defLockStatus {- determined by default in galley -}))

  -- This block catches potential errors in the logic that reverts to default if there is a disinction made between
  -- 1. there is no row for a team_id in galley.team_features
  -- 2. there is a row for team_id in galley.team_features but the feature has a no entry (null value)
  galley <- view tsGalley
  -- this sets the guest links config to its default value thereby creating a row for the team in galley.team_features
  Util.putTeamFeatureFlagInternal @Public.GuestLinksConfig galley tid (Public.WithStatusNoLock FeatureStatusEnabled Public.GuestLinksConfig)
    !!! statusCode === const 200
  Util.getAllTeamFeatures member tid !!! do
    statusCode === const 200
    responseJsonMaybe === const (Just (expected FeatureStatusEnabled defLockStatus {- determined by default in galley -}))

  Util.getAllTeamFeaturesPersonal member !!! do
    statusCode === const 200
    responseJsonMaybe === const (Just (expected FeatureStatusEnabled defLockStatus {- determined by default in galley -}))

  randomPersonalUser <- Util.randomUser
  Util.getAllTeamFeaturesPersonal randomPersonalUser !!! do
    statusCode === const 200
    responseJsonMaybe === const (Just (expected FeatureStatusEnabled defLockStatus {- determined by 'getAfcConferenceCallingDefNew' in brig -}))
  where
    expected confCalling lockStateSelfDeleting =
      Public.AllFeatureConfigs
        { Public.afcLegalholdStatus = Public.withStatus FeatureStatusDisabled Public.LockStatusUnlocked Public.LegalholdConfig,
          Public.afcSSOStatus = Public.withStatus FeatureStatusDisabled Public.LockStatusUnlocked Public.SSOConfig,
          Public.afcTeamSearchVisibilityAvailable = Public.withStatus FeatureStatusDisabled Public.LockStatusUnlocked Public.SearchVisibilityAvailableConfig,
          Public.afcValidateSAMLEmails = Public.withStatus FeatureStatusEnabled Public.LockStatusUnlocked Public.ValidateSAMLEmailsConfig,
          Public.afcDigitalSignatures = Public.withStatus FeatureStatusDisabled Public.LockStatusUnlocked Public.DigitalSignaturesConfig,
          Public.afcAppLock = Public.withStatus FeatureStatusEnabled Public.LockStatusUnlocked (Public.AppLockConfig (Public.EnforceAppLock False) (60 :: Int32)),
          Public.afcFileSharing = Public.withStatus FeatureStatusEnabled Public.LockStatusUnlocked Public.FileSharingConfig,
          Public.afcClassifiedDomains = Public.withStatus FeatureStatusEnabled Public.LockStatusUnlocked (Public.ClassifiedDomainsConfig [Domain "example.com"]),
          Public.afcConferenceCalling = Public.withStatus confCalling Public.LockStatusUnlocked Public.ConferenceCallingConfig,
          Public.afcSelfDeletingMessages = Public.withStatus FeatureStatusEnabled lockStateSelfDeleting (Public.SelfDeletingMessagesConfig 0),
          Public.afcGuestLink = Public.withStatus FeatureStatusEnabled Public.LockStatusUnlocked Public.GuestLinksConfig,
          Public.afcSndFactorPasswordChallenge = Public.withStatus FeatureStatusDisabled Public.LockStatusLocked Public.SndFactorPasswordChallengeConfig,
          Public.afcMLS = Public.withStatus FeatureStatusDisabled Public.LockStatusUnlocked (Public.MLSConfig [] ProtocolProteusTag [MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519] MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519),
          Public.afcSearchVisibilityInboundConfig = Public.withStatus FeatureStatusDisabled Public.LockStatusUnlocked Public.SearchVisibilityInboundConfig
        }

testFeatureConfigConsistency :: TestM ()
testFeatureConfigConsistency = do
  owner <- Util.randomUser
  member <- Util.randomUser
  tid <- Util.createNonBindingTeam "foo" owner []
  Util.connectUsers owner (list1 member [])
  Util.addTeamMember owner tid member (rolePermissions RoleMember) Nothing

  allFeaturesRes <- Util.getAllFeatureConfigs member >>= parseObjectKeys

  allTeamFeaturesRes <- Util.getAllTeamFeatures member tid >>= parseObjectKeys

  unless (allTeamFeaturesRes `Set.isSubsetOf` allFeaturesRes) $
    liftIO $ expectationFailure (show allTeamFeaturesRes <> " is not a subset of " <> show allFeaturesRes)
  where
    parseObjectKeys :: ResponseLBS -> TestM (Set.Set Text)
    parseObjectKeys res = do
      case responseJsonEither res of
        Left err -> liftIO $ assertFailure ("Did not parse as an object" <> err)
        Right (val :: Aeson.Value) ->
          case val of
            (Aeson.Object hm) -> pure (Set.fromList . map AesonKey.toText . KeyMap.keys $ hm)
            x -> liftIO $ assertFailure ("JSON was not an object, but " <> show x)

testSearchVisibilityInbound :: TestM ()
testSearchVisibilityInbound = do
  let defaultValue = FeatureStatusDisabled
  owner <- Util.randomUser
  tid <- Util.createNonBindingTeam "foo" owner []

  let getFlagInternal :: HasCallStack => Public.FeatureStatus -> TestM ()
      getFlagInternal expected =
        flip (assertFlagNoConfig @Public.SearchVisibilityInboundConfig) expected $ Util.getTeamFeatureFlagInternal @Public.SearchVisibilityInboundConfig tid

      setFlagInternal :: Public.FeatureStatus -> TestM ()
      setFlagInternal statusValue =
        void $ Util.putTeamFeatureFlagInternal @Public.SearchVisibilityInboundConfig expect2xx tid (Public.WithStatusNoLock statusValue Public.SearchVisibilityInboundConfig)

  let otherValue = case defaultValue of
        Public.FeatureStatusDisabled -> Public.FeatureStatusEnabled
        Public.FeatureStatusEnabled -> Public.FeatureStatusDisabled

  -- Initial value should be the default value
  getFlagInternal defaultValue
  setFlagInternal otherValue
  getFlagInternal otherValue

testFeatureNoConfigMultiSearchVisibilityInbound :: TestM ()
testFeatureNoConfigMultiSearchVisibilityInbound = do
  owner1 <- Util.randomUser
  team1 <- Util.createNonBindingTeam "team1" owner1 []

  owner2 <- Util.randomUser
  team2 <- Util.createNonBindingTeam "team2" owner2 []

  let setFlagInternal :: TeamId -> Public.FeatureStatus -> TestM ()
      setFlagInternal tid statusValue =
        void $ Util.putTeamFeatureFlagInternal @Public.SearchVisibilityInboundConfig expect2xx tid (Public.WithStatusNoLock statusValue Public.SearchVisibilityInboundConfig)

  setFlagInternal team2 Public.FeatureStatusEnabled

  r <-
    getFeatureStatusMulti @Public.SearchVisibilityInboundConfig (Multi.TeamFeatureNoConfigMultiRequest [team1, team2])
      <!! statusCode === const 200

  Multi.TeamFeatureNoConfigMultiResponse teamsStatuses :: Multi.TeamFeatureNoConfigMultiResponse Public.SearchVisibilityInboundConfig <- responseJsonError r

  liftIO $ do
    length teamsStatuses @?= 2

    Multi.TeamStatus _ team1Status <- Util.assertOne (filter ((== team1) . Multi.team) teamsStatuses)
    team1Status @?= Public.FeatureStatusDisabled

    Multi.TeamStatus _ team2Status <- Util.assertOne (filter ((== team2) . Multi.team) teamsStatuses)
    team2Status @?= Public.FeatureStatusEnabled

testMLS :: TestM ()
testMLS = do
  owner <- Util.randomUser
  member <- Util.randomUser
  tid <- Util.createNonBindingTeam "foo" owner []
  Util.connectUsers owner (list1 member [])
  Util.addTeamMember owner tid member (rolePermissions RoleMember) Nothing

  galley <- view tsGalley
  cannon <- view tsCannon

  let getForTeam :: HasCallStack => Public.WithStatusNoLock MLSConfig -> TestM ()
      getForTeam expected =
        flip assertFlagWithConfig expected $ Util.getTeamFeatureFlag @MLSConfig member tid

      getForTeamInternal :: HasCallStack => Public.WithStatusNoLock MLSConfig -> TestM ()
      getForTeamInternal expected =
        flip assertFlagWithConfig expected $ Util.getTeamFeatureFlagInternal @Public.MLSConfig tid

      getForUser :: HasCallStack => Public.WithStatusNoLock MLSConfig -> TestM ()
      getForUser expected = do
        result <- Util.getFeatureConfig @MLSConfig member
        liftIO $ Public.wsStatus result @?= Public.wssStatus expected
        liftIO $ Public.wsConfig result @?= Public.wssConfig expected

      getViaEndpoints :: HasCallStack => Public.WithStatusNoLock MLSConfig -> TestM ()
      getViaEndpoints expected = do
        getForTeam expected
        getForTeamInternal expected
        getForUser expected

      setForTeam :: HasCallStack => Public.WithStatusNoLock MLSConfig -> TestM ()
      setForTeam wsnl =
        Util.putTeamFeatureFlagWithGalley @MLSConfig galley owner tid wsnl
          !!! statusCode === const 200

      setForTeamInternal :: HasCallStack => Public.WithStatusNoLock MLSConfig -> TestM ()
      setForTeamInternal wsnl =
        void $ Util.putTeamFeatureFlagInternal @Public.MLSConfig expect2xx tid wsnl

  let cipherSuite = MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519
  let defaultConfig = Public.WithStatusNoLock FeatureStatusDisabled (MLSConfig [] ProtocolProteusTag [cipherSuite] cipherSuite)
  let config2 = Public.WithStatusNoLock FeatureStatusEnabled (MLSConfig [member] ProtocolMLSTag [] cipherSuite)
  let config3 = Public.WithStatusNoLock FeatureStatusDisabled (MLSConfig [] ProtocolMLSTag [cipherSuite] cipherSuite)

  getViaEndpoints defaultConfig

  WS.bracketR cannon member $ \ws -> do
    setForTeam config2
    void . liftIO $
      WS.assertMatch (5 # Second) ws $
        wsAssertFeatureConfigUpdate @MLSConfig config2 LockStatusUnlocked
  getViaEndpoints config2

  WS.bracketR cannon member $ \ws -> do
    setForTeamInternal config3
    void . liftIO $
      WS.assertMatch (5 # Second) ws $
        wsAssertFeatureConfigUpdate @MLSConfig config3 LockStatusUnlocked
  getViaEndpoints config3

assertFlagForbidden :: HasCallStack => TestM ResponseLBS -> TestM ()
assertFlagForbidden res = do
  res !!! do
    statusCode === const 403
    fmap label . responseJsonMaybe === const (Just "no-team-member")

assertFlagNoConfig ::
  forall cfg.
  ( HasCallStack,
    Typeable cfg,
    Public.IsFeatureConfig cfg,
    Public.FeatureTrivialConfig cfg,
    FromJSON (Public.WithStatusNoLock cfg)
  ) =>
  TestM ResponseLBS ->
  Public.FeatureStatus ->
  TestM ()
assertFlagNoConfig res expected = do
  res !!! do
    statusCode === const 200
    ( fmap Public.wssStatus
        . responseJsonEither @(Public.WithStatusNoLock cfg)
      )
      === const (Right expected)

assertFlagNoConfigWithLockStatus ::
  forall cfg.
  ( HasCallStack,
    Typeable cfg,
    Public.IsFeatureConfig cfg,
    Public.FeatureTrivialConfig cfg,
    FromJSON (Public.WithStatus cfg),
    Eq cfg,
    Show cfg
  ) =>
  TestM ResponseLBS ->
  Public.FeatureStatus ->
  Public.LockStatus ->
  TestM ()
assertFlagNoConfigWithLockStatus res expectedStatus expectedLockStatus = do
  res !!! do
    statusCode === const 200
    responseJsonEither @(Public.WithStatus cfg)
      === const (Right (Public.withStatus expectedStatus expectedLockStatus (Public.trivialConfig @cfg)))

assertFlagWithConfig ::
  forall cfg m.
  ( HasCallStack,
    Eq cfg,
    ToSchema cfg,
    Show cfg,
    Typeable cfg,
    Public.IsFeatureConfig cfg,
    MonadIO m,
    MonadCatch m
  ) =>
  m ResponseLBS ->
  Public.WithStatusNoLock cfg ->
  m ()
assertFlagWithConfig response expected = do
  r <- response
  let rJson = responseJsonEither @(Public.WithStatusNoLock cfg) r
  pure r !!! statusCode === const 200
  liftIO $ do
    fmap Public.wssStatus rJson @?= (Right . Public.wssStatus $ expected)
    fmap Public.wssConfig rJson @?= (Right . Public.wssConfig $ expected)

wsAssertFeatureTrivialConfigUpdate ::
  forall cfg.
  ( Public.IsFeatureConfig cfg,
    KnownSymbol (Public.FeatureSymbol cfg),
    ToJSON (Public.WithStatusNoLock cfg),
    Public.FeatureTrivialConfig cfg,
    ToSchema cfg
  ) =>
  Public.FeatureStatus ->
  Notification ->
  IO ()
wsAssertFeatureTrivialConfigUpdate status notification = do
  let e :: FeatureConfig.Event = List1.head (WS.unpackPayload notification)
  FeatureConfig._eventType e @?= FeatureConfig.Update
  FeatureConfig._eventFeatureName e @?= Public.featureName @cfg
  FeatureConfig._eventData e @?= Aeson.toJSON (Public.withStatus status (Public.wsLockStatus (Public.defFeatureStatus @cfg)) (Public.trivialConfig @cfg))

wsAssertFeatureConfigWithLockStatusUpdate ::
  forall cfg.
  ( Public.IsFeatureConfig cfg,
    ToSchema cfg,
    KnownSymbol (Public.FeatureSymbol cfg),
    ToJSON (Public.WithStatusNoLock cfg),
    Public.FeatureTrivialConfig cfg
  ) =>
  Public.FeatureStatus ->
  Public.LockStatus ->
  Notification ->
  IO ()
wsAssertFeatureConfigWithLockStatusUpdate status lockStatus notification = do
  let e :: FeatureConfig.Event = List1.head (WS.unpackPayload notification)
  FeatureConfig._eventType e @?= FeatureConfig.Update
  FeatureConfig._eventFeatureName e @?= (Public.featureName @cfg)
  FeatureConfig._eventData e @?= Aeson.toJSON (Public.withStatus status lockStatus (Public.trivialConfig @cfg))

wsAssertFeatureConfigUpdate ::
  forall cfg.
  ( Public.IsFeatureConfig cfg,
    KnownSymbol (Public.FeatureSymbol cfg),
    ToJSON (Public.WithStatus cfg),
    ToSchema cfg
  ) =>
  Public.WithStatusNoLock cfg ->
  Public.LockStatus ->
  Notification ->
  IO ()
wsAssertFeatureConfigUpdate config lockStatus notification = do
  let e :: FeatureConfig.Event = List1.head (WS.unpackPayload notification)
  FeatureConfig._eventType e @?= FeatureConfig.Update
  FeatureConfig._eventFeatureName e @?= Public.featureName @cfg
  FeatureConfig._eventData e @?= Aeson.toJSON (Public.withLockStatus lockStatus config)
