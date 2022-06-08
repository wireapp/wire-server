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
import qualified API.Util.TeamFeature as Util
import Bilge
import Bilge.Assert
import Cassandra as Cql
import Control.Lens (over, to, view)
import Control.Monad.Catch (MonadCatch)
import Data.Aeson (FromJSON, ToJSON, object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString.Conversion (toByteString')
import Data.Domain (Domain (..))
import Data.Id
import Data.List1 (list1)
import qualified Data.List1 as List1
import Data.Schema (ToSchema)
import qualified Data.Set as Set
import qualified Data.Text.Encoding as TE
import Data.Timeout (TimeoutUnit (Second), (#))
import Galley.Data.TeamFeatures (HasStatusCol (..))
import Galley.Options (optSettings, setFeatureFlags)
import Galley.Types.Teams
import Imports
import Network.Wai.Utilities (label)
import Test.Hspec (expectationFailure, shouldBe)
import Test.Tasty
import qualified Test.Tasty.Cannon as WS
import Test.Tasty.HUnit (assertBool, assertFailure, (@?=))
import TestHelpers (test)
import TestSetup
import Wire.API.Event.FeatureConfig (EventData (..))
import qualified Wire.API.Event.FeatureConfig as FeatureConfig
import Wire.API.Internal.Notification (Notification)
import Wire.API.Routes.Internal.Galley.TeamFeatureNoConfigMulti as Multi
import Wire.API.Team.Feature (TeamFeatureName (..), TeamFeatureStatusValue (..), TeamFeatureTTLValue (..))
import qualified Wire.API.Team.Feature as Public

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "Feature Config API and Team Features API"
    [ test s "SSO" testSSO,
      test s "LegalHold" testLegalHold,
      test s "SearchVisibility" testSearchVisibility,
      test s "DigitalSignatures" $ testSimpleFlag @'Public.TeamFeatureDigitalSignatures Public.TeamFeatureDisabled,
      test s "ValidateSAMLEmails" $ testSimpleFlag @'Public.TeamFeatureValidateSAMLEmails Public.TeamFeatureEnabled,
      test s "FileSharing with lock status" $ testSimpleFlagWithLockStatus @'Public.TeamFeatureFileSharing Public.TeamFeatureEnabled Public.Unlocked,
      test s "Classified Domains (enabled)" testClassifiedDomainsEnabled,
      test s "Classified Domains (disabled)" testClassifiedDomainsDisabled,
      test s "All features" testAllFeatures,
      test s "Feature Configs / Team Features Consistency" testFeatureConfigConsistency,
      test s "SelfDeletingMessages" testSelfDeletingMessages,
      test s "ConversationGuestLinks - public API" testGuestLinksPublic,
      test s "ConversationGuestLinks - internal API" testGuestLinksInternal,
      test s "ConversationGuestLinks - lock status" $ testSimpleFlagWithLockStatus @'Public.TeamFeatureGuestLinks Public.TeamFeatureEnabled Public.Unlocked,
      test s "SndFactorPasswordChallenge - lock status" $ testSimpleFlagWithLockStatus @'Public.TeamFeatureSndFactorPasswordChallenge Public.TeamFeatureDisabled Public.Locked,
      test s "SearchVisibilityInbound - internal API" testSearchVisibilityInbound,
      test s "SearchVisibilityInbound - internal multi team API" testFeatureNoConfigMultiSearchVisibilityInbound,
      testGroup
        "Conferece calling"
        [ test s "ConferenceCalling unlimited TTL" $ testSimpleFlagTTL @'Public.TeamFeatureConferenceCalling Public.TeamFeatureEnabled TeamFeatureTTLUnlimited,
          test s "ConferenceCalling 1s TTL" $ testSimpleFlagTTL @'Public.TeamFeatureConferenceCalling Public.TeamFeatureEnabled (TeamFeatureTTLSeconds 1),
          test s "ConferenceCalling 2s TTL" $ testSimpleFlagTTL @'Public.TeamFeatureConferenceCalling Public.TeamFeatureEnabled (TeamFeatureTTLSeconds 2)
        ],
      testGroup
        "Overrides"
        [ test s "increase to unlimited" $ testSimpleFlagTTLOverride @'Public.TeamFeatureConferenceCalling Public.TeamFeatureEnabled (TeamFeatureTTLSeconds 1) TeamFeatureTTLUnlimited,
          test s "increase" $ testSimpleFlagTTLOverride @'Public.TeamFeatureConferenceCalling Public.TeamFeatureEnabled (TeamFeatureTTLSeconds 1) (TeamFeatureTTLSeconds 2),
          test s "reduce from unlimited" $ testSimpleFlagTTLOverride @'Public.TeamFeatureConferenceCalling Public.TeamFeatureEnabled TeamFeatureTTLUnlimited (TeamFeatureTTLSeconds 1),
          test s "reduce" $ testSimpleFlagTTLOverride @'Public.TeamFeatureConferenceCalling Public.TeamFeatureEnabled (TeamFeatureTTLSeconds 5) (TeamFeatureTTLSeconds 1),
          test s "Unlimited to unlimited" $ testSimpleFlagTTLOverride @'Public.TeamFeatureConferenceCalling Public.TeamFeatureEnabled TeamFeatureTTLUnlimited TeamFeatureTTLUnlimited
        ]
    ]

testSSO :: TestM ()
testSSO = do
  owner <- Util.randomUser
  member <- Util.randomUser
  nonMember <- Util.randomUser
  tid <- Util.createNonBindingTeam "foo" owner []
  Util.connectUsers owner (list1 member [])
  Util.addTeamMember owner tid member (rolePermissions RoleMember) Nothing

  let getSSO :: HasCallStack => Public.TeamFeatureStatusValue -> TestM ()
      getSSO = assertFlagNoConfig @'Public.TeamFeatureSSO $ Util.getTeamFeatureFlag Public.TeamFeatureSSO member tid
      getSSOFeatureConfig :: HasCallStack => Public.TeamFeatureStatusValue -> TestM ()
      getSSOFeatureConfig = assertFlagNoConfig @'Public.TeamFeatureSSO $ Util.getFeatureConfig Public.TeamFeatureSSO member
      getSSOInternal :: HasCallStack => Public.TeamFeatureStatusValue -> TestM ()
      getSSOInternal = assertFlagNoConfig @'Public.TeamFeatureSSO $ Util.getTeamFeatureFlagInternal Public.TeamFeatureSSO tid
      setSSOInternal :: HasCallStack => Public.TeamFeatureStatusValue -> TestM ()
      setSSOInternal = void . Util.putTeamFeatureFlagInternal @'Public.TeamFeatureSSO expect2xx tid . Public.TeamFeatureStatusNoConfig

  assertFlagForbidden $ Util.getTeamFeatureFlag Public.TeamFeatureSSO nonMember tid

  featureSSO <- view (tsGConf . optSettings . setFeatureFlags . flagSSO)
  case featureSSO of
    FeatureSSODisabledByDefault -> do
      -- Test default
      getSSO Public.TeamFeatureDisabled
      getSSOInternal Public.TeamFeatureDisabled
      getSSOFeatureConfig Public.TeamFeatureDisabled

      -- Test override
      setSSOInternal Public.TeamFeatureEnabled
      getSSO Public.TeamFeatureEnabled
      getSSOInternal Public.TeamFeatureEnabled
      getSSOFeatureConfig Public.TeamFeatureEnabled
    FeatureSSOEnabledByDefault -> do
      -- since we don't allow to disable (see 'disableSsoNotImplemented'), we can't test
      -- much here.  (disable failure is covered in "enable/disable SSO" above.)
      getSSO Public.TeamFeatureEnabled
      getSSOInternal Public.TeamFeatureEnabled
      getSSOFeatureConfig Public.TeamFeatureEnabled

testLegalHold :: TestM ()
testLegalHold = do
  owner <- Util.randomUser
  member <- Util.randomUser
  nonMember <- Util.randomUser
  tid <- Util.createNonBindingTeam "foo" owner []
  Util.connectUsers owner (list1 member [])
  Util.addTeamMember owner tid member (rolePermissions RoleMember) Nothing

  let getLegalHold :: HasCallStack => Public.TeamFeatureStatusValue -> TestM ()
      getLegalHold = assertFlagNoConfig @'Public.TeamFeatureLegalHold $ Util.getTeamFeatureFlag Public.TeamFeatureLegalHold member tid
      getLegalHoldInternal :: HasCallStack => Public.TeamFeatureStatusValue -> TestM ()
      getLegalHoldInternal = assertFlagNoConfig @'Public.TeamFeatureLegalHold $ Util.getTeamFeatureFlagInternal Public.TeamFeatureLegalHold tid
      getLegalHoldFeatureConfig = assertFlagNoConfig @'Public.TeamFeatureLegalHold $ Util.getFeatureConfig Public.TeamFeatureLegalHold member

      setLegalHoldInternal :: HasCallStack => Public.TeamFeatureStatusValue -> TestM ()
      setLegalHoldInternal = void . Util.putTeamFeatureFlagInternal @'Public.TeamFeatureLegalHold expect2xx tid . Public.TeamFeatureStatusNoConfig
  getLegalHold Public.TeamFeatureDisabled
  getLegalHoldInternal Public.TeamFeatureDisabled

  assertFlagForbidden $ Util.getTeamFeatureFlag Public.TeamFeatureLegalHold nonMember tid

  -- FUTUREWORK: run two galleys, like below for custom search visibility.
  featureLegalHold <- view (tsGConf . optSettings . setFeatureFlags . flagLegalHold)
  case featureLegalHold of
    FeatureLegalHoldDisabledByDefault -> do
      -- Test default
      getLegalHold Public.TeamFeatureDisabled
      getLegalHoldInternal Public.TeamFeatureDisabled
      getLegalHoldFeatureConfig Public.TeamFeatureDisabled

      -- Test override
      setLegalHoldInternal Public.TeamFeatureEnabled
      getLegalHold Public.TeamFeatureEnabled
      getLegalHoldInternal Public.TeamFeatureEnabled
      getLegalHoldFeatureConfig Public.TeamFeatureEnabled

    -- turned off for instance
    FeatureLegalHoldDisabledPermanently -> do
      Util.putLegalHoldEnabledInternal' expect4xx tid Public.TeamFeatureEnabled

    -- turned off but for whitelisted teams with implicit consent
    FeatureLegalHoldWhitelistTeamsAndImplicitConsent -> do
      Util.putLegalHoldEnabledInternal' expect4xx tid Public.TeamFeatureEnabled

testSearchVisibility :: TestM ()
testSearchVisibility = do
  owner <- Util.randomUser
  member <- Util.randomUser
  nonMember <- Util.randomUser
  tid <- Util.createNonBindingTeam "foo" owner []
  Util.connectUsers owner (list1 member [])
  Util.addTeamMember owner tid member (rolePermissions RoleMember) Nothing

  let getTeamSearchVisibility :: TeamId -> Public.TeamFeatureStatusValue -> TestM ()
      getTeamSearchVisibility teamid expected = do
        g <- view tsGalley
        Util.getTeamSearchVisibilityAvailable g owner teamid !!! do
          statusCode === const 200
          responseJsonEither === const (Right (Public.TeamFeatureStatusNoConfig expected))

  let getTeamSearchVisibilityInternal :: TeamId -> Public.TeamFeatureStatusValue -> TestM ()
      getTeamSearchVisibilityInternal teamid expected = do
        g <- view tsGalley
        Util.getTeamSearchVisibilityAvailableInternal g teamid !!! do
          statusCode === const 200
          responseJsonEither === const (Right (Public.TeamFeatureStatusNoConfig expected))

  let getTeamSearchVisibilityFeatureConfig :: UserId -> Public.TeamFeatureStatusValue -> TestM ()
      getTeamSearchVisibilityFeatureConfig uid expected = do
        g <- view tsGalley
        Util.getFeatureConfigWithGalley Public.TeamFeatureSearchVisibility g uid !!! do
          statusCode === const 200
          responseJsonEither === const (Right (Public.TeamFeatureStatusNoConfig expected))

  let setTeamSearchVisibilityInternal :: TeamId -> Public.TeamFeatureStatusValue -> TestM ()
      setTeamSearchVisibilityInternal teamid val = do
        g <- view tsGalley
        Util.putTeamSearchVisibilityAvailableInternal g teamid val

  assertFlagForbidden $ Util.getTeamFeatureFlag Public.TeamFeatureSearchVisibility nonMember tid

  tid2 <- Util.createNonBindingTeam "foo" owner []
  team2member <- Util.randomUser
  Util.connectUsers owner (list1 team2member [])
  Util.addTeamMember owner tid2 team2member (rolePermissions RoleMember) Nothing

  Util.withCustomSearchFeature FeatureTeamSearchVisibilityDisabledByDefault $ do
    getTeamSearchVisibility tid2 Public.TeamFeatureDisabled
    getTeamSearchVisibilityInternal tid2 Public.TeamFeatureDisabled
    getTeamSearchVisibilityFeatureConfig team2member Public.TeamFeatureDisabled

    setTeamSearchVisibilityInternal tid2 Public.TeamFeatureEnabled
    getTeamSearchVisibility tid2 Public.TeamFeatureEnabled
    getTeamSearchVisibilityInternal tid2 Public.TeamFeatureEnabled
    getTeamSearchVisibilityFeatureConfig team2member Public.TeamFeatureEnabled

    setTeamSearchVisibilityInternal tid2 Public.TeamFeatureDisabled
    getTeamSearchVisibility tid2 Public.TeamFeatureDisabled
    getTeamSearchVisibilityInternal tid2 Public.TeamFeatureDisabled
    getTeamSearchVisibilityFeatureConfig team2member Public.TeamFeatureDisabled

  tid3 <- Util.createNonBindingTeam "foo" owner []
  team3member <- Util.randomUser
  Util.connectUsers owner (list1 team3member [])
  Util.addTeamMember owner tid3 team3member (rolePermissions RoleMember) Nothing

  Util.withCustomSearchFeature FeatureTeamSearchVisibilityEnabledByDefault $ do
    getTeamSearchVisibility tid3 Public.TeamFeatureEnabled
    getTeamSearchVisibilityInternal tid3 Public.TeamFeatureEnabled
    getTeamSearchVisibilityFeatureConfig team3member Public.TeamFeatureEnabled

    setTeamSearchVisibilityInternal tid3 Public.TeamFeatureDisabled
    getTeamSearchVisibility tid3 Public.TeamFeatureDisabled
    getTeamSearchVisibilityInternal tid3 Public.TeamFeatureDisabled
    getTeamSearchVisibilityFeatureConfig team3member Public.TeamFeatureDisabled

    setTeamSearchVisibilityInternal tid3 Public.TeamFeatureEnabled
    getTeamSearchVisibility tid3 Public.TeamFeatureEnabled
    getTeamSearchVisibilityInternal tid3 Public.TeamFeatureEnabled
    getTeamSearchVisibilityFeatureConfig team3member Public.TeamFeatureEnabled

getClassifiedDomains ::
  (HasCallStack, HasGalley m, MonadIO m, MonadHttp m, MonadCatch m) =>
  UserId ->
  TeamId ->
  Public.TeamFeatureStatus 'Public.WithoutLockStatus 'Public.TeamFeatureClassifiedDomains ->
  m ()
getClassifiedDomains member tid =
  assertFlagWithConfig @Public.TeamFeatureClassifiedDomainsConfig $
    Util.getTeamFeatureFlag Public.TeamFeatureClassifiedDomains member tid

getClassifiedDomainsInternal ::
  (HasCallStack, HasGalley m, MonadIO m, MonadHttp m, MonadCatch m) =>
  TeamId ->
  Public.TeamFeatureStatus 'Public.WithoutLockStatus 'Public.TeamFeatureClassifiedDomains ->
  m ()
getClassifiedDomainsInternal tid =
  assertFlagWithConfig @Public.TeamFeatureClassifiedDomainsConfig $
    Util.getTeamFeatureFlagInternal Public.TeamFeatureClassifiedDomains tid

testClassifiedDomainsEnabled :: TestM ()
testClassifiedDomainsEnabled = do
  (_owner, tid, member : _) <- Util.createBindingTeamWithNMembers 1
  let expected =
        Public.TeamFeatureStatusWithConfig
          { Public.tfwcStatus = Public.TeamFeatureEnabled,
            Public.tfwcConfig = Public.TeamFeatureClassifiedDomainsConfig [Domain "example.com"]
          }

  let getClassifiedDomainsFeatureConfig ::
        (HasCallStack, HasGalley m, MonadIO m, MonadHttp m, MonadCatch m) =>
        UserId ->
        Public.TeamFeatureStatus 'Public.WithoutLockStatus 'Public.TeamFeatureClassifiedDomains ->
        m ()
      getClassifiedDomainsFeatureConfig uid = do
        assertFlagWithConfig @Public.TeamFeatureClassifiedDomainsConfig $
          Util.getFeatureConfig Public.TeamFeatureClassifiedDomains uid

  getClassifiedDomains member tid expected
  getClassifiedDomainsInternal tid expected
  getClassifiedDomainsFeatureConfig member expected

testClassifiedDomainsDisabled :: TestM ()
testClassifiedDomainsDisabled = do
  (_owner, tid, member : _) <- Util.createBindingTeamWithNMembers 1
  let expected =
        Public.TeamFeatureStatusWithConfig
          { Public.tfwcStatus = Public.TeamFeatureDisabled,
            Public.tfwcConfig = Public.TeamFeatureClassifiedDomainsConfig []
          }

  let getClassifiedDomainsFeatureConfig ::
        (HasCallStack, HasGalley m, MonadIO m, MonadHttp m, MonadCatch m) =>
        UserId ->
        Public.TeamFeatureStatus 'Public.WithoutLockStatus 'Public.TeamFeatureClassifiedDomains ->
        m ()
      getClassifiedDomainsFeatureConfig uid = do
        assertFlagWithConfig @Public.TeamFeatureClassifiedDomainsConfig $
          Util.getFeatureConfig Public.TeamFeatureClassifiedDomains uid

  let classifiedDomainsDisabled = \opts ->
        opts
          & over
            (optSettings . setFeatureFlags . flagClassifiedDomains)
            (\s -> s {Public.tfwcStatus = Public.TeamFeatureDisabled})
  withSettingsOverrides classifiedDomainsDisabled $ do
    getClassifiedDomains member tid expected
    getClassifiedDomainsInternal tid expected
    getClassifiedDomainsFeatureConfig member expected

testSimpleFlag ::
  forall (a :: Public.TeamFeatureName).
  ( HasCallStack,
    Typeable a,
    HasStatusCol a,
    Public.FeatureHasNoConfig 'Public.WithoutLockStatus a,
    Public.KnownTeamFeatureName a,
    FromJSON (Public.TeamFeatureStatus 'Public.WithoutLockStatus a),
    ToJSON (Public.TeamFeatureStatus 'Public.WithoutLockStatus a)
  ) =>
  Public.TeamFeatureStatusValue ->
  TestM ()
testSimpleFlag defaultValue = testSimpleFlagTTL @a defaultValue TeamFeatureTTLUnlimited

testSimpleFlagTTLOverride ::
  forall (a :: Public.TeamFeatureName).
  ( HasCallStack,
    HasStatusCol a,
    Typeable a,
    Public.FeatureHasNoConfig 'Public.WithoutLockStatus a,
    Public.KnownTeamFeatureName a,
    FromJSON (Public.TeamFeatureStatus 'Public.WithoutLockStatus a),
    ToJSON (Public.TeamFeatureStatus 'Public.WithoutLockStatus a)
  ) =>
  Public.TeamFeatureStatusValue ->
  TeamFeatureTTLValue ->
  TeamFeatureTTLValue ->
  TestM ()
testSimpleFlagTTLOverride defaultValue ttl ttlAfter = do
  let feature = Public.knownTeamFeatureName @a
  owner <- Util.randomUser
  member <- Util.randomUser
  nonMember <- Util.randomUser
  tid <- Util.createNonBindingTeam "foo" owner []
  Util.connectUsers owner (list1 member [])
  Util.addTeamMember owner tid member (rolePermissions RoleMember) Nothing

  let getFlag :: HasCallStack => Public.TeamFeatureStatusValue -> TestM ()
      getFlag = assertFlag @a feature member tid

      getFeatureConfig :: HasCallStack => Public.TeamFeatureStatusValue -> TestM ()
      getFeatureConfig = assertFeatureConfig @a feature member

      getFlagInternal :: HasCallStack => Public.TeamFeatureStatusValue -> TestM ()
      getFlagInternal = assertFlagInternal @a feature tid

      setFlagInternal' :: Public.TeamFeatureStatusValue -> TeamFeatureTTLValue -> TestM ()
      setFlagInternal' = setFlagInternal @a tid

      select :: PrepQuery R (Identity TeamId) (Identity (Maybe TeamFeatureTTLValue))
      select = fromString "select ttl(conference_calling) from team_features where team_id = ?"

      assertUnlimited = do
        -- TTL should be NULL inside cassandra
        cassState <- view tsCass
        liftIO $ do
          storedTTL <- maybe Nothing runIdentity <$> Cql.runClient cassState (Cql.query1 select $ params LocalQuorum (Identity tid))
          storedTTL `shouldBe` Nothing

      half = 500000
      seconds = 1000000

  assertFlagForbidden $ Util.getTeamFeatureFlag feature nonMember tid

  let otherValue = case defaultValue of
        Public.TeamFeatureDisabled -> Public.TeamFeatureEnabled
        Public.TeamFeatureEnabled -> Public.TeamFeatureDisabled

  -- Initial value should be the default value
  getFlag defaultValue
  getFlagInternal defaultValue
  getFeatureConfig defaultValue

  -- Setting should work
  setFlagInternal' otherValue ttl
  getFlag otherValue
  getFeatureConfig otherValue
  getFlagInternal otherValue

  case (ttl, ttlAfter) of
    (TeamFeatureTTLSeconds d, TeamFeatureTTLSeconds d') -> do
      -- wait less than expiration, override and recheck.
      liftIO $ threadDelay (fromIntegral d * half) -- waiting half of TTL
      setFlagInternal' otherValue ttlAfter
      -- value is still correct
      getFlag otherValue

      liftIO $ threadDelay (fromIntegral d' * seconds) -- waiting for new TTL
      getFlag defaultValue
      assertUnlimited -- TTL should be NULL after expiration.
    (TeamFeatureTTLSeconds d, TeamFeatureTTLUnlimited) -> do
      -- wait less than expiration, override and recheck.
      liftIO $ threadDelay (fromIntegral d * half) -- waiting half of TTL
      setFlagInternal' otherValue ttlAfter
      -- value is still correct
      getFlag otherValue
      assertUnlimited
    (TeamFeatureTTLUnlimited, TeamFeatureTTLUnlimited) -> do
      -- overriding in this case should have no effect.
      setFlagInternal' otherValue ttl
      getFlag otherValue
      getFeatureConfig otherValue
      getFlagInternal otherValue

      assertUnlimited
    (TeamFeatureTTLUnlimited, TeamFeatureTTLSeconds d) -> do
      assertUnlimited

      setFlagInternal' otherValue ttlAfter
      getFlag otherValue
      getFeatureConfig otherValue
      getFlagInternal otherValue

      liftIO $ threadDelay (fromIntegral d * seconds) -- waiting it out
      -- value reverts back
      getFlag defaultValue
      -- TTL should be NULL inside cassandra
      assertUnlimited

  -- Clean up
  setFlagInternal' defaultValue TeamFeatureTTLUnlimited
  getFlag defaultValue

testSimpleFlagTTL ::
  forall (a :: Public.TeamFeatureName).
  ( HasCallStack,
    HasStatusCol a,
    Typeable a,
    Public.FeatureHasNoConfig 'Public.WithoutLockStatus a,
    Public.KnownTeamFeatureName a,
    FromJSON (Public.TeamFeatureStatus 'Public.WithoutLockStatus a),
    ToJSON (Public.TeamFeatureStatus 'Public.WithoutLockStatus a)
  ) =>
  Public.TeamFeatureStatusValue ->
  TeamFeatureTTLValue ->
  TestM ()
testSimpleFlagTTL defaultValue ttl = do
  let feature = Public.knownTeamFeatureName @a
  owner <- Util.randomUser
  member <- Util.randomUser
  nonMember <- Util.randomUser
  tid <- Util.createNonBindingTeam "foo" owner []
  Util.connectUsers owner (list1 member [])
  Util.addTeamMember owner tid member (rolePermissions RoleMember) Nothing

  let getFlag :: HasCallStack => Public.TeamFeatureStatusValue -> TestM ()
      getFlag = assertFlag @a feature member tid

      getFeatureConfig :: HasCallStack => Public.TeamFeatureStatusValue -> TestM ()
      getFeatureConfig = assertFeatureConfig @a feature member

      getFlagInternal :: HasCallStack => Public.TeamFeatureStatusValue -> TestM ()
      getFlagInternal = assertFlagInternal @a feature tid

      setFlagInternal' :: Public.TeamFeatureStatusValue -> TeamFeatureTTLValue -> TestM ()
      setFlagInternal' = setFlagInternal @a tid

      select :: PrepQuery R (Identity TeamId) (Identity (Maybe TeamFeatureTTLValue))
      select = fromString "select ttl(conference_calling) from team_features where team_id = ?"

  assertFlagForbidden $ Util.getTeamFeatureFlag feature nonMember tid

  let otherValue = case defaultValue of
        Public.TeamFeatureDisabled -> Public.TeamFeatureEnabled
        Public.TeamFeatureEnabled -> Public.TeamFeatureDisabled

  -- Initial value should be the default value
  getFlag defaultValue
  getFlagInternal defaultValue
  getFeatureConfig defaultValue

  -- Setting should work
  cannon <- view tsCannon
  -- should receive an event
  WS.bracketR cannon member $ \ws -> do
    setFlagInternal' otherValue ttl
    void . liftIO $
      WS.assertMatch (5 # Second) ws $
        wsAssertFeatureConfigUpdate feature otherValue
  getFlag otherValue
  getFeatureConfig otherValue
  getFlagInternal otherValue

  case ttl of
    TeamFeatureTTLSeconds d -> do
      -- should revert back after TTL expires
      liftIO $ threadDelay (fromIntegral d * 1000000)
      getFlag defaultValue
    TeamFeatureTTLUnlimited -> do
      -- TTL should be NULL inside cassandra
      cassState <- view tsCass
      liftIO $ do
        storedTTL <- Cql.runClient cassState $ Cql.query1 select $ params LocalQuorum (Identity tid)
        runIdentity <$> storedTTL `shouldBe` Just Nothing

  -- Clean up
  setFlagInternal' defaultValue TeamFeatureTTLUnlimited
  getFlag defaultValue

testSimpleFlagWithLockStatus ::
  forall (a :: Public.TeamFeatureName).
  ( HasCallStack,
    Typeable a,
    Public.FeatureHasNoConfig 'Public.WithLockStatus a,
    Public.FeatureHasNoConfig 'Public.WithoutLockStatus a,
    Public.KnownTeamFeatureName a,
    FromJSON (Public.TeamFeatureStatus 'Public.WithLockStatus a),
    ToJSON (Public.TeamFeatureStatus 'Public.WithLockStatus a)
  ) =>
  Public.TeamFeatureStatusValue ->
  Public.LockStatusValue ->
  TestM ()
testSimpleFlagWithLockStatus defaultStatus defaultLockStatus = do
  galley <- view tsGalley
  let feature = Public.knownTeamFeatureName @a
  owner <- Util.randomUser
  member <- Util.randomUser
  nonMember <- Util.randomUser
  tid <- Util.createNonBindingTeam "foo" owner []
  Util.connectUsers owner (list1 member [])
  Util.addTeamMember owner tid member (rolePermissions RoleMember) Nothing

  let getFlag :: HasCallStack => Public.TeamFeatureStatusValue -> Public.LockStatusValue -> TestM ()
      getFlag expectedStatus expectedLockStatus = do
        let flag = Util.getTeamFeatureFlag feature member tid
        assertFlagNoConfigWithLockStatus @a flag expectedStatus expectedLockStatus

      getFeatureConfig :: HasCallStack => Public.TeamFeatureStatusValue -> Public.LockStatusValue -> TestM ()
      getFeatureConfig expectedStatus expectedLockStatus = do
        let flag = Util.getFeatureConfig feature member
        assertFlagNoConfigWithLockStatus @a flag expectedStatus expectedLockStatus

      getFlagInternal :: HasCallStack => Public.TeamFeatureStatusValue -> Public.LockStatusValue -> TestM ()
      getFlagInternal expectedStatus expectedLockStatus = do
        let flag = Util.getTeamFeatureFlagInternal feature tid
        assertFlagNoConfigWithLockStatus @a flag expectedStatus expectedLockStatus

      getFlags expectedStatus expectedLockStatus = do
        getFlag expectedStatus expectedLockStatus
        getFeatureConfig expectedStatus expectedLockStatus
        getFlagInternal expectedStatus expectedLockStatus

      setFlagWithGalley :: Public.TeamFeatureStatusValue -> TestM ()
      setFlagWithGalley statusValue =
        Util.putTeamFeatureFlagWithGalley @a galley owner tid (Public.TeamFeatureStatusNoConfig statusValue)
          !!! statusCode === const 200

      assertSetStatusForbidden :: Public.TeamFeatureStatusValue -> TestM ()
      assertSetStatusForbidden statusValue =
        Util.putTeamFeatureFlagWithGalley @a galley owner tid (Public.TeamFeatureStatusNoConfig statusValue)
          !!! statusCode === const 409

      setLockStatus :: Public.LockStatusValue -> TestM ()
      setLockStatus lockStatus =
        Util.setLockStatusInternal @a galley tid lockStatus
          !!! statusCode === const 200

  assertFlagForbidden $ Util.getTeamFeatureFlag feature nonMember tid

  let otherStatus = case defaultStatus of
        Public.TeamFeatureDisabled -> Public.TeamFeatureEnabled
        Public.TeamFeatureEnabled -> Public.TeamFeatureDisabled

  -- Initial status and lock status should be the defaults
  getFlags defaultStatus defaultLockStatus

  -- unlock feature if it is locked
  when (defaultLockStatus == Public.Locked) $ setLockStatus Public.Unlocked

  -- setting should work
  cannon <- view tsCannon
  -- should receive an event
  WS.bracketR cannon member $ \ws -> do
    setFlagWithGalley otherStatus
    void . liftIO $
      WS.assertMatch (5 # Second) ws $
        wsAssertFeatureConfigWithLockStatusUpdate feature otherStatus Public.Unlocked

  getFlags otherStatus Public.Unlocked

  -- lock feature
  setLockStatus Public.Locked
  -- feature status should now be the default again
  getFlags defaultStatus Public.Locked
  assertSetStatusForbidden defaultStatus
  -- unlock feature
  setLockStatus Public.Unlocked
  -- feature status should be the previously set value
  getFlags otherStatus Public.Unlocked

  -- clean up
  setFlagWithGalley defaultStatus
  setLockStatus defaultLockStatus
  getFlags defaultStatus defaultLockStatus

testSelfDeletingMessages :: TestM ()
testSelfDeletingMessages = do
  defLockStatus :: Public.LockStatusValue <-
    view
      ( tsGConf
          . optSettings
          . setFeatureFlags
          . flagSelfDeletingMessages
          . unDefaults
          . to Public.tfwcapsLockStatus
      )

  -- personal users
  let settingWithoutLockStatus :: TeamFeatureStatusValue -> Int32 -> Public.TeamFeatureStatus 'Public.WithoutLockStatus 'Public.TeamFeatureSelfDeletingMessages
      settingWithoutLockStatus stat tout =
        Public.TeamFeatureStatusWithConfig @Public.TeamFeatureSelfDeletingMessagesConfig
          stat
          (Public.TeamFeatureSelfDeletingMessagesConfig tout)
      settingWithLockStatus :: TeamFeatureStatusValue -> Int32 -> Public.LockStatusValue -> Public.TeamFeatureStatus 'Public.WithLockStatus 'Public.TeamFeatureSelfDeletingMessages
      settingWithLockStatus stat tout lockStatus =
        Public.TeamFeatureStatusWithConfigAndLockStatus @Public.TeamFeatureSelfDeletingMessagesConfig
          stat
          (Public.TeamFeatureSelfDeletingMessagesConfig tout)
          lockStatus

  personalUser <- Util.randomUser
  Util.getFeatureConfig Public.TeamFeatureSelfDeletingMessages personalUser
    !!! responseJsonEither === const (Right $ settingWithLockStatus TeamFeatureEnabled 0 defLockStatus)

  -- team users
  galley <- view tsGalley
  (owner, tid, []) <- Util.createBindingTeamWithNMembers 0

  let checkSet :: TeamFeatureStatusValue -> Int32 -> Int -> TestM ()
      checkSet stat tout expectedStatusCode =
        do
          Util.putTeamFeatureFlagInternal @'Public.TeamFeatureSelfDeletingMessages
            galley
            tid
            (settingWithoutLockStatus stat tout)
          !!! statusCode === const expectedStatusCode

      -- internal, public (/team/:tid/features), and team-agnostic (/feature-configs).
      checkGet :: HasCallStack => TeamFeatureStatusValue -> Int32 -> Public.LockStatusValue -> TestM ()
      checkGet stat tout lockStatus = do
        let expected = settingWithLockStatus stat tout lockStatus
        forM_
          [ Util.getTeamFeatureFlagInternal Public.TeamFeatureSelfDeletingMessages tid,
            Util.getTeamFeatureFlagWithGalley Public.TeamFeatureSelfDeletingMessages galley owner tid,
            Util.getFeatureConfig Public.TeamFeatureSelfDeletingMessages owner
          ]
          (!!! responseJsonEither === const (Right expected))

      checkSetLockStatus :: HasCallStack => Public.LockStatusValue -> TestM ()
      checkSetLockStatus status =
        do
          Util.setLockStatusInternal @'Public.TeamFeatureSelfDeletingMessages galley tid status
          !!! statusCode === const 200

  -- test that the default lock status comes from `galley.yaml`.
  -- use this to change `galley.integration.yaml` locally and manually test that conf file
  -- parsing works as expected.
  checkGet TeamFeatureEnabled 0 defLockStatus

  case defLockStatus of
    Public.Locked -> do
      checkSet TeamFeatureDisabled 0 409
    Public.Unlocked -> do
      checkSet TeamFeatureDisabled 0 200
      checkGet TeamFeatureDisabled 0 Public.Unlocked
      checkSet TeamFeatureEnabled 0 200
      checkGet TeamFeatureEnabled 0 Public.Unlocked

  -- now don't worry about what's in the config, write something to cassandra, and test with that.
  checkSetLockStatus Public.Locked
  checkGet TeamFeatureEnabled 0 Public.Locked
  checkSet TeamFeatureDisabled 0 409
  checkGet TeamFeatureEnabled 0 Public.Locked
  checkSet TeamFeatureEnabled 30 409
  checkGet TeamFeatureEnabled 0 Public.Locked
  checkSetLockStatus Public.Unlocked
  checkGet TeamFeatureEnabled 0 Public.Unlocked
  checkSet TeamFeatureDisabled 0 200
  checkGet TeamFeatureDisabled 0 Public.Unlocked
  checkSet TeamFeatureEnabled 30 200
  checkGet TeamFeatureEnabled 30 Public.Unlocked
  checkSet TeamFeatureDisabled 30 200
  checkGet TeamFeatureDisabled 30 Public.Unlocked
  checkSetLockStatus Public.Locked
  checkGet TeamFeatureEnabled 0 Public.Locked
  checkSet TeamFeatureEnabled 50 409
  checkSetLockStatus Public.Unlocked
  checkGet TeamFeatureDisabled 30 Public.Unlocked

testGuestLinksInternal :: TestM ()
testGuestLinksInternal = do
  galley <- view tsGalley
  testGuestLinks
    (const $ Util.getTeamFeatureFlagInternal Public.TeamFeatureGuestLinks)
    (const $ Util.putTeamFeatureFlagInternal @'Public.TeamFeatureGuestLinks galley)
    (Util.setLockStatusInternal @'Public.TeamFeatureGuestLinks galley)

testGuestLinksPublic :: TestM ()
testGuestLinksPublic = do
  galley <- view tsGalley
  testGuestLinks
    (Util.getTeamFeatureFlagWithGalley Public.TeamFeatureGuestLinks galley)
    (Util.putTeamFeatureFlagWithGalley @'Public.TeamFeatureGuestLinks galley)
    (Util.setLockStatusInternal @'Public.TeamFeatureGuestLinks galley)

testGuestLinks ::
  (UserId -> TeamId -> TestM ResponseLBS) ->
  (UserId -> TeamId -> Public.TeamFeatureStatusNoConfig -> TestM ResponseLBS) ->
  (TeamId -> Public.LockStatusValue -> TestM ResponseLBS) ->
  TestM ()
testGuestLinks getStatus putStatus setLockStatusInternal = do
  (owner, tid, []) <- Util.createBindingTeamWithNMembers 0
  let checkGet :: HasCallStack => Public.TeamFeatureStatusValue -> Public.LockStatusValue -> TestM ()
      checkGet status lock =
        getStatus owner tid !!! do
          statusCode === const 200
          responseJsonEither === const (Right (Public.TeamFeatureStatusNoConfigAndLockStatus status lock))

      checkSet :: HasCallStack => Public.TeamFeatureStatusValue -> Int -> TestM ()
      checkSet status expectedStatusCode =
        putStatus owner tid (Public.TeamFeatureStatusNoConfig status) !!! statusCode === const expectedStatusCode

      checkSetLockStatusInternal :: HasCallStack => Public.LockStatusValue -> TestM ()
      checkSetLockStatusInternal lockStatus =
        setLockStatusInternal tid lockStatus !!! statusCode === const 200

  checkGet Public.TeamFeatureEnabled Public.Unlocked
  checkSet Public.TeamFeatureDisabled 200
  checkGet Public.TeamFeatureDisabled Public.Unlocked
  checkSet Public.TeamFeatureEnabled 200
  checkGet Public.TeamFeatureEnabled Public.Unlocked
  checkSet Public.TeamFeatureDisabled 200
  checkGet Public.TeamFeatureDisabled Public.Unlocked
  -- when locks status is locked the team default feature status should be returned
  -- and the team feature status can not be changed
  checkSetLockStatusInternal Public.Locked
  checkGet Public.TeamFeatureEnabled Public.Locked
  checkSet Public.TeamFeatureDisabled 409
  -- when lock status is unlocked again the previously set feature status is restored
  checkSetLockStatusInternal Public.Unlocked
  checkGet Public.TeamFeatureDisabled Public.Unlocked

-- | Call 'GET /teams/:tid/features' and 'GET /feature-configs', and check if all
-- features are there.
testAllFeatures :: TestM ()
testAllFeatures = do
  defLockStatus :: Public.LockStatusValue <-
    view
      ( tsGConf
          . optSettings
          . setFeatureFlags
          . flagSelfDeletingMessages
          . unDefaults
          . to Public.tfwcapsLockStatus
      )

  (_owner, tid, member : _) <- Util.createBindingTeamWithNMembers 1
  Util.getAllTeamFeatures member tid !!! do
    statusCode === const 200
    responseJsonMaybe === const (Just (expected TeamFeatureEnabled defLockStatus {- determined by default in galley -}))
  Util.getAllTeamFeaturesPersonal member !!! do
    statusCode === const 200
    responseJsonMaybe === const (Just (expected TeamFeatureEnabled defLockStatus {- determined by default in galley -}))

  randomPersonalUser <- Util.randomUser
  Util.getAllTeamFeaturesPersonal randomPersonalUser !!! do
    statusCode === const 200
    responseJsonMaybe === const (Just (expected TeamFeatureEnabled defLockStatus {- determined by 'getAfcConferenceCallingDefNew' in brig -}))
  where
    expected confCalling lockState =
      object
        [ toS TeamFeatureLegalHold .= Public.TeamFeatureStatusNoConfig TeamFeatureDisabled,
          toS TeamFeatureSSO .= Public.TeamFeatureStatusNoConfig TeamFeatureDisabled,
          toS TeamFeatureSearchVisibility .= Public.TeamFeatureStatusNoConfig TeamFeatureDisabled,
          toS TeamFeatureValidateSAMLEmails .= Public.TeamFeatureStatusNoConfig TeamFeatureDisabled,
          toS TeamFeatureDigitalSignatures .= Public.TeamFeatureStatusNoConfig TeamFeatureDisabled,
          toS TeamFeatureAppLock
            .= Public.TeamFeatureStatusWithConfig
              TeamFeatureEnabled
              (Public.TeamFeatureAppLockConfig (Public.EnforceAppLock False) (60 :: Int32)),
          toS TeamFeatureFileSharing .= Public.TeamFeatureStatusNoConfigAndLockStatus TeamFeatureEnabled Public.Unlocked,
          toS TeamFeatureClassifiedDomains
            .= Public.TeamFeatureStatusWithConfig
              TeamFeatureEnabled
              (Public.TeamFeatureClassifiedDomainsConfig [Domain "example.com"]),
          toS TeamFeatureConferenceCalling
            .= Public.TeamFeatureStatusNoConfig confCalling,
          toS TeamFeatureSelfDeletingMessages
            .= Public.TeamFeatureStatusWithConfigAndLockStatus @Public.TeamFeatureSelfDeletingMessagesConfig
              TeamFeatureEnabled
              (Public.TeamFeatureSelfDeletingMessagesConfig 0)
              lockState,
          toS TeamFeatureGuestLinks
            .= Public.TeamFeatureStatusNoConfigAndLockStatus
              TeamFeatureEnabled
              Public.Unlocked,
          toS TeamFeatureValidateSAMLEmails .= Public.TeamFeatureStatusNoConfig TeamFeatureEnabled,
          toS TeamFeatureGuestLinks .= Public.TeamFeatureStatusNoConfigAndLockStatus TeamFeatureEnabled Public.Unlocked,
          toS TeamFeatureSndFactorPasswordChallenge .= Public.TeamFeatureStatusNoConfigAndLockStatus TeamFeatureDisabled Public.Locked
        ]
    toS :: TeamFeatureName -> Aeson.Key
    toS = AesonKey.fromText . TE.decodeUtf8 . toByteString'

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
  let defaultValue = TeamFeatureDisabled
  let feature = Public.knownTeamFeatureName @'TeamFeatureSearchVisibilityInbound
  owner <- Util.randomUser
  tid <- Util.createNonBindingTeam "foo" owner []

  let getFlagInternal :: HasCallStack => Public.TeamFeatureStatusValue -> TestM ()
      getFlagInternal = assertFlagInternal @'TeamFeatureSearchVisibilityInbound feature tid

      setFlagInternal' :: Public.TeamFeatureStatusValue -> TestM ()
      setFlagInternal' = flip (setFlagInternal @'TeamFeatureSearchVisibilityInbound tid) TeamFeatureTTLUnlimited

      otherValue = case defaultValue of
        Public.TeamFeatureDisabled -> Public.TeamFeatureEnabled
        Public.TeamFeatureEnabled -> Public.TeamFeatureDisabled

  -- Initial value should be the default value
  getFlagInternal defaultValue
  setFlagInternal' otherValue
  getFlagInternal otherValue

testFeatureNoConfigMultiSearchVisibilityInbound :: TestM ()
testFeatureNoConfigMultiSearchVisibilityInbound = do
  owner1 <- Util.randomUser
  team1 <- Util.createNonBindingTeam "team1" owner1 []

  owner2 <- Util.randomUser
  team2 <- Util.createNonBindingTeam "team2" owner2 []

  let setFlagInternal' :: TeamId -> Public.TeamFeatureStatusValue -> TestM ()
      setFlagInternal' tid = flip (setFlagInternal @'TeamFeatureSearchVisibilityInbound tid) TeamFeatureTTLUnlimited

  setFlagInternal' team2 Public.TeamFeatureEnabled

  r <-
    getFeatureStatusMulti TeamFeatureSearchVisibilityInbound (Multi.TeamFeatureNoConfigMultiRequest [team1, team2])
      <!! statusCode === const 200

  Multi.TeamFeatureNoConfigMultiResponse teamsStatuses :: Multi.TeamFeatureNoConfigMultiResponse 'TeamFeatureSearchVisibilityInbound <- responseJsonError r

  liftIO $ do
    length teamsStatuses @?= 2

    Multi.TeamStatus _ team1Status team1WriteTime <- Util.assertOne (filter ((== team1) . Multi.team) teamsStatuses)
    team1Status @?= Public.TeamFeatureDisabled
    assertBool "expected Nothing" (isNothing team1WriteTime)

    Multi.TeamStatus _ team2Status team2WriteTime <- Util.assertOne (filter ((== team2) . Multi.team) teamsStatuses)
    team2Status @?= Public.TeamFeatureEnabled
    assertBool "expected Just" (isJust team2WriteTime)

assertFlagForbidden :: HasCallStack => TestM ResponseLBS -> TestM ()
assertFlagForbidden res = do
  res !!! do
    statusCode === const 403
    fmap label . responseJsonMaybe === const (Just "no-team-member")

assertFlagNoConfig ::
  forall (a :: Public.TeamFeatureName).
  ( HasCallStack,
    Typeable a,
    Public.FeatureHasNoConfig 'Public.WithoutLockStatus a,
    FromJSON (Public.TeamFeatureStatus 'Public.WithoutLockStatus a),
    Public.KnownTeamFeatureName a
  ) =>
  TestM ResponseLBS ->
  Public.TeamFeatureStatusValue ->
  TestM ()
assertFlagNoConfig res expected = do
  res !!! do
    statusCode === const 200
    ( fmap Public.tfwoStatus
        . responseJsonEither @(Public.TeamFeatureStatus 'Public.WithoutLockStatus a)
      )
      === const (Right expected)

assertFlagNoConfigWithLockStatus ::
  forall (a :: Public.TeamFeatureName).
  ( HasCallStack,
    Typeable a,
    Public.FeatureHasNoConfig 'Public.WithLockStatus a,
    FromJSON (Public.TeamFeatureStatus 'Public.WithLockStatus a),
    Public.KnownTeamFeatureName a
  ) =>
  TestM ResponseLBS ->
  Public.TeamFeatureStatusValue ->
  Public.LockStatusValue ->
  TestM ()
assertFlagNoConfigWithLockStatus res expectedStatus expectedLockStatus = do
  res !!! do
    statusCode === const 200
    responseJsonEither @(Public.TeamFeatureStatus 'Public.WithLockStatus a)
      === const (Right (Public.TeamFeatureStatusNoConfigAndLockStatus expectedStatus expectedLockStatus))

assertFlagWithConfig ::
  forall cfg m.
  ( HasCallStack,
    Eq cfg,
    ToSchema cfg,
    Show cfg,
    Typeable cfg,
    MonadIO m,
    MonadCatch m
  ) =>
  m ResponseLBS ->
  Public.TeamFeatureStatusWithConfig cfg ->
  m ()
assertFlagWithConfig response expected = do
  r <- response
  let rJson = responseJsonEither @(Public.TeamFeatureStatusWithConfig cfg) r
  pure r !!! statusCode === const 200
  liftIO $ do
    fmap Public.tfwcStatus rJson @?= (Right . Public.tfwcStatus $ expected)
    fmap Public.tfwcConfig rJson @?= (Right . Public.tfwcConfig $ expected)

wsAssertFeatureConfigUpdate :: Public.TeamFeatureName -> Public.TeamFeatureStatusValue -> Notification -> IO ()
wsAssertFeatureConfigUpdate teamFeature status notification = do
  let e :: FeatureConfig.Event = List1.head (WS.unpackPayload notification)
  FeatureConfig._eventType e @?= FeatureConfig.Update
  FeatureConfig._eventFeatureName e @?= teamFeature
  FeatureConfig._eventData e @?= EdFeatureWithoutConfigChanged (Public.TeamFeatureStatusNoConfig status)

wsAssertFeatureConfigWithLockStatusUpdate :: Public.TeamFeatureName -> Public.TeamFeatureStatusValue -> Public.LockStatusValue -> Notification -> IO ()
wsAssertFeatureConfigWithLockStatusUpdate teamFeature status lockStatus notification = do
  let e :: FeatureConfig.Event = List1.head (WS.unpackPayload notification)
  FeatureConfig._eventType e @?= FeatureConfig.Update
  FeatureConfig._eventFeatureName e @?= teamFeature
  FeatureConfig._eventData e
    @?= EdFeatureWithoutConfigAndLockStatusChanged
      (Public.TeamFeatureStatusNoConfigAndLockStatus status lockStatus)

assertFlag ::
  forall (a :: Public.TeamFeatureName).
  ( HasStatusCol a,
    Public.FeatureHasNoConfig 'Public.WithoutLockStatus a,
    Public.KnownTeamFeatureName a,
    Typeable a,
    HasCallStack
  ) =>
  Public.TeamFeatureName ->
  UserId ->
  TeamId ->
  Public.TeamFeatureStatusValue ->
  TestM ()
assertFlag feature member tid expected =
  flip (assertFlagNoConfig @a) expected $ Util.getTeamFeatureFlag feature member tid

assertFeatureConfig ::
  forall (a :: Public.TeamFeatureName).
  ( HasStatusCol a,
    Public.FeatureHasNoConfig 'Public.WithoutLockStatus a,
    Public.KnownTeamFeatureName a,
    Typeable a,
    HasCallStack
  ) =>
  Public.TeamFeatureName ->
  UserId ->
  Public.TeamFeatureStatusValue ->
  TestM ()
assertFeatureConfig feature member expected =
  flip (assertFlagNoConfig @a) expected $ Util.getFeatureConfig feature member

assertFlagInternal ::
  forall (a :: Public.TeamFeatureName).
  ( HasStatusCol a,
    Public.FeatureHasNoConfig 'Public.WithoutLockStatus a,
    Public.KnownTeamFeatureName a,
    Typeable a,
    HasCallStack
  ) =>
  Public.TeamFeatureName ->
  TeamId ->
  Public.TeamFeatureStatusValue ->
  TestM ()
assertFlagInternal feature tid expected =
  flip (assertFlagNoConfig @a) expected $ Util.getTeamFeatureFlagInternal feature tid

setFlagInternal ::
  forall (a :: Public.TeamFeatureName).
  ( HasStatusCol a,
    Public.FeatureHasNoConfig 'Public.WithoutLockStatus a,
    Public.KnownTeamFeatureName a,
    Typeable a,
    HasCallStack
  ) =>
  TeamId ->
  Public.TeamFeatureStatusValue ->
  TeamFeatureTTLValue ->
  TestM ()
setFlagInternal tid statusValue ttl' =
  void $ Util.putTeamFeatureFlagInternalTTL @a expect2xx tid (Public.TeamFeatureStatusNoConfig statusValue) ttl'
