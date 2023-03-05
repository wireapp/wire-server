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

import API.SQS (assertTeamActivate)
import API.Util
import API.Util.TeamFeature hiding (getFeatureConfig, setLockStatusInternal)
import qualified API.Util.TeamFeature as Util
import Bilge
import Bilge.Assert
import Brig.Types.Test.Arbitrary (Arbitrary (arbitrary))
import Cassandra as Cql
import Control.Lens (over, to, view, (.~), (?~))
import Control.Lens.Operators ()
import Control.Monad.Catch (MonadCatch)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString.Char8 (unpack)
import Data.Domain (Domain (..))
import Data.Id
import qualified Data.List1 as List1
import Data.Schema (ToSchema)
import qualified Data.Set as Set
import Data.Timeout (TimeoutUnit (Second), (#))
import GHC.TypeLits (KnownSymbol)
import Galley.Options (optSettings, setExposeInvitationURLsTeamAllowlist, setFeatureFlags)
import Galley.Types.Teams
import Imports
import Network.Wai.Utilities (label)
import Test.Hspec (expectationFailure)
import Test.QuickCheck (Gen, generate, suchThat)
import Test.Tasty
import qualified Test.Tasty.Cannon as WS
import Test.Tasty.HUnit (assertBool, assertFailure, (@?=))
import TestHelpers (eventually, test)
import TestSetup
import Wire.API.Conversation.Protocol (ProtocolTag (ProtocolMLSTag, ProtocolProteusTag))
import qualified Wire.API.Event.FeatureConfig as FeatureConfig
import Wire.API.Internal.Notification (Notification)
import Wire.API.MLS.CipherSuite
import Wire.API.Routes.Internal.Galley.TeamFeatureNoConfigMulti as Multi
import Wire.API.Team.Feature hiding (setLockStatus)

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "Feature Config API and Team Features API"
    [ test s "SSO - set with HTTP PUT" (testSSO putSSOInternal),
      test s "SSO - set with HTTP PATCH" (testSSO patchSSOInternal),
      test s "LegalHold - set with HTTP PUT" (testLegalHold putLegalHoldInternal),
      test s "LegalHold - set with HTTP PATCH" (testLegalHold patchLegalHoldInternal),
      test s "SearchVisibility" testSearchVisibility,
      test s "DigitalSignatures" $ testSimpleFlag @DigitalSignaturesConfig FeatureStatusDisabled,
      test s "ValidateSAMLEmails" $ testSimpleFlag @ValidateSAMLEmailsConfig FeatureStatusEnabled,
      test s "FileSharing with lock status" $ testSimpleFlagWithLockStatus @FileSharingConfig FeatureStatusEnabled LockStatusUnlocked,
      test s "Classified Domains (enabled)" testClassifiedDomainsEnabled,
      test s "Classified Domains (disabled)" testClassifiedDomainsDisabled,
      test s "All features" testAllFeatures,
      test s "Feature Configs / Team Features Consistency" testFeatureConfigConsistency,
      test s "ConferenceCalling" $ testSimpleFlag @ConferenceCallingConfig FeatureStatusEnabled,
      test s "SelfDeletingMessages" testSelfDeletingMessages,
      test s "ConversationGuestLinks - public API" testGuestLinksPublic,
      test s "ConversationGuestLinks - internal API" testGuestLinksInternal,
      test s "ConversationGuestLinks - lock status" $ testSimpleFlagWithLockStatus @GuestLinksConfig FeatureStatusEnabled LockStatusUnlocked,
      test s "SndFactorPasswordChallenge - lock status" $ testSimpleFlagWithLockStatus @SndFactorPasswordChallengeConfig FeatureStatusDisabled LockStatusLocked,
      test s "SearchVisibilityInbound - internal API" testSearchVisibilityInbound,
      test s "SearchVisibilityInbound - internal multi team API" testFeatureNoConfigMultiSearchVisibilityInbound,
      test s "OutlookCalIntegration" $ testSimpleFlagWithLockStatus @OutlookCalIntegrationConfig FeatureStatusDisabled LockStatusLocked,
      testGroup
        "TTL / Conference calling"
        [ test s "ConferenceCalling unlimited TTL" $ testSimpleFlagTTL @ConferenceCallingConfig FeatureStatusEnabled FeatureTTLUnlimited,
          test s "ConferenceCalling 2s TTL" $ testSimpleFlagTTL @ConferenceCallingConfig FeatureStatusEnabled (FeatureTTLSeconds 2)
        ],
      testGroup
        "TTL / Overrides"
        [ test s "increase to unlimited" $ testSimpleFlagTTLOverride @ConferenceCallingConfig FeatureStatusEnabled (FeatureTTLSeconds 2) FeatureTTLUnlimited,
          test s "increase" $ testSimpleFlagTTLOverride @ConferenceCallingConfig FeatureStatusEnabled (FeatureTTLSeconds 2) (FeatureTTLSeconds 4),
          test s "reduce from unlimited" $ testSimpleFlagTTLOverride @ConferenceCallingConfig FeatureStatusEnabled FeatureTTLUnlimited (FeatureTTLSeconds 2),
          test s "reduce" $ testSimpleFlagTTLOverride @ConferenceCallingConfig FeatureStatusEnabled (FeatureTTLSeconds 5) (FeatureTTLSeconds 2),
          test s "Unlimited to unlimited" $ testSimpleFlagTTLOverride @ConferenceCallingConfig FeatureStatusEnabled FeatureTTLUnlimited FeatureTTLUnlimited
        ],
      test s "MLS feature config" testMLS,
      test s "SearchVisibilityInbound" $ testSimpleFlag @SearchVisibilityInboundConfig FeatureStatusDisabled,
      test s "MlsE2EId feature config" $
        testNonTrivialConfigNoTTL
          ( withStatus
              FeatureStatusDisabled
              LockStatusUnlocked
              (MlsE2EIdConfig Nothing)
              FeatureTTLUnlimited
          ),
      testGroup
        "Patch"
        [ -- Note: `SSOConfig` and `LegalHoldConfig` may not be able to be reset
          -- (depending on prior state or configuration). Thus, they cannot be
          -- tested here (setting random values), but are tested with separate
          -- tests.
          test s (unpack $ featureNameBS @SearchVisibilityAvailableConfig) $
            testPatch IgnoreLockStatusChange FeatureStatusEnabled SearchVisibilityAvailableConfig,
          test s (unpack $ featureNameBS @ValidateSAMLEmailsConfig) $
            testPatch IgnoreLockStatusChange FeatureStatusEnabled ValidateSAMLEmailsConfig,
          test s (unpack $ featureNameBS @DigitalSignaturesConfig) $
            testPatch IgnoreLockStatusChange FeatureStatusEnabled DigitalSignaturesConfig,
          test s (unpack $ featureNameBS @AppLockConfig) $
            testPatchWithCustomGen IgnoreLockStatusChange FeatureStatusEnabled (AppLockConfig (EnforceAppLock False) 60) validAppLockConfigGen,
          test s (unpack $ featureNameBS @ConferenceCallingConfig) $
            testPatch IgnoreLockStatusChange FeatureStatusEnabled ConferenceCallingConfig,
          test s (unpack $ featureNameBS @SearchVisibilityAvailableConfig) $
            testPatch IgnoreLockStatusChange FeatureStatusEnabled SearchVisibilityAvailableConfig,
          test s (unpack $ featureNameBS @MLSConfig) $
            testPatchWithCustomGen
              IgnoreLockStatusChange
              FeatureStatusEnabled
              ( MLSConfig
                  []
                  ProtocolProteusTag
                  [MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519]
                  MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519
              )
              validMLSConfigGen,
          test s (unpack $ featureNameBS @FileSharingConfig) $
            testPatch AssertLockStatusChange FeatureStatusEnabled FileSharingConfig,
          test s (unpack $ featureNameBS @GuestLinksConfig) $
            testPatch AssertLockStatusChange FeatureStatusEnabled GuestLinksConfig,
          test s (unpack $ featureNameBS @SndFactorPasswordChallengeConfig) $
            testPatch AssertLockStatusChange FeatureStatusDisabled SndFactorPasswordChallengeConfig,
          test s (unpack $ featureNameBS @SelfDeletingMessagesConfig) $
            testPatch AssertLockStatusChange FeatureStatusEnabled (SelfDeletingMessagesConfig 0),
          test s (unpack $ featureNameBS @OutlookCalIntegrationConfig) $
            testPatch AssertLockStatusChange FeatureStatusDisabled OutlookCalIntegrationConfig,
          test s (unpack $ featureNameBS @MlsE2EIdConfig) $ testPatchWithArbitrary AssertLockStatusChange FeatureStatusDisabled (MlsE2EIdConfig Nothing)
        ],
      testGroup
        "ExposeInvitationURLsToTeamAdmin"
        [ test s "can be set when TeamId is in allow list" testExposeInvitationURLsToTeamAdminTeamIdInAllowList,
          test s "can not be set when allow list is empty" testExposeInvitationURLsToTeamAdminEmptyAllowList,
          test s "server config takes precendece over team feature config" testExposeInvitationURLsToTeamAdminServerConfigTakesPrecedence
        ]
    ]

-- | Provides a `Gen` with test objects that are realistic and can easily be asserted
validMLSConfigGen :: Gen (WithStatusPatch MLSConfig)
validMLSConfigGen =
  arbitrary
    `suchThat` ( \cfg -> case wspConfig cfg of
                   Just (MLSConfig us _ cTags ctag) ->
                     sortedAndNoDuplicates us
                       && sortedAndNoDuplicates cTags
                       && elem ctag cTags
                   _ -> True
               )
  where
    sortedAndNoDuplicates xs = (sort . nub) xs == xs

validAppLockConfigGen :: Gen (WithStatusPatch AppLockConfig)
validAppLockConfigGen =
  arbitrary
    `suchThat` ( \cfg -> case wspConfig cfg of
                   Just (AppLockConfig _ secs) -> secs >= 30
                   Nothing -> True
               )

-- | Binary type to prevent "boolean blindness"
data AssertLockStatusChange = AssertLockStatusChange | IgnoreLockStatusChange
  deriving (Eq)

testPatchWithArbitrary ::
  forall cfg.
  ( HasCallStack,
    IsFeatureConfig cfg,
    Typeable cfg,
    ToSchema cfg,
    Eq cfg,
    Show cfg,
    KnownSymbol (FeatureSymbol cfg),
    Arbitrary (WithStatusPatch cfg)
  ) =>
  AssertLockStatusChange ->
  FeatureStatus ->
  cfg ->
  TestM ()
testPatchWithArbitrary assertLockStatusChange featureStatus cfg = do
  generatedConfig <- liftIO $ generate arbitrary
  testPatch' assertLockStatusChange generatedConfig featureStatus cfg

testPatchWithCustomGen ::
  forall cfg.
  ( HasCallStack,
    IsFeatureConfig cfg,
    Typeable cfg,
    ToSchema cfg,
    Eq cfg,
    Show cfg,
    KnownSymbol (FeatureSymbol cfg)
  ) =>
  AssertLockStatusChange ->
  FeatureStatus ->
  cfg ->
  Gen (WithStatusPatch cfg) ->
  TestM ()
testPatchWithCustomGen assertLockStatusChange featureStatus cfg gen = do
  generatedConfig <- liftIO $ generate gen
  testPatch' assertLockStatusChange generatedConfig featureStatus cfg

testPatch ::
  forall cfg.
  ( HasCallStack,
    IsFeatureConfig cfg,
    Typeable cfg,
    ToSchema cfg,
    Eq cfg,
    Show cfg,
    KnownSymbol (FeatureSymbol cfg),
    Arbitrary (WithStatusPatch cfg)
  ) =>
  AssertLockStatusChange ->
  FeatureStatus ->
  cfg ->
  TestM ()
testPatch assertLockStatusChange status cfg = testPatchWithCustomGen assertLockStatusChange status cfg arbitrary

testPatch' ::
  forall cfg.
  ( HasCallStack,
    IsFeatureConfig cfg,
    Typeable cfg,
    ToSchema cfg,
    Eq cfg,
    Show cfg,
    KnownSymbol (FeatureSymbol cfg)
  ) =>
  AssertLockStatusChange ->
  WithStatusPatch cfg ->
  FeatureStatus ->
  cfg ->
  TestM ()
testPatch' testLockStatusChange rndFeatureConfig defStatus defConfig = do
  (_, tid) <- createBindingTeam
  Just original <- responseJsonMaybe <$> getFeatureStatusInternal @cfg tid
  patchFeatureStatusInternal tid rndFeatureConfig !!! statusCode === const 200
  Just actual <- responseJsonMaybe <$> getFeatureStatusInternal @cfg tid
  liftIO $
    if wsLockStatus actual == LockStatusLocked
      then do
        wsStatus actual @?= defStatus
        wsConfig actual @?= defConfig
      else do
        wsStatus actual @?= fromMaybe (wsStatus original) (wspStatus rndFeatureConfig)
        when (testLockStatusChange == AssertLockStatusChange) $
          wsLockStatus actual @?= fromMaybe (wsLockStatus original) (wspLockStatus rndFeatureConfig)
        wsConfig actual @?= fromMaybe (wsConfig original) (wspConfig rndFeatureConfig)

testSSO :: (TeamId -> FeatureStatus -> TestM ()) -> TestM ()
testSSO setSSOFeature = do
  (_owner, tid, member : _) <- createBindingTeamWithNMembers 1
  nonMember <- randomUser

  let getSSO :: HasCallStack => FeatureStatus -> TestM ()
      getSSO = assertFlagNoConfig @SSOConfig $ getTeamFeatureFlag @SSOConfig member tid
      getSSOFeatureConfig :: HasCallStack => FeatureStatus -> TestM ()
      getSSOFeatureConfig expectedStatus = do
        actual <- Util.getFeatureConfig @SSOConfig member
        liftIO $ wsStatus actual @?= expectedStatus
      getSSOInternal :: HasCallStack => FeatureStatus -> TestM ()
      getSSOInternal = assertFlagNoConfig @SSOConfig $ getTeamFeatureFlagInternal @SSOConfig tid

  assertFlagForbidden $ getTeamFeatureFlag @SSOConfig nonMember tid

  featureSSO <- view (tsGConf . optSettings . setFeatureFlags . flagSSO)
  case featureSSO of
    FeatureSSODisabledByDefault -> do
      -- Test default
      getSSO FeatureStatusDisabled
      getSSOInternal FeatureStatusDisabled
      getSSOFeatureConfig FeatureStatusDisabled

      -- Test override
      setSSOFeature tid FeatureStatusEnabled
      getSSO FeatureStatusEnabled
      getSSOInternal FeatureStatusEnabled
      getSSOFeatureConfig FeatureStatusEnabled
    FeatureSSOEnabledByDefault -> do
      -- since we don't allow to disable (see 'disableSsoNotImplemented'), we can't test
      -- much here.  (disable failure is covered in "enable/disable SSO" above.)
      getSSO FeatureStatusEnabled
      getSSOInternal FeatureStatusEnabled
      getSSOFeatureConfig FeatureStatusEnabled

putSSOInternal :: HasCallStack => TeamId -> FeatureStatus -> TestM ()
putSSOInternal tid =
  void
    . putTeamFeatureFlagInternal @SSOConfig expect2xx tid
    . (\st -> WithStatusNoLock st SSOConfig FeatureTTLUnlimited)

patchSSOInternal :: HasCallStack => TeamId -> FeatureStatus -> TestM ()
patchSSOInternal tid status = void $ patchFeatureStatusInternalWithMod @SSOConfig expect2xx tid (withStatus' (Just status) Nothing Nothing (Just FeatureTTLUnlimited))

testLegalHold :: ((Request -> Request) -> TeamId -> FeatureStatus -> TestM ()) -> TestM ()
testLegalHold setLegalHoldInternal = do
  (_owner, tid, member : _) <- createBindingTeamWithNMembers 1
  nonMember <- randomUser
  let getLegalHold :: HasCallStack => FeatureStatus -> TestM ()
      getLegalHold = assertFlagNoConfig @LegalholdConfig $ getTeamFeatureFlag @LegalholdConfig member tid
      getLegalHoldInternal :: HasCallStack => FeatureStatus -> TestM ()
      getLegalHoldInternal = assertFlagNoConfig @LegalholdConfig $ getTeamFeatureFlagInternal @LegalholdConfig tid
      getLegalHoldFeatureConfig expectedStatus = do
        actual <- Util.getFeatureConfig @LegalholdConfig member
        liftIO $ wsStatus actual @?= expectedStatus

  getLegalHold FeatureStatusDisabled
  getLegalHoldInternal FeatureStatusDisabled

  assertFlagForbidden $ getTeamFeatureFlag @LegalholdConfig nonMember tid

  -- FUTUREWORK: run two galleys, like below for custom search visibility.
  featureLegalHold <- view (tsGConf . optSettings . setFeatureFlags . flagLegalHold)
  case featureLegalHold of
    FeatureLegalHoldDisabledByDefault -> do
      -- Test default
      getLegalHold FeatureStatusDisabled
      getLegalHoldInternal FeatureStatusDisabled
      getLegalHoldFeatureConfig FeatureStatusDisabled

      -- Test override
      setLegalHoldInternal expect2xx tid FeatureStatusEnabled
      getLegalHold FeatureStatusEnabled
      getLegalHoldInternal FeatureStatusEnabled
      getLegalHoldFeatureConfig FeatureStatusEnabled

    -- turned off for instance
    FeatureLegalHoldDisabledPermanently -> do
      setLegalHoldInternal expect4xx tid FeatureStatusEnabled

    -- turned off but for whitelisted teams with implicit consent
    FeatureLegalHoldWhitelistTeamsAndImplicitConsent -> do
      setLegalHoldInternal expect4xx tid FeatureStatusEnabled

putLegalHoldInternal :: HasCallStack => (Request -> Request) -> TeamId -> FeatureStatus -> TestM ()
putLegalHoldInternal expectation tid =
  void
    . putTeamFeatureFlagInternal @LegalholdConfig expectation tid
    . (\st -> WithStatusNoLock st LegalholdConfig FeatureTTLUnlimited)

patchLegalHoldInternal :: HasCallStack => (Request -> Request) -> TeamId -> FeatureStatus -> TestM ()
patchLegalHoldInternal expectation tid status = void $ patchFeatureStatusInternalWithMod @LegalholdConfig expectation tid (withStatus' (Just status) Nothing Nothing (Just FeatureTTLUnlimited))

testSearchVisibility :: TestM ()
testSearchVisibility = do
  let getTeamSearchVisibility :: TeamId -> UserId -> FeatureStatus -> TestM ()
      getTeamSearchVisibility teamid uid expected = do
        g <- viewGalley
        getTeamSearchVisibilityAvailable g uid teamid !!! do
          statusCode === const 200
          responseJsonEither === const (Right (WithStatusNoLock expected SearchVisibilityAvailableConfig FeatureTTLUnlimited))

  let getTeamSearchVisibilityInternal :: TeamId -> FeatureStatus -> TestM ()
      getTeamSearchVisibilityInternal teamid expected = do
        g <- viewGalley
        getTeamSearchVisibilityAvailableInternal g teamid !!! do
          statusCode === const 200
          responseJsonEither === const (Right (WithStatusNoLock expected SearchVisibilityAvailableConfig FeatureTTLUnlimited))

  let getTeamSearchVisibilityFeatureConfig :: UserId -> FeatureStatus -> TestM ()
      getTeamSearchVisibilityFeatureConfig uid expected = do
        actual <- Util.getFeatureConfig @SearchVisibilityAvailableConfig uid
        liftIO $ wsStatus actual @?= expected

  let setTeamSearchVisibilityInternal :: TeamId -> FeatureStatus -> TestM ()
      setTeamSearchVisibilityInternal teamid val = do
        g <- viewGalley
        putTeamSearchVisibilityAvailableInternal g teamid val

  (owner, tid, [member]) <- createBindingTeamWithNMembers 1
  nonMember <- randomUser

  assertFlagForbidden $ getTeamFeatureFlag @SearchVisibilityAvailableConfig nonMember tid

  withCustomSearchFeature FeatureTeamSearchVisibilityUnavailableByDefault $ do
    getTeamSearchVisibility tid owner FeatureStatusDisabled
    getTeamSearchVisibilityInternal tid FeatureStatusDisabled
    getTeamSearchVisibilityFeatureConfig member FeatureStatusDisabled

    setTeamSearchVisibilityInternal tid FeatureStatusEnabled
    getTeamSearchVisibility tid owner FeatureStatusEnabled
    getTeamSearchVisibilityInternal tid FeatureStatusEnabled
    getTeamSearchVisibilityFeatureConfig member FeatureStatusEnabled

    setTeamSearchVisibilityInternal tid FeatureStatusDisabled
    getTeamSearchVisibility tid owner FeatureStatusDisabled
    getTeamSearchVisibilityInternal tid FeatureStatusDisabled
    getTeamSearchVisibilityFeatureConfig member FeatureStatusDisabled

  (owner2, tid2, team2member : _) <- createBindingTeamWithNMembers 1

  withCustomSearchFeature FeatureTeamSearchVisibilityAvailableByDefault $ do
    getTeamSearchVisibility tid2 owner2 FeatureStatusEnabled
    getTeamSearchVisibilityInternal tid2 FeatureStatusEnabled
    getTeamSearchVisibilityFeatureConfig team2member FeatureStatusEnabled

    setTeamSearchVisibilityInternal tid2 FeatureStatusDisabled
    getTeamSearchVisibility tid2 owner2 FeatureStatusDisabled
    getTeamSearchVisibilityInternal tid2 FeatureStatusDisabled
    getTeamSearchVisibilityFeatureConfig team2member FeatureStatusDisabled

    setTeamSearchVisibilityInternal tid2 FeatureStatusEnabled
    getTeamSearchVisibility tid2 owner2 FeatureStatusEnabled
    getTeamSearchVisibilityInternal tid2 FeatureStatusEnabled
    getTeamSearchVisibilityFeatureConfig team2member FeatureStatusEnabled

getClassifiedDomains ::
  (HasCallStack, HasGalley m, MonadIO m, MonadHttp m, MonadCatch m) =>
  UserId ->
  TeamId ->
  WithStatusNoLock ClassifiedDomainsConfig ->
  m ()
getClassifiedDomains member tid =
  assertFlagWithConfig @ClassifiedDomainsConfig $
    getTeamFeatureFlag @ClassifiedDomainsConfig member tid

getClassifiedDomainsInternal ::
  (HasCallStack, HasGalley m, MonadIO m, MonadHttp m, MonadCatch m) =>
  TeamId ->
  WithStatusNoLock ClassifiedDomainsConfig ->
  m ()
getClassifiedDomainsInternal tid =
  assertFlagWithConfig @ClassifiedDomainsConfig $
    getTeamFeatureFlagInternal @ClassifiedDomainsConfig tid

testClassifiedDomainsEnabled :: TestM ()
testClassifiedDomainsEnabled = do
  (_owner, tid, member : _) <- createBindingTeamWithNMembers 1
  let expected =
        WithStatusNoLock FeatureStatusEnabled (ClassifiedDomainsConfig [Domain "example.com"]) FeatureTTLUnlimited

  let getClassifiedDomainsFeatureConfig ::
        (HasCallStack, HasGalley m, MonadIO m, MonadHttp m, MonadCatch m) =>
        UserId ->
        WithStatusNoLock ClassifiedDomainsConfig ->
        m ()
      getClassifiedDomainsFeatureConfig uid expected' = do
        result <- Util.getFeatureConfig @ClassifiedDomainsConfig uid
        liftIO $ wsStatus result @?= wssStatus expected'
        liftIO $ wsConfig result @?= wssConfig expected'

  getClassifiedDomains member tid expected
  getClassifiedDomainsInternal tid expected
  getClassifiedDomainsFeatureConfig member expected

testClassifiedDomainsDisabled :: TestM ()
testClassifiedDomainsDisabled = do
  (_owner, tid, member : _) <- createBindingTeamWithNMembers 1
  let expected =
        WithStatusNoLock FeatureStatusDisabled (ClassifiedDomainsConfig []) FeatureTTLUnlimited

  let getClassifiedDomainsFeatureConfig ::
        (HasCallStack, HasGalley m, MonadIO m, MonadHttp m, MonadCatch m) =>
        UserId ->
        WithStatusNoLock ClassifiedDomainsConfig ->
        m ()
      getClassifiedDomainsFeatureConfig uid expected' = do
        result <- Util.getFeatureConfig @ClassifiedDomainsConfig uid
        liftIO $ wsStatus result @?= wssStatus expected'
        liftIO $ wsConfig result @?= wssConfig expected'

  let classifiedDomainsDisabled opts =
        opts
          & over
            (optSettings . setFeatureFlags . flagClassifiedDomains)
            (\(ImplicitLockStatus s) -> ImplicitLockStatus (s & setStatus FeatureStatusDisabled & setConfig (ClassifiedDomainsConfig [])))
  withSettingsOverrides classifiedDomainsDisabled $ do
    getClassifiedDomains member tid expected
    getClassifiedDomainsInternal tid expected
    getClassifiedDomainsFeatureConfig member expected

testSimpleFlag ::
  forall cfg.
  ( HasCallStack,
    Typeable cfg,
    IsFeatureConfig cfg,
    KnownSymbol (FeatureSymbol cfg),
    FeatureTrivialConfig cfg,
    ToSchema cfg,
    FromJSON (WithStatusNoLock cfg)
  ) =>
  FeatureStatus ->
  TestM ()
testSimpleFlag defaultValue = testSimpleFlagTTL @cfg defaultValue FeatureTTLUnlimited

testSimpleFlagTTLOverride ::
  forall cfg.
  ( HasCallStack,
    Typeable cfg,
    IsFeatureConfig cfg,
    KnownSymbol (FeatureSymbol cfg),
    FeatureTrivialConfig cfg,
    ToSchema cfg,
    FromJSON (WithStatusNoLock cfg)
  ) =>
  FeatureStatus ->
  FeatureTTL ->
  FeatureTTL ->
  TestM ()
testSimpleFlagTTLOverride defaultValue ttl ttlAfter = do
  (_owner, tid, member : _) <- createBindingTeamWithNMembers 1
  nonMember <- randomUser

  let getFlag :: HasCallStack => FeatureStatus -> TestM ()
      getFlag expected = eventually $ do
        flip (assertFlagNoConfig @cfg) expected $ getTeamFeatureFlag @cfg member tid

      getFeatureConfig :: HasCallStack => FeatureStatus -> FeatureTTL -> TestM ()
      getFeatureConfig expectedStatus expectedTtl = eventually $ do
        actual <- Util.getFeatureConfig @cfg member
        liftIO $ wsStatus actual @?= expectedStatus
        liftIO $ checkTtl (wsTTL actual) expectedTtl

      getFlagInternal :: HasCallStack => FeatureStatus -> TestM ()
      getFlagInternal expected = eventually $ do
        flip (assertFlagNoConfig @cfg) expected $ getTeamFeatureFlagInternal @cfg tid

      setFlagInternal :: FeatureStatus -> FeatureTTL -> TestM ()
      setFlagInternal statusValue ttl' =
        void $ putTeamFeatureFlagInternalTTL @cfg expect2xx tid (WithStatusNoLock statusValue (trivialConfig @cfg) ttl')

      select :: PrepQuery R (Identity TeamId) (Identity (Maybe FeatureTTL))
      select = fromString "select ttl(conference_calling) from team_features where team_id = ?"

      assertUnlimited :: TestM ()
      assertUnlimited = do
        -- TTL should be NULL inside cassandra
        cassState <- view tsCass
        liftIO $ do
          storedTTL <- maybe Nothing runIdentity <$> Cql.runClient cassState (Cql.query1 select $ params LocalQuorum (Identity tid))
          storedTTL @?= Nothing

      assertLimited :: Word -> TestM ()
      assertLimited upper = do
        -- TTL should NOT be NULL inside cassandra
        cassState <- view tsCass
        liftIO $ do
          storedTTL <- maybe Nothing runIdentity <$> Cql.runClient cassState (Cql.query1 select $ params LocalQuorum (Identity tid))
          let check = case storedTTL of
                Nothing -> False
                Just FeatureTTLUnlimited -> False
                Just (FeatureTTLSeconds i) -> i <= upper
          unless check $ error ("expected ttl <= " <> show upper <> ", got " <> show storedTTL)

      checkTtl :: FeatureTTL -> FeatureTTL -> IO ()
      checkTtl (FeatureTTLSeconds actualTtl) (FeatureTTLSeconds expectedTtl) =
        assertBool
          ("expected the actual TTL to be greater than 0 and equal to or no more than 2 seconds less than " <> show expectedTtl <> ", but it was " <> show actualTtl)
          ( actualTtl > 0
              && actualTtl <= expectedTtl
              && abs (fromIntegral @Word @Int actualTtl - fromIntegral @Word @Int expectedTtl) <= 2
          )
      checkTtl FeatureTTLUnlimited FeatureTTLUnlimited = pure ()
      checkTtl FeatureTTLUnlimited _ = assertFailure "expected the actual TTL to be unlimited, but it was limited"
      checkTtl _ FeatureTTLUnlimited = assertFailure "expected the actual TTL to be limited, but it was unlimited"

      toMicros :: Word -> Int
      toMicros secs = fromIntegral secs * 1000000

  assertFlagForbidden $ getTeamFeatureFlag @cfg nonMember tid

  let otherValue = case defaultValue of
        FeatureStatusDisabled -> FeatureStatusEnabled
        FeatureStatusEnabled -> FeatureStatusDisabled

  -- Initial value should be the default value
  getFlag defaultValue
  getFlagInternal defaultValue
  getFeatureConfig defaultValue FeatureTTLUnlimited

  -- Setting should work
  setFlagInternal otherValue ttl
  getFlag otherValue
  getFeatureConfig otherValue ttl
  getFlagInternal otherValue

  case (ttl, ttlAfter) of
    (FeatureTTLSeconds d, FeatureTTLSeconds d') -> do
      assertLimited d -- TTL should be NULL after expiration.
      -- wait less than expiration, override and recheck.
      liftIO $ threadDelay (toMicros d `div` 2) -- waiting half of TTL
      setFlagInternal otherValue ttlAfter
      -- value is still correct
      getFlag otherValue

      liftIO $ threadDelay (toMicros d') -- waiting for new TTL
      getFlag defaultValue
      assertUnlimited -- TTL should be NULL after expiration.
    (FeatureTTLSeconds d, FeatureTTLUnlimited) -> do
      assertLimited d -- TTL should be NULL after expiration.
      -- wait less than expiration, override and recheck.
      liftIO $ threadDelay (fromIntegral d `div` 2) -- waiting half of TTL
      setFlagInternal otherValue ttlAfter
      -- value is still correct
      getFlag otherValue
      assertUnlimited
    (FeatureTTLUnlimited, FeatureTTLUnlimited) -> do
      assertUnlimited

      -- overriding in this case should have no effect.
      setFlagInternal otherValue ttl
      getFlag otherValue
      getFeatureConfig otherValue ttl
      getFlagInternal otherValue

      assertUnlimited
    (FeatureTTLUnlimited, FeatureTTLSeconds d) -> do
      assertUnlimited

      setFlagInternal otherValue ttlAfter
      getFlag otherValue
      getFeatureConfig otherValue ttlAfter
      getFlagInternal otherValue

      liftIO $ threadDelay (toMicros d) -- waiting it out
      -- value reverts back
      getFlag defaultValue
      -- TTL should be NULL inside cassandra
      assertUnlimited

  -- Clean up
  setFlagInternal defaultValue FeatureTTLUnlimited
  assertUnlimited
  getFlag defaultValue

testSimpleFlagTTL ::
  forall cfg.
  ( HasCallStack,
    Typeable cfg,
    IsFeatureConfig cfg,
    KnownSymbol (FeatureSymbol cfg),
    FeatureTrivialConfig cfg,
    ToSchema cfg,
    FromJSON (WithStatusNoLock cfg)
  ) =>
  FeatureStatus ->
  FeatureTTL ->
  TestM ()
testSimpleFlagTTL defaultValue ttl = do
  (_owner, tid, member : _) <- createBindingTeamWithNMembers 1
  nonMember <- randomUser

  let getFlag :: HasCallStack => FeatureStatus -> TestM ()
      getFlag expected =
        flip (assertFlagNoConfig @cfg) expected $ getTeamFeatureFlag @cfg member tid

      getFeatureConfig :: HasCallStack => FeatureStatus -> TestM ()
      getFeatureConfig expected = do
        actual <- Util.getFeatureConfig @cfg member
        liftIO $ wsStatus actual @?= expected

      getFlagInternal :: HasCallStack => FeatureStatus -> TestM ()
      getFlagInternal expected =
        flip (assertFlagNoConfig @cfg) expected $ getTeamFeatureFlagInternal @cfg tid

      setFlagInternal :: FeatureStatus -> FeatureTTL -> TestM ()
      setFlagInternal statusValue ttl' =
        void $ putTeamFeatureFlagInternalTTL @cfg expect2xx tid (WithStatusNoLock statusValue (trivialConfig @cfg) ttl')

      select :: PrepQuery R (Identity TeamId) (Identity (Maybe FeatureTTL))
      select = fromString "select ttl(conference_calling) from team_features where team_id = ?"

      assertUnlimited :: TestM ()
      assertUnlimited = do
        -- TTL should be NULL inside cassandra
        cassState <- view tsCass
        liftIO $ do
          storedTTL <- maybe Nothing runIdentity <$> Cql.runClient cassState (Cql.query1 select $ params LocalQuorum (Identity tid))
          storedTTL @?= Nothing

      assertLimited :: Word -> TestM ()
      assertLimited upper = do
        -- TTL should NOT be NULL inside cassandra
        cassState <- view tsCass
        liftIO $ do
          storedTTL <- maybe Nothing runIdentity <$> Cql.runClient cassState (Cql.query1 select $ params LocalQuorum (Identity tid))
          let check = case storedTTL of
                Nothing -> False
                Just FeatureTTLUnlimited -> False
                Just (FeatureTTLSeconds i) -> i <= upper
          unless check $ error ("expected ttl <= " <> show upper <> ", got " <> show storedTTL)

  assertFlagForbidden $ getTeamFeatureFlag @cfg nonMember tid

  let otherValue = case defaultValue of
        FeatureStatusDisabled -> FeatureStatusEnabled
        FeatureStatusEnabled -> FeatureStatusDisabled

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
        wsAssertFeatureTrivialConfigUpdate @cfg otherValue ttl
  getFlag otherValue
  getFeatureConfig otherValue
  getFlagInternal otherValue

  case ttl of
    FeatureTTLSeconds d -> do
      -- should revert back after TTL expires
      assertLimited d
      liftIO $ threadDelay (fromIntegral d * 1000000)
      assertUnlimited
      getFlag defaultValue
    FeatureTTLUnlimited -> do
      -- TTL should be NULL inside cassandra
      assertUnlimited

  -- Clean up
  setFlagInternal defaultValue FeatureTTLUnlimited
  getFlag defaultValue

testSimpleFlagWithLockStatus ::
  forall cfg.
  ( HasCallStack,
    Typeable cfg,
    Eq cfg,
    Show cfg,
    FeatureTrivialConfig cfg,
    IsFeatureConfig cfg,
    KnownSymbol (FeatureSymbol cfg),
    ToSchema cfg,
    ToJSON (WithStatusNoLock cfg)
  ) =>
  FeatureStatus ->
  LockStatus ->
  TestM ()
testSimpleFlagWithLockStatus defaultStatus defaultLockStatus = do
  galley <- viewGalley
  (owner, tid, member : _) <- createBindingTeamWithNMembers 1
  nonMember <- randomUser

  let getFlag :: HasCallStack => FeatureStatus -> LockStatus -> TestM ()
      getFlag expectedStatus expectedLockStatus = do
        let flag = getTeamFeatureFlag @cfg member tid
        assertFlagNoConfigWithLockStatus @cfg flag expectedStatus expectedLockStatus

      getFeatureConfig :: HasCallStack => FeatureStatus -> LockStatus -> TestM ()
      getFeatureConfig expectedStatus expectedLockStatus = do
        actual <- Util.getFeatureConfig @cfg member
        liftIO $ wsStatus actual @?= expectedStatus
        liftIO $ wsLockStatus actual @?= expectedLockStatus

      getFlagInternal :: HasCallStack => FeatureStatus -> LockStatus -> TestM ()
      getFlagInternal expectedStatus expectedLockStatus = do
        let flag = getTeamFeatureFlagInternal @cfg tid
        assertFlagNoConfigWithLockStatus @cfg flag expectedStatus expectedLockStatus

      getFlags expectedStatus expectedLockStatus = do
        getFlag expectedStatus expectedLockStatus
        getFeatureConfig expectedStatus expectedLockStatus
        getFlagInternal expectedStatus expectedLockStatus

      setFlagWithGalley :: FeatureStatus -> TestM ()
      setFlagWithGalley statusValue =
        putTeamFeatureFlagWithGalley @cfg galley owner tid (WithStatusNoLock statusValue (trivialConfig @cfg) FeatureTTLUnlimited)
          !!! statusCode
            === const 200

      assertSetStatusForbidden :: FeatureStatus -> TestM ()
      assertSetStatusForbidden statusValue =
        putTeamFeatureFlagWithGalley @cfg galley owner tid (WithStatusNoLock statusValue (trivialConfig @cfg) FeatureTTLUnlimited)
          !!! statusCode
            === const 409

      setLockStatus :: LockStatus -> TestM ()
      setLockStatus lockStatus =
        Util.setLockStatusInternal @cfg galley tid lockStatus
          !!! statusCode
            === const 200

  assertFlagForbidden $ getTeamFeatureFlag @cfg nonMember tid

  let otherStatus = case defaultStatus of
        FeatureStatusDisabled -> FeatureStatusEnabled
        FeatureStatusEnabled -> FeatureStatusDisabled

  -- Initial status and lock status should be the defaults
  getFlags defaultStatus defaultLockStatus

  -- unlock feature if it is locked
  when (defaultLockStatus == LockStatusLocked) $ setLockStatus LockStatusUnlocked

  -- setting should work
  cannon <- view tsCannon
  -- should receive an event
  WS.bracketR cannon member $ \ws -> do
    setFlagWithGalley otherStatus
    void . liftIO $
      WS.assertMatch (5 # Second) ws $
        wsAssertFeatureConfigWithLockStatusUpdate @cfg otherStatus LockStatusUnlocked

  getFlags otherStatus LockStatusUnlocked

  -- lock feature
  setLockStatus LockStatusLocked
  -- feature status should now be the default again
  getFlags defaultStatus LockStatusLocked
  assertSetStatusForbidden defaultStatus
  -- unlock feature
  setLockStatus LockStatusUnlocked
  -- feature status should be the previously set value
  getFlags otherStatus LockStatusUnlocked

  -- clean up
  setFlagWithGalley defaultStatus
  setLockStatus defaultLockStatus
  getFlags defaultStatus defaultLockStatus

testSelfDeletingMessages :: TestM ()
testSelfDeletingMessages = do
  defLockStatus :: LockStatus <-
    view
      ( tsGConf
          . optSettings
          . setFeatureFlags
          . flagSelfDeletingMessages
          . unDefaults
          . to wsLockStatus
      )

  -- personal users
  let settingWithoutLockStatus :: FeatureStatus -> Int32 -> WithStatusNoLock SelfDeletingMessagesConfig
      settingWithoutLockStatus stat tout =
        WithStatusNoLock
          stat
          (SelfDeletingMessagesConfig tout)
          FeatureTTLUnlimited
      settingWithLockStatus :: FeatureStatus -> Int32 -> LockStatus -> WithStatus SelfDeletingMessagesConfig
      settingWithLockStatus stat tout lockStatus =
        withStatus
          stat
          lockStatus
          (SelfDeletingMessagesConfig tout)
          FeatureTTLUnlimited

  personalUser <- randomUser
  do
    result <- Util.getFeatureConfig @SelfDeletingMessagesConfig personalUser
    liftIO $ result @?= settingWithLockStatus FeatureStatusEnabled 0 defLockStatus

  -- team users
  galley <- viewGalley
  (owner, tid, []) <- createBindingTeamWithNMembers 0

  let checkSet :: FeatureStatus -> Int32 -> Int -> TestM ()
      checkSet stat tout expectedStatusCode =
        do
          putTeamFeatureFlagInternal @SelfDeletingMessagesConfig
            galley
            tid
            (settingWithoutLockStatus stat tout)
          !!! statusCode
            === const expectedStatusCode

      -- internal, public (/team/:tid/features), and team-agnostic (/feature-configs).
      checkGet :: HasCallStack => FeatureStatus -> Int32 -> LockStatus -> TestM ()
      checkGet stat tout lockStatus = do
        let expected = settingWithLockStatus stat tout lockStatus
        forM_
          [ getTeamFeatureFlagInternal @SelfDeletingMessagesConfig tid,
            getTeamFeatureFlagWithGalley @SelfDeletingMessagesConfig galley owner tid
          ]
          (!!! responseJsonEither === const (Right expected))
        result <- Util.getFeatureConfig @SelfDeletingMessagesConfig owner
        liftIO $ result @?= expected

      checkSetLockStatus :: HasCallStack => LockStatus -> TestM ()
      checkSetLockStatus status =
        do
          Util.setLockStatusInternal @SelfDeletingMessagesConfig galley tid status
          !!! statusCode
            === const 200

  -- test that the default lock status comes from `galley.yaml`.
  -- use this to change `galley.integration.yaml` locally and manually test that conf file
  -- parsing works as expected.
  checkGet FeatureStatusEnabled 0 defLockStatus

  case defLockStatus of
    LockStatusLocked -> do
      checkSet FeatureStatusDisabled 0 409
    LockStatusUnlocked -> do
      checkSet FeatureStatusDisabled 0 200
      checkGet FeatureStatusDisabled 0 LockStatusUnlocked
      checkSet FeatureStatusEnabled 0 200
      checkGet FeatureStatusEnabled 0 LockStatusUnlocked

  -- now don't worry about what's in the config, write something to cassandra, and test with that.
  checkSetLockStatus LockStatusLocked
  checkGet FeatureStatusEnabled 0 LockStatusLocked
  checkSet FeatureStatusDisabled 0 409
  checkGet FeatureStatusEnabled 0 LockStatusLocked
  checkSet FeatureStatusEnabled 30 409
  checkGet FeatureStatusEnabled 0 LockStatusLocked
  checkSetLockStatus LockStatusUnlocked
  checkGet FeatureStatusEnabled 0 LockStatusUnlocked
  checkSet FeatureStatusDisabled 0 200
  checkGet FeatureStatusDisabled 0 LockStatusUnlocked
  checkSet FeatureStatusEnabled 30 200
  checkGet FeatureStatusEnabled 30 LockStatusUnlocked
  checkSet FeatureStatusDisabled 30 200
  checkGet FeatureStatusDisabled 30 LockStatusUnlocked
  checkSetLockStatus LockStatusLocked
  checkGet FeatureStatusEnabled 0 LockStatusLocked
  checkSet FeatureStatusEnabled 50 409
  checkSetLockStatus LockStatusUnlocked
  checkGet FeatureStatusDisabled 30 LockStatusUnlocked

testGuestLinksInternal :: TestM ()
testGuestLinksInternal = do
  galley <- viewGalley
  testGuestLinks
    (const $ getTeamFeatureFlagInternal @GuestLinksConfig)
    (const $ putTeamFeatureFlagInternal @GuestLinksConfig galley)
    (Util.setLockStatusInternal @GuestLinksConfig galley)

testGuestLinksPublic :: TestM ()
testGuestLinksPublic = do
  galley <- viewGalley
  testGuestLinks
    (getTeamFeatureFlagWithGalley @GuestLinksConfig galley)
    (putTeamFeatureFlagWithGalley @GuestLinksConfig galley)
    (Util.setLockStatusInternal @GuestLinksConfig galley)

testGuestLinks ::
  (UserId -> TeamId -> TestM ResponseLBS) ->
  (UserId -> TeamId -> WithStatusNoLock GuestLinksConfig -> TestM ResponseLBS) ->
  (TeamId -> LockStatus -> TestM ResponseLBS) ->
  TestM ()
testGuestLinks getStatus putStatus setLockStatusInternal = do
  (owner, tid, []) <- createBindingTeamWithNMembers 0
  let checkGet :: HasCallStack => FeatureStatus -> LockStatus -> TestM ()
      checkGet status lock =
        getStatus owner tid !!! do
          statusCode === const 200
          responseJsonEither === const (Right (withStatus status lock GuestLinksConfig FeatureTTLUnlimited))

      checkSet :: HasCallStack => FeatureStatus -> Int -> TestM ()
      checkSet status expectedStatusCode =
        putStatus owner tid (WithStatusNoLock status GuestLinksConfig FeatureTTLUnlimited) !!! statusCode === const expectedStatusCode

      checkSetLockStatusInternal :: HasCallStack => LockStatus -> TestM ()
      checkSetLockStatusInternal lockStatus =
        setLockStatusInternal tid lockStatus !!! statusCode === const 200

  checkGet FeatureStatusEnabled LockStatusUnlocked
  checkSet FeatureStatusDisabled 200
  checkGet FeatureStatusDisabled LockStatusUnlocked
  checkSet FeatureStatusEnabled 200
  checkGet FeatureStatusEnabled LockStatusUnlocked
  checkSet FeatureStatusDisabled 200
  checkGet FeatureStatusDisabled LockStatusUnlocked
  -- when locks status is locked the team default feature status should be returned
  -- and the team feature status can not be changed
  checkSetLockStatusInternal LockStatusLocked
  checkGet FeatureStatusEnabled LockStatusLocked
  checkSet FeatureStatusDisabled 409
  -- when lock status is unlocked again the previously set feature status is restored
  checkSetLockStatusInternal LockStatusUnlocked
  checkGet FeatureStatusDisabled LockStatusUnlocked

-- | Call 'GET /teams/:tid/features' and 'GET /feature-configs', and check if all
-- features are there.
testAllFeatures :: TestM ()
testAllFeatures = do
  defLockStatus :: LockStatus <-
    view
      ( tsGConf
          . optSettings
          . setFeatureFlags
          . flagSelfDeletingMessages
          . unDefaults
          . to wsLockStatus
      )

  (_owner, tid, member : _) <- createBindingTeamWithNMembers 1
  getAllTeamFeatures member tid !!! do
    statusCode === const 200
    responseJsonMaybe === const (Just (expected FeatureStatusEnabled defLockStatus {- determined by default in galley -}))

  -- This block catches potential errors in the logic that reverts to default if there is a distinction made between
  -- 1. there is no row for a team_id in galley.team_features
  -- 2. there is a row for team_id in galley.team_features but the feature has a no entry (null value)
  galley <- viewGalley
  -- this sets the guest links config to its default value thereby creating a row for the team in galley.team_features
  putTeamFeatureFlagInternal @GuestLinksConfig galley tid (WithStatusNoLock FeatureStatusEnabled GuestLinksConfig FeatureTTLUnlimited)
    !!! statusCode
      === const 200
  getAllTeamFeatures member tid !!! do
    statusCode === const 200
    responseJsonMaybe === const (Just (expected FeatureStatusEnabled defLockStatus {- determined by default in galley -}))

  getAllTeamFeaturesPersonal member !!! do
    statusCode === const 200
    responseJsonMaybe === const (Just (expected FeatureStatusEnabled defLockStatus {- determined by default in galley -}))

  randomPersonalUser <- randomUser
  getAllTeamFeaturesPersonal randomPersonalUser !!! do
    statusCode === const 200
    responseJsonMaybe === const (Just (expected FeatureStatusEnabled defLockStatus {- determined by 'getAfcConferenceCallingDefNew' in brig -}))
  where
    expected confCalling lockStateSelfDeleting =
      AllFeatureConfigs
        { afcLegalholdStatus = withStatus FeatureStatusDisabled LockStatusUnlocked LegalholdConfig FeatureTTLUnlimited,
          afcSSOStatus = withStatus FeatureStatusDisabled LockStatusUnlocked SSOConfig FeatureTTLUnlimited,
          afcTeamSearchVisibilityAvailable = withStatus FeatureStatusDisabled LockStatusUnlocked SearchVisibilityAvailableConfig FeatureTTLUnlimited,
          afcValidateSAMLEmails = withStatus FeatureStatusEnabled LockStatusUnlocked ValidateSAMLEmailsConfig FeatureTTLUnlimited,
          afcDigitalSignatures = withStatus FeatureStatusDisabled LockStatusUnlocked DigitalSignaturesConfig FeatureTTLUnlimited,
          afcAppLock = withStatus FeatureStatusEnabled LockStatusUnlocked (AppLockConfig (EnforceAppLock False) (60 :: Int32)) FeatureTTLUnlimited,
          afcFileSharing = withStatus FeatureStatusEnabled LockStatusUnlocked FileSharingConfig FeatureTTLUnlimited,
          afcClassifiedDomains = withStatus FeatureStatusEnabled LockStatusUnlocked (ClassifiedDomainsConfig [Domain "example.com"]) FeatureTTLUnlimited,
          afcConferenceCalling = withStatus confCalling LockStatusUnlocked ConferenceCallingConfig FeatureTTLUnlimited,
          afcSelfDeletingMessages = withStatus FeatureStatusEnabled lockStateSelfDeleting (SelfDeletingMessagesConfig 0) FeatureTTLUnlimited,
          afcGuestLink = withStatus FeatureStatusEnabled LockStatusUnlocked GuestLinksConfig FeatureTTLUnlimited,
          afcSndFactorPasswordChallenge = withStatus FeatureStatusDisabled LockStatusLocked SndFactorPasswordChallengeConfig FeatureTTLUnlimited,
          afcMLS = withStatus FeatureStatusDisabled LockStatusUnlocked (MLSConfig [] ProtocolProteusTag [MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519] MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519) FeatureTTLUnlimited,
          afcSearchVisibilityInboundConfig = withStatus FeatureStatusDisabled LockStatusUnlocked SearchVisibilityInboundConfig FeatureTTLUnlimited,
          afcExposeInvitationURLsToTeamAdmin = withStatus FeatureStatusDisabled LockStatusLocked ExposeInvitationURLsToTeamAdminConfig FeatureTTLUnlimited,
          afcOutlookCalIntegration = withStatus FeatureStatusDisabled LockStatusLocked OutlookCalIntegrationConfig FeatureTTLUnlimited,
          afcMlsE2EId = withStatus FeatureStatusDisabled LockStatusUnlocked (MlsE2EIdConfig Nothing) FeatureTTLUnlimited
        }

testFeatureConfigConsistency :: TestM ()
testFeatureConfigConsistency = do
  (_owner, tid, member : _) <- createBindingTeamWithNMembers 1

  allFeaturesRes <- getAllFeatureConfigs member >>= parseObjectKeys

  allTeamFeaturesRes <- getAllTeamFeatures member tid >>= parseObjectKeys

  unless (allTeamFeaturesRes `Set.isSubsetOf` allFeaturesRes) $
    liftIO $
      expectationFailure (show allTeamFeaturesRes <> " is not a subset of " <> show allFeaturesRes)
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
  (_owner, tid, _) <- createBindingTeamWithNMembers 1

  let getFlagInternal :: HasCallStack => FeatureStatus -> TestM ()
      getFlagInternal expected =
        flip (assertFlagNoConfig @SearchVisibilityInboundConfig) expected $ getTeamFeatureFlagInternal @SearchVisibilityInboundConfig tid

      setFlagInternal :: FeatureStatus -> TestM ()
      setFlagInternal statusValue =
        void $ putTeamFeatureFlagInternal @SearchVisibilityInboundConfig expect2xx tid (WithStatusNoLock statusValue SearchVisibilityInboundConfig FeatureTTLUnlimited)

  let otherValue = case defaultValue of
        FeatureStatusDisabled -> FeatureStatusEnabled
        FeatureStatusEnabled -> FeatureStatusDisabled

  -- Initial value should be the default value
  getFlagInternal defaultValue
  setFlagInternal otherValue
  getFlagInternal otherValue

testFeatureNoConfigMultiSearchVisibilityInbound :: TestM ()
testFeatureNoConfigMultiSearchVisibilityInbound = do
  (_owner1, team1, _) <- createBindingTeamWithNMembers 0
  (_owner2, team2, _) <- createBindingTeamWithNMembers 0

  let setFlagInternal :: TeamId -> FeatureStatus -> TestM ()
      setFlagInternal tid statusValue =
        void $ putTeamFeatureFlagInternal @SearchVisibilityInboundConfig expect2xx tid (WithStatusNoLock statusValue SearchVisibilityInboundConfig FeatureTTLUnlimited)

  setFlagInternal team2 FeatureStatusEnabled

  r <-
    getFeatureStatusMulti @SearchVisibilityInboundConfig (Multi.TeamFeatureNoConfigMultiRequest [team1, team2])
      <!! statusCode
        === const 200

  Multi.TeamFeatureNoConfigMultiResponse teamsStatuses :: Multi.TeamFeatureNoConfigMultiResponse SearchVisibilityInboundConfig <- responseJsonError r

  liftIO $ do
    length teamsStatuses @?= 2

    Multi.TeamStatus _ team1Status <- assertOne (filter ((== team1) . Multi.team) teamsStatuses)
    team1Status @?= FeatureStatusDisabled

    Multi.TeamStatus _ team2Status <- assertOne (filter ((== team2) . Multi.team) teamsStatuses)
    team2Status @?= FeatureStatusEnabled

testNonTrivialConfigNoTTL ::
  forall cfg.
  ( Typeable cfg,
    Eq cfg,
    Show cfg,
    ToSchema cfg,
    IsFeatureConfig cfg,
    KnownSymbol (FeatureSymbol cfg),
    Arbitrary (WithStatus cfg)
  ) =>
  WithStatus cfg ->
  TestM ()
testNonTrivialConfigNoTTL defaultCfg = do
  (owner, tid, member : _) <- createBindingTeamWithNMembers 1
  nonMember <- randomUser

  galley <- viewGalley
  cannon <- view tsCannon

  let getForTeam :: HasCallStack => WithStatusNoLock cfg -> TestM ()
      getForTeam expected =
        flip assertFlagWithConfig expected $ getTeamFeatureFlag @cfg member tid

      getForTeamInternal :: HasCallStack => WithStatusNoLock cfg -> TestM ()
      getForTeamInternal expected =
        flip assertFlagWithConfig expected $ getTeamFeatureFlagInternal @cfg tid

      getForUser :: HasCallStack => WithStatusNoLock cfg -> TestM ()
      getForUser expected = do
        result <- Util.getFeatureConfig @cfg member
        liftIO $ wsStatus result @?= wssStatus expected
        liftIO $ wsConfig result @?= wssConfig expected

      getViaEndpoints :: HasCallStack => WithStatusNoLock cfg -> TestM ()
      getViaEndpoints expected = do
        getForTeam expected
        getForTeamInternal expected
        getForUser expected

      setForTeam :: HasCallStack => WithStatusNoLock cfg -> TestM ()
      setForTeam wsnl =
        putTeamFeatureFlagWithGalley @cfg galley owner tid wsnl
          !!! statusCode
            === const 200

      setForTeamInternal :: HasCallStack => WithStatusNoLock cfg -> TestM ()
      setForTeamInternal wsnl =
        void $ putTeamFeatureFlagInternal @cfg expect2xx tid wsnl
      setLockStatus :: LockStatus -> TestM ()
      setLockStatus lockStatus =
        Util.setLockStatusInternal @cfg galley tid lockStatus
          !!! statusCode
            === const 200

  assertFlagForbidden $ getTeamFeatureFlag @cfg nonMember tid

  getViaEndpoints (forgetLock defaultCfg)

  -- unlock feature
  setLockStatus LockStatusUnlocked

  config2 <- liftIO $ generate arbitrary <&> (forgetLock . setTTL FeatureTTLUnlimited)
  config3 <- liftIO $ generate arbitrary <&> (forgetLock . setTTL FeatureTTLUnlimited)

  WS.bracketR cannon member $ \ws -> do
    setForTeam config2
    void . liftIO $
      WS.assertMatch (5 # Second) ws $
        wsAssertFeatureConfigUpdate @cfg config2 LockStatusUnlocked
  getViaEndpoints config2

  WS.bracketR cannon member $ \ws -> do
    setForTeamInternal config3
    void . liftIO $
      WS.assertMatch (5 # Second) ws $
        wsAssertFeatureConfigUpdate @cfg config3 LockStatusUnlocked
  getViaEndpoints config3

  -- lock the feature
  setLockStatus LockStatusLocked
  -- feature status should now be the default again
  getViaEndpoints (forgetLock defaultCfg)
  -- unlock feature
  setLockStatus LockStatusUnlocked
  -- feature status should be the previously set value
  getViaEndpoints config3

testMLS :: TestM ()
testMLS = do
  (owner, tid, member : _) <- createBindingTeamWithNMembers 1

  galley <- viewGalley
  cannon <- view tsCannon

  let getForTeam :: HasCallStack => WithStatusNoLock MLSConfig -> TestM ()
      getForTeam expected =
        flip assertFlagWithConfig expected $ getTeamFeatureFlag @MLSConfig member tid

      getForTeamInternal :: HasCallStack => WithStatusNoLock MLSConfig -> TestM ()
      getForTeamInternal expected =
        flip assertFlagWithConfig expected $ getTeamFeatureFlagInternal @MLSConfig tid

      getForUser :: HasCallStack => WithStatusNoLock MLSConfig -> TestM ()
      getForUser expected = do
        result <- Util.getFeatureConfig @MLSConfig member
        liftIO $ wsStatus result @?= wssStatus expected
        liftIO $ wsConfig result @?= wssConfig expected

      getViaEndpoints :: HasCallStack => WithStatusNoLock MLSConfig -> TestM ()
      getViaEndpoints expected = do
        getForTeam expected
        getForTeamInternal expected
        getForUser expected

      setForTeam :: HasCallStack => WithStatusNoLock MLSConfig -> TestM ()
      setForTeam wsnl =
        putTeamFeatureFlagWithGalley @MLSConfig galley owner tid wsnl
          !!! statusCode
            === const 200

      setForTeamInternal :: HasCallStack => WithStatusNoLock MLSConfig -> TestM ()
      setForTeamInternal wsnl =
        void $ putTeamFeatureFlagInternal @MLSConfig expect2xx tid wsnl

  let cipherSuite = MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519
  let defaultConfig =
        WithStatusNoLock
          FeatureStatusDisabled
          (MLSConfig [] ProtocolProteusTag [cipherSuite] cipherSuite)
          FeatureTTLUnlimited
  let config2 =
        WithStatusNoLock
          FeatureStatusEnabled
          (MLSConfig [member] ProtocolMLSTag [] cipherSuite)
          FeatureTTLUnlimited
  let config3 =
        WithStatusNoLock
          FeatureStatusDisabled
          (MLSConfig [] ProtocolMLSTag [cipherSuite] cipherSuite)
          FeatureTTLUnlimited

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

testExposeInvitationURLsToTeamAdminTeamIdInAllowList :: TestM ()
testExposeInvitationURLsToTeamAdminTeamIdInAllowList = do
  owner <- randomUser
  tid <- createBindingTeamInternal "foo" owner
  assertTeamActivate "create team" tid
  void $
    withSettingsOverrides (\opts -> opts & optSettings . setExposeInvitationURLsTeamAllowlist ?~ [tid]) $ do
      g <- viewGalley
      assertExposeInvitationURLsToTeamAdminConfigStatus owner tid FeatureStatusDisabled LockStatusUnlocked
      let enabled = WithStatusNoLock FeatureStatusEnabled ExposeInvitationURLsToTeamAdminConfig FeatureTTLUnlimited
      void $
        putTeamFeatureFlagWithGalley @ExposeInvitationURLsToTeamAdminConfig g owner tid enabled !!! do
          const 200 === statusCode
      assertExposeInvitationURLsToTeamAdminConfigStatus owner tid FeatureStatusEnabled LockStatusUnlocked

testExposeInvitationURLsToTeamAdminEmptyAllowList :: TestM ()
testExposeInvitationURLsToTeamAdminEmptyAllowList = do
  owner <- randomUser
  tid <- createBindingTeamInternal "foo" owner
  assertTeamActivate "create team" tid
  void $
    withSettingsOverrides (\opts -> opts & optSettings . setExposeInvitationURLsTeamAllowlist .~ Nothing) $ do
      g <- viewGalley
      assertExposeInvitationURLsToTeamAdminConfigStatus owner tid FeatureStatusDisabled LockStatusLocked
      let enabled = WithStatusNoLock FeatureStatusEnabled ExposeInvitationURLsToTeamAdminConfig FeatureTTLUnlimited
      void $
        putTeamFeatureFlagWithGalley @ExposeInvitationURLsToTeamAdminConfig g owner tid enabled !!! do
          const 409 === statusCode
      assertExposeInvitationURLsToTeamAdminConfigStatus owner tid FeatureStatusDisabled LockStatusLocked

-- | Ensure that the server config takes precedence over a saved team config.
--
-- In other words: When a team id is no longer in the
-- `setExposeInvitationURLsTeamAllowlist` the
-- `ExposeInvitationURLsToTeamAdminConfig` is always disabled (even tough it
-- might have been enabled before).
testExposeInvitationURLsToTeamAdminServerConfigTakesPrecedence :: TestM ()
testExposeInvitationURLsToTeamAdminServerConfigTakesPrecedence = do
  owner <- randomUser
  tid <- createBindingTeamInternal "foo" owner
  assertTeamActivate "create team" tid
  void $
    withSettingsOverrides (\opts -> opts & optSettings . setExposeInvitationURLsTeamAllowlist ?~ [tid]) $ do
      g <- viewGalley
      assertExposeInvitationURLsToTeamAdminConfigStatus owner tid FeatureStatusDisabled LockStatusUnlocked
      let enabled = WithStatusNoLock FeatureStatusEnabled ExposeInvitationURLsToTeamAdminConfig FeatureTTLUnlimited
      void $
        putTeamFeatureFlagWithGalley @ExposeInvitationURLsToTeamAdminConfig g owner tid enabled !!! do
          const 200 === statusCode
      assertExposeInvitationURLsToTeamAdminConfigStatus owner tid FeatureStatusEnabled LockStatusUnlocked
  void $
    withSettingsOverrides (\opts -> opts & optSettings . setExposeInvitationURLsTeamAllowlist .~ Nothing) $ do
      g <- viewGalley
      assertExposeInvitationURLsToTeamAdminConfigStatus owner tid FeatureStatusDisabled LockStatusLocked
      let enabled = WithStatusNoLock FeatureStatusEnabled ExposeInvitationURLsToTeamAdminConfig FeatureTTLUnlimited
      void $
        putTeamFeatureFlagWithGalley @ExposeInvitationURLsToTeamAdminConfig g owner tid enabled !!! do
          const 409 === statusCode
      assertExposeInvitationURLsToTeamAdminConfigStatus owner tid FeatureStatusDisabled LockStatusLocked

assertExposeInvitationURLsToTeamAdminConfigStatus :: UserId -> TeamId -> FeatureStatus -> LockStatus -> TestM ()
assertExposeInvitationURLsToTeamAdminConfigStatus owner tid fStatus lStatus = do
  g <- viewGalley
  Util.getTeamFeatureFlagWithGalley @ExposeInvitationURLsToTeamAdminConfig g owner tid !!! do
    const 200 === statusCode
    const (Right (withStatus fStatus lStatus ExposeInvitationURLsToTeamAdminConfig FeatureTTLUnlimited)) === responseJsonEither

assertFlagForbidden :: HasCallStack => TestM ResponseLBS -> TestM ()
assertFlagForbidden res = do
  res !!! do
    statusCode === const 403
    fmap label . responseJsonMaybe === const (Just "no-team-member")

assertFlagNoConfig ::
  forall cfg.
  ( HasCallStack,
    Typeable cfg,
    FromJSON (WithStatusNoLock cfg)
  ) =>
  TestM ResponseLBS ->
  FeatureStatus ->
  TestM ()
assertFlagNoConfig res expected = do
  res !!! do
    statusCode === const 200
    ( fmap wssStatus
        . responseJsonEither @(WithStatusNoLock cfg)
      )
      === const (Right expected)

assertFlagNoConfigWithLockStatus ::
  forall cfg.
  ( HasCallStack,
    Typeable cfg,
    FeatureTrivialConfig cfg,
    FromJSON (WithStatus cfg),
    Eq cfg,
    Show cfg
  ) =>
  TestM ResponseLBS ->
  FeatureStatus ->
  LockStatus ->
  TestM ()
assertFlagNoConfigWithLockStatus res expectedStatus expectedLockStatus = do
  res !!! do
    statusCode === const 200
    responseJsonEither @(WithStatus cfg)
      === const (Right (withStatus expectedStatus expectedLockStatus (trivialConfig @cfg) FeatureTTLUnlimited))

assertFlagWithConfig ::
  forall cfg m.
  ( HasCallStack,
    Eq cfg,
    ToSchema cfg,
    Show cfg,
    Typeable cfg,
    IsFeatureConfig cfg,
    MonadIO m,
    MonadCatch m
  ) =>
  m ResponseLBS ->
  WithStatusNoLock cfg ->
  m ()
assertFlagWithConfig response expected = do
  r <- response
  let rJson = responseJsonEither @(WithStatusNoLock cfg) r
  pure r !!! statusCode === const 200
  liftIO $ do
    fmap wssStatus rJson @?= (Right . wssStatus $ expected)
    fmap wssConfig rJson @?= (Right . wssConfig $ expected)

wsAssertFeatureTrivialConfigUpdate ::
  forall cfg.
  ( IsFeatureConfig cfg,
    KnownSymbol (FeatureSymbol cfg),
    FeatureTrivialConfig cfg,
    ToSchema cfg
  ) =>
  FeatureStatus ->
  FeatureTTL ->
  Notification ->
  IO ()
wsAssertFeatureTrivialConfigUpdate status ttl notification = do
  let e :: FeatureConfig.Event = List1.head (WS.unpackPayload notification)
  FeatureConfig._eventType e @?= FeatureConfig.Update
  FeatureConfig._eventFeatureName e @?= featureName @cfg
  FeatureConfig._eventData e
    @?= Aeson.toJSON
      (withStatus status (wsLockStatus (defFeatureStatus @cfg)) (trivialConfig @cfg) ttl)

wsAssertFeatureConfigWithLockStatusUpdate ::
  forall cfg.
  ( IsFeatureConfig cfg,
    ToSchema cfg,
    KnownSymbol (FeatureSymbol cfg),
    FeatureTrivialConfig cfg
  ) =>
  FeatureStatus ->
  LockStatus ->
  Notification ->
  IO ()
wsAssertFeatureConfigWithLockStatusUpdate status lockStatus notification = do
  let e :: FeatureConfig.Event = List1.head (WS.unpackPayload notification)
  FeatureConfig._eventType e @?= FeatureConfig.Update
  FeatureConfig._eventFeatureName e @?= (featureName @cfg)
  FeatureConfig._eventData e @?= Aeson.toJSON (withStatus status lockStatus (trivialConfig @cfg) FeatureTTLUnlimited)

wsAssertFeatureConfigUpdate ::
  forall cfg.
  ( KnownSymbol (FeatureSymbol cfg),
    ToJSON (WithStatus cfg)
  ) =>
  WithStatusNoLock cfg ->
  LockStatus ->
  Notification ->
  IO ()
wsAssertFeatureConfigUpdate config lockStatus notification = do
  let e :: FeatureConfig.Event = List1.head (WS.unpackPayload notification)
  FeatureConfig._eventType e @?= FeatureConfig.Update
  FeatureConfig._eventFeatureName e @?= featureName @cfg
  FeatureConfig._eventData e @?= Aeson.toJSON (withLockStatus lockStatus config)
