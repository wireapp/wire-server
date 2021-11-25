-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

import API.Util (HasGalley, withSettingsOverrides)
import qualified API.Util as Util
import qualified API.Util.TeamFeature as Util
import Bilge
import Bilge.Assert
import Control.Lens (over, to, view)
import Control.Monad.Catch (MonadCatch)
import Data.Aeson (FromJSON, ToJSON, object, (.=))
import qualified Data.Aeson as Aeson
import Data.ByteString.Conversion (toByteString')
import Data.Domain (Domain (..))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.Id
import Data.List1 (list1)
import qualified Data.List1 as List1
import Data.Schema (ToSchema)
import qualified Data.Set as Set
import Data.String.Conversions (cs)
import qualified Data.Text.Encoding as TE
import Data.Timeout (TimeoutUnit (Second), (#))
import Galley.Options (optSettings, setFeatureFlags)
import Galley.Types.Teams
import Gundeck.Types (Notification)
import Imports
import Network.Wai.Utilities (label)
import Test.Hspec (expectationFailure, shouldBe)
import Test.Tasty
import qualified Test.Tasty.Cannon as WS
import Test.Tasty.HUnit (assertFailure, (@?=))
import TestHelpers (test)
import TestSetup
import Wire.API.Event.FeatureConfig (EventData (EdFeatureWithoutConfigChanged))
import qualified Wire.API.Event.FeatureConfig as FeatureConfig
import Wire.API.Team.Feature (TeamFeatureName (..), TeamFeatureStatusValue (..))
import qualified Wire.API.Team.Feature as Public

tests :: IO TestSetup -> TestTree
tests s =
  testGroup "Feature Config API and Team Features API" $
    [ test s "SSO" testSSO,
      test s "LegalHold" testLegalHold,
      test s "SearchVisibility" testSearchVisibility,
      test s "DigitalSignatures" $ testSimpleFlag @'Public.WithoutLockStatus @'Public.TeamFeatureDigitalSignatures Public.TeamFeatureDisabled,
      test s "ValidateSAMLEmails" $ testSimpleFlag @'Public.WithoutLockStatus @'Public.TeamFeatureValidateSAMLEmails Public.TeamFeatureDisabled,
      test s "FileSharing" $ testSimpleFlag @'Public.WithoutLockStatus @'Public.TeamFeatureFileSharing Public.TeamFeatureEnabled,
      test s "Classified Domains (enabled)" testClassifiedDomainsEnabled,
      test s "Classified Domains (disabled)" testClassifiedDomainsDisabled,
      test s "All features" testAllFeatures,
      test s "Feature Configs / Team Features Consistency" testFeatureConfigConsistency,
      test s "ConferenceCalling" $ testSimpleFlag @'Public.WithoutLockStatus @'Public.TeamFeatureConferenceCalling Public.TeamFeatureEnabled,
      test s "SelfDeletingMessages" testSelfDeletingMessages
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
      getSSO = assertFlagNoConfig @'Public.WithoutLockStatus @'Public.TeamFeatureSSO $ Util.getTeamFeatureFlag Public.TeamFeatureSSO member tid
      getSSOFeatureConfig :: HasCallStack => Public.TeamFeatureStatusValue -> TestM ()
      getSSOFeatureConfig = assertFlagNoConfig @'Public.WithoutLockStatus @'Public.TeamFeatureSSO $ Util.getFeatureConfig Public.TeamFeatureSSO member
      getSSOInternal :: HasCallStack => Public.TeamFeatureStatusValue -> TestM ()
      getSSOInternal = assertFlagNoConfig @'Public.WithoutLockStatus @'Public.TeamFeatureSSO $ Util.getTeamFeatureFlagInternal Public.TeamFeatureSSO tid
      setSSOInternal :: HasCallStack => Public.TeamFeatureStatusValue -> TestM ()
      setSSOInternal = void . Util.putTeamFeatureFlagInternal @'Public.WithoutLockStatus @'Public.TeamFeatureSSO expect2xx tid . Public.TeamFeatureStatusNoConfig

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
      getLegalHold = assertFlagNoConfig @'Public.WithoutLockStatus @'Public.TeamFeatureLegalHold $ Util.getTeamFeatureFlag Public.TeamFeatureLegalHold member tid
      getLegalHoldInternal :: HasCallStack => Public.TeamFeatureStatusValue -> TestM ()
      getLegalHoldInternal = assertFlagNoConfig @'Public.WithoutLockStatus @'Public.TeamFeatureLegalHold $ Util.getTeamFeatureFlagInternal Public.TeamFeatureLegalHold tid
      getLegalHoldFeatureConfig = assertFlagNoConfig @'Public.WithoutLockStatus @'Public.TeamFeatureLegalHold $ Util.getFeatureConfig Public.TeamFeatureLegalHold member

      setLegalHoldInternal :: HasCallStack => Public.TeamFeatureStatusValue -> TestM ()
      setLegalHoldInternal = void . Util.putTeamFeatureFlagInternal @'Public.WithoutLockStatus @'Public.TeamFeatureLegalHold expect2xx tid . Public.TeamFeatureStatusNoConfig
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

  g <- view tsGalley
  let getTeamSearchVisibility ::
        (Monad m, MonadHttp m, MonadIO m, MonadCatch m, HasCallStack) =>
        TeamId ->
        Public.TeamFeatureStatusValue ->
        m ()
      getTeamSearchVisibility teamid expected =
        Util.getTeamSearchVisibilityAvailable g owner teamid !!! do
          statusCode === const 200
          responseJsonEither === const (Right (Public.TeamFeatureStatusNoConfig expected))

  let getTeamSearchVisibilityInternal ::
        (Monad m, MonadHttp m, MonadIO m, MonadCatch m, HasCallStack) =>
        TeamId ->
        Public.TeamFeatureStatusValue ->
        m ()
      getTeamSearchVisibilityInternal teamid expected =
        Util.getTeamSearchVisibilityAvailableInternal g teamid !!! do
          statusCode === const 200
          responseJsonEither === const (Right (Public.TeamFeatureStatusNoConfig expected))

  let getTeamSearchVisibilityFeatureConfig ::
        (Monad m, MonadHttp m, MonadIO m, MonadCatch m, HasCallStack) =>
        UserId ->
        Public.TeamFeatureStatusValue ->
        m ()
      getTeamSearchVisibilityFeatureConfig uid expected =
        Util.getFeatureConfigWithGalley Public.TeamFeatureSearchVisibility g uid !!! do
          statusCode === const 200
          responseJsonEither === const (Right (Public.TeamFeatureStatusNoConfig expected))

  let setTeamSearchVisibilityInternal ::
        (Monad m, MonadHttp m, MonadIO m, HasCallStack) =>
        TeamId ->
        Public.TeamFeatureStatusValue ->
        m ()
      setTeamSearchVisibilityInternal = Util.putTeamSearchVisibilityAvailableInternal g

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

  opts <- view tsGConf
  let classifiedDomainsDisabled =
        opts
          & over
            (optSettings . setFeatureFlags . flagClassifiedDomains)
            (\s -> s {Public.tfwcStatus = Public.TeamFeatureDisabled})
  withSettingsOverrides classifiedDomainsDisabled $ do
    getClassifiedDomains member tid expected
    getClassifiedDomainsInternal tid expected
    getClassifiedDomainsFeatureConfig member expected

testSimpleFlag ::
  forall (ps :: Public.IncludeLockStatus) (a :: Public.TeamFeatureName).
  ( HasCallStack,
    Typeable a,
    Public.FeatureHasNoConfig ps a,
    Public.KnownTeamFeatureName a,
    FromJSON (Public.TeamFeatureStatus 'Public.WithoutLockStatus a),
    ToJSON (Public.TeamFeatureStatus ps a)
  ) =>
  Public.TeamFeatureStatusValue ->
  TestM ()
testSimpleFlag defaultValue = do
  let feature = Public.knownTeamFeatureName @a
  owner <- Util.randomUser
  member <- Util.randomUser
  nonMember <- Util.randomUser
  tid <- Util.createNonBindingTeam "foo" owner []
  Util.connectUsers owner (list1 member [])
  Util.addTeamMember owner tid member (rolePermissions RoleMember) Nothing

  let getFlag :: HasCallStack => Public.TeamFeatureStatusValue -> TestM ()
      getFlag expected =
        flip (assertFlagNoConfig @ps @a) expected $ Util.getTeamFeatureFlag feature member tid

      getFeatureConfig :: HasCallStack => Public.TeamFeatureStatusValue -> TestM ()
      getFeatureConfig expected =
        flip (assertFlagNoConfig @ps @a) expected $ Util.getFeatureConfig feature member

      getFlagInternal :: HasCallStack => Public.TeamFeatureStatusValue -> TestM ()
      getFlagInternal expected =
        flip (assertFlagNoConfig @ps @a) expected $ Util.getTeamFeatureFlagInternal feature tid

      setFlagInternal :: Public.TeamFeatureStatusValue -> TestM ()
      setFlagInternal statusValue =
        void $ Util.putTeamFeatureFlagInternal @ps @a expect2xx tid (Public.TeamFeatureStatusNoConfig statusValue)

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
    setFlagInternal otherValue
    void . liftIO $
      WS.assertMatch (5 # Second) ws $
        wsAssertFeatureConfigUpdate feature otherValue
  getFlag otherValue
  getFeatureConfig otherValue
  getFlagInternal otherValue

  -- Clean up
  setFlagInternal defaultValue
  getFlag defaultValue

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
          Util.putTeamFeatureFlagInternal @'Public.WithoutLockStatus @'Public.TeamFeatureSelfDeletingMessages
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

-- | Call 'GET /teams/:tid/features' and 'GET /feature-configs', and check if all
-- features are there.
testAllFeatures :: TestM ()
testAllFeatures = do
  (_owner, tid, member : _) <- Util.createBindingTeamWithNMembers 1
  Util.getAllTeamFeatures member tid !!! do
    statusCode === const 200
    responseJsonMaybe === const (Just (expected TeamFeatureEnabled {- determined by default in galley -}))
  Util.getAllTeamFeaturesPersonal member !!! do
    statusCode === const 200
    responseJsonMaybe === const (Just (expected TeamFeatureEnabled {- determined by default in galley -}))

  randomPersonalUser <- Util.randomUser
  Util.getAllTeamFeaturesPersonal randomPersonalUser !!! do
    statusCode === const 200
    responseJsonMaybe === const (Just (expected TeamFeatureEnabled {- determined by 'getAfcConferenceCallingDefNew' in brig -}))
  where
    expected confCalling =
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
          toS TeamFeatureFileSharing .= Public.TeamFeatureStatusNoConfig TeamFeatureEnabled,
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
              Public.Locked
        ]
    toS :: TeamFeatureName -> Text
    toS = TE.decodeUtf8 . toByteString'

testFeatureConfigConsistency :: TestM ()
testFeatureConfigConsistency = do
  owner <- Util.randomUser
  member <- Util.randomUser
  tid <- Util.createNonBindingTeam "foo" owner []
  Util.connectUsers owner (list1 member [])
  Util.addTeamMember owner tid member (rolePermissions RoleMember) Nothing

  allFeaturesRes <- Util.getAllFeatureConfigs member >>= parseObjectKeys
  liftIO $ allFeaturesRes `shouldBe` allFeatures

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
            (Aeson.Object hm) -> pure (Set.fromList . HashSet.toList . HashMap.keysSet $ hm)
            x -> liftIO $ assertFailure ("JSON was not an object, but " <> show x)

    allFeatures :: Set.Set Text
    allFeatures = Set.fromList $ cs . toByteString' @TeamFeatureName <$> [minBound ..]

assertFlagForbidden :: HasCallStack => TestM ResponseLBS -> TestM ()
assertFlagForbidden res = do
  res !!! do
    statusCode === const 403
    fmap label . responseJsonMaybe === const (Just "no-team-member")

assertFlagNoConfig ::
  forall (ps :: Public.IncludeLockStatus) (a :: Public.TeamFeatureName).
  ( HasCallStack,
    Typeable a,
    Public.FeatureHasNoConfig ps a,
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
        . responseJsonEither @(Public.TeamFeatureStatus ps a)
      )
      === const (Right expected)

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
