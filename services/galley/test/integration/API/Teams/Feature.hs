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
import Control.Lens (over, view)
import Control.Monad.Catch (MonadCatch)
import Data.Aeson (FromJSON, ToJSON, object, (.=))
import qualified Data.Aeson as Aeson
import Data.ByteString.Conversion (toByteString')
import Data.Domain (Domain (..))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.Id
import Data.List1 (list1)
import Data.Proxy (Proxy (Proxy))
import Data.Schema (ToSchema)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import GHC.TypeLits (KnownSymbol, symbolVal)
import Galley.Options (optSettings, setFeatureFlags)
import Galley.Types.Teams
import Imports
import Network.Wai.Utilities (label)
import Test.Hspec (expectationFailure, shouldBe)
import Test.Tasty
import Test.Tasty.HUnit (assertFailure, (@?=))
import TestHelpers (test)
import TestSetup
import Wire.API.Team.Feature (KnownTeamFeatureName (..), TeamFeatureName (..), TeamFeatureStatusValue (..))
import qualified Wire.API.Team.Feature as Public

tests :: IO TestSetup -> TestTree
tests s =
  testGroup "Feature Config API and Team Features API" $
    [ test s "SSO" testSSO,
      test s "LegalHold" testLegalHold,
      test s "SearchVisibility" testSearchVisibility,
      test s "DigitalSignatures" $ testSimpleFlag @'Public.TeamFeatureDigitalSignatures Public.TeamFeatureDisabled,
      test s "ValidateSAMLEmails" $ testSimpleFlag @'Public.TeamFeatureValidateSAMLEmails Public.TeamFeatureDisabled,
      test s "FileSharing" $ testSimpleFlag @'Public.TeamFeatureFileSharing Public.TeamFeatureEnabled,
      test s "Classified Domains (enabled)" testClassifiedDomainsEnabled,
      test s "Classified Domains (disabled)" testClassifiedDomainsDisabled,
      test s "All features" testAllFeatures,
      test s "Feature Configs / Team Features Consistency" testFeatureConfigConsistency
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
      setSSOInternal = Util.putTeamFeatureFlagInternal @'Public.TeamFeatureSSO expect2xx tid . Public.TeamFeatureStatusNoConfig

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
      setLegalHoldInternal = Util.putTeamFeatureFlagInternal @'Public.TeamFeatureLegalHold expect2xx tid . Public.TeamFeatureStatusNoConfig
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
  Public.TeamFeatureStatus 'Public.TeamFeatureClassifiedDomains ->
  m ()
getClassifiedDomains member tid =
  assertFlagWithConfig @Public.TeamFeatureClassifiedDomainsConfig $
    Util.getTeamFeatureFlag Public.TeamFeatureClassifiedDomains member tid

getClassifiedDomainsInternal ::
  (HasCallStack, HasGalley m, MonadIO m, MonadHttp m, MonadCatch m) =>
  TeamId ->
  Public.TeamFeatureStatus 'Public.TeamFeatureClassifiedDomains ->
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
        Public.TeamFeatureStatus 'Public.TeamFeatureClassifiedDomains ->
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
        Public.TeamFeatureStatus 'Public.TeamFeatureClassifiedDomains ->
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
  forall (a :: Public.TeamFeatureName).
  ( HasCallStack,
    Typeable a,
    Public.FeatureHasNoConfig a,
    Public.KnownTeamFeatureName a,
    FromJSON (Public.TeamFeatureStatus a),
    ToJSON (Public.TeamFeatureStatus a)
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
        flip (assertFlagNoConfig @a) expected $ Util.getTeamFeatureFlag feature member tid

      getFeatureConfig :: HasCallStack => Public.TeamFeatureStatusValue -> TestM ()
      getFeatureConfig expected =
        flip (assertFlagNoConfig @a) expected $ Util.getFeatureConfig feature member

      getFlagInternal :: HasCallStack => Public.TeamFeatureStatusValue -> TestM ()
      getFlagInternal expected =
        flip (assertFlagNoConfig @a) expected $ Util.getTeamFeatureFlagInternal feature tid

      setFlagInternal :: Public.TeamFeatureStatusValue -> TestM ()
      setFlagInternal statusValue =
        Util.putTeamFeatureFlagInternal @a expect2xx tid (Public.TeamFeatureStatusNoConfig statusValue)

  assertFlagForbidden $ Util.getTeamFeatureFlag feature nonMember tid

  let otherValue = case defaultValue of
        Public.TeamFeatureDisabled -> Public.TeamFeatureEnabled
        Public.TeamFeatureEnabled -> Public.TeamFeatureDisabled

  -- Disabled by default
  getFlag defaultValue
  getFlagInternal defaultValue
  getFeatureConfig defaultValue

  -- Setting should work
  setFlagInternal otherValue
  getFlag otherValue
  getFeatureConfig otherValue
  getFlagInternal otherValue

-- | Call 'GET /teams/:tid/features' and check if all features are there
testAllFeatures :: TestM ()
testAllFeatures = do
  (_owner, tid, member : _) <- Util.createBindingTeamWithNMembers 1
  let res = Util.getAllTeamFeatures member tid
  res !!! do
    statusCode === const 200
    responseJsonMaybe === const (Just expected)
  where
    expected =
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
              (Public.TeamFeatureClassifiedDomainsConfig [Domain "example.com"])
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

  pure ()
  where
    parseObjectKeys :: ResponseLBS -> TestM (Set.Set Text)
    parseObjectKeys res = do
      case responseJsonEither res of
        Left err -> liftIO $ assertFailure ("Did not parse as an object" <> err)
        Right (val :: Aeson.Value) ->
          case val of
            (Aeson.Object hm) -> pure (Set.fromList . HashSet.toList . HashMap.keysSet $ hm)
            x -> liftIO $ assertFailure ("JSON was not an object, but " <> show x)

    featureKey :: forall (a :: TeamFeatureName). (KnownTeamFeatureName a, KnownSymbol (KnownTeamFeatureNameSymbol a)) => Text
    featureKey = Text.pack $ symbolVal (Proxy @(KnownTeamFeatureNameSymbol a))

    allFeatures :: Set.Set Text
    allFeatures =
      Set.fromList
        [ featureKey @'TeamFeatureLegalHold,
          featureKey @'TeamFeatureSSO,
          featureKey @'TeamFeatureSearchVisibility,
          featureKey @'TeamFeatureValidateSAMLEmails,
          featureKey @'TeamFeatureDigitalSignatures,
          featureKey @'TeamFeatureAppLock,
          featureKey @'TeamFeatureFileSharing,
          featureKey @'TeamFeatureClassifiedDomains
        ]

assertFlagForbidden :: HasCallStack => TestM ResponseLBS -> TestM ()
assertFlagForbidden res = do
  res !!! do
    statusCode === const 403
    fmap label . responseJsonMaybe === const (Just "no-team-member")

assertFlagNoConfig ::
  forall (a :: Public.TeamFeatureName).
  ( HasCallStack,
    Typeable a,
    Public.FeatureHasNoConfig a,
    FromJSON (Public.TeamFeatureStatus a),
    Public.KnownTeamFeatureName a
  ) =>
  TestM ResponseLBS ->
  Public.TeamFeatureStatusValue ->
  TestM ()
assertFlagNoConfig res expected = do
  res !!! do
    statusCode === const 200
    ( fmap Public.tfwoStatus
        . responseJsonEither @(Public.TeamFeatureStatus a)
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
