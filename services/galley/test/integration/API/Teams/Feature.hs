module API.Teams.Feature (tests) where

import qualified API.Util as Util
import qualified API.Util.TeamFeature as Util
import Bilge
import Bilge.Assert
import Control.Lens (view)
import Control.Monad.Catch (MonadCatch)
import Data.Id (TeamId)
import Data.List1 (list1)
import Galley.Options (optSettings, setFeatureFlags)
import Galley.Types.Teams
import Imports
import Test.Tasty
import TestHelpers (test)
import TestSetup
import qualified Wire.API.Team.Feature as Public
import qualified Wire.API.Team.Member as Public

tests :: IO TestSetup -> TestTree
tests s =
  testGroup "Team Features API" $
    [ test s "SSO" testSSO,
      test s "LegalHold" testLegalHold,
      test s "SearchVisibility" testSearchVisibility,
      test s "DigitalSignatures" $ testSimpleFlag Public.TeamFeatureDigitalSignatures,
      test s "ValidateSAMLEmails" $ testSimpleFlag Public.TeamFeatureValidateSAMLEmails
    ]

testSSO :: TestM ()
testSSO = do
  owner <- Util.randomUser
  member <- Util.randomUser
  tid <- Util.createNonBindingTeam "foo" owner []
  Util.connectUsers owner (list1 member [])
  Util.addTeamMember owner tid (Public.newTeamMember member (rolePermissions RoleMember) Nothing)

  let getSSO :: HasCallStack => Public.TeamFeatureStatus -> TestM ()
      getSSO = assertFlag $ Util.getTeamFeatureFlag Public.TeamFeatureSSO member tid
      getSSOInternal :: HasCallStack => Public.TeamFeatureStatus -> TestM ()
      getSSOInternal = assertFlag $ Util.getTeamFeatureFlagInternal Public.TeamFeatureSSO tid
      setSSOInternal :: HasCallStack => Public.TeamFeatureStatus -> TestM ()
      setSSOInternal = Util.putTeamFeatureFlagInternal' Public.TeamFeatureSSO expect2xx tid
  featureSSO <- view (tsGConf . optSettings . setFeatureFlags . flagSSO)
  case featureSSO of
    FeatureSSODisabledByDefault -> do
      -- Test default
      getSSO Public.TeamFeatureDisabled
      getSSOInternal Public.TeamFeatureDisabled

      -- Test override
      setSSOInternal Public.TeamFeatureEnabled
      getSSO Public.TeamFeatureEnabled
      getSSOInternal Public.TeamFeatureEnabled
    FeatureSSOEnabledByDefault -> do
      -- since we don't allow to disable (see 'disableSsoNotImplemented'), we can't test
      -- much here.  (disable failure is covered in "enable/disable SSO" above.)
      getSSO Public.TeamFeatureEnabled
      getSSOInternal Public.TeamFeatureEnabled

testLegalHold :: TestM ()
testLegalHold = do
  owner <- Util.randomUser
  member <- Util.randomUser
  tid <- Util.createNonBindingTeam "foo" owner []
  Util.connectUsers owner (list1 member [])
  Util.addTeamMember owner tid (Public.newTeamMember member (rolePermissions RoleMember) Nothing)

  let getLegalHold :: HasCallStack => Public.TeamFeatureStatus -> TestM ()
      getLegalHold = assertFlag $ Util.getTeamFeatureFlag Public.TeamFeatureLegalHold member tid
      getLegalHoldInternal :: HasCallStack => Public.TeamFeatureStatus -> TestM ()
      getLegalHoldInternal = assertFlag $ Util.getTeamFeatureFlagInternal Public.TeamFeatureLegalHold tid
      setLegalHoldInternal :: HasCallStack => Public.TeamFeatureStatus -> TestM ()
      setLegalHoldInternal = Util.putTeamFeatureFlagInternal' Public.TeamFeatureLegalHold expect2xx tid
  getLegalHold Public.TeamFeatureDisabled
  getLegalHoldInternal Public.TeamFeatureDisabled

  -- FUTUREWORK: run two galleys, like below for custom search visibility.
  featureLegalHold <- view (tsGConf . optSettings . setFeatureFlags . flagLegalHold)
  case featureLegalHold of
    FeatureLegalHoldDisabledByDefault -> do
      -- Test default
      getLegalHold Public.TeamFeatureDisabled
      getLegalHoldInternal Public.TeamFeatureDisabled

      -- Test override
      setLegalHoldInternal Public.TeamFeatureEnabled
      getLegalHold Public.TeamFeatureEnabled
      getLegalHoldInternal Public.TeamFeatureEnabled
    FeatureLegalHoldDisabledPermanently -> do
      Util.putLegalHoldEnabledInternal' expect4xx tid Public.TeamFeatureEnabled

testSearchVisibility :: TestM ()
testSearchVisibility = do
  owner <- Util.randomUser
  member <- Util.randomUser
  tid <- Util.createNonBindingTeam "foo" owner []
  Util.connectUsers owner (list1 member [])
  Util.addTeamMember owner tid (Public.newTeamMember member (rolePermissions RoleMember) Nothing)

  g <- view tsGalley
  let getTeamSearchVisibility ::
        (Monad m, MonadHttp m, MonadIO m, MonadCatch m, HasCallStack) =>
        TeamId ->
        Public.TeamFeatureStatus ->
        m ()
      getTeamSearchVisibility teamid expected = Util.getTeamSearchVisibilityAvailable g owner teamid !!! do
        statusCode === const 200
        responseJsonEither === const (Right expected)

  let getTeamSearchVisibilityInternal ::
        (Monad m, MonadHttp m, MonadIO m, MonadCatch m, HasCallStack) =>
        TeamId ->
        Public.TeamFeatureStatus ->
        m ()
      getTeamSearchVisibilityInternal teamid expected = Util.getTeamSearchVisibilityAvailableInternal g teamid !!! do
        statusCode === const 200
        responseJsonEither === const (Right expected)

  let setTeamSearchVisibilityInternal ::
        (Monad m, MonadHttp m, MonadIO m, HasCallStack) =>
        TeamId ->
        Public.TeamFeatureStatus ->
        m ()
      setTeamSearchVisibilityInternal = Util.putTeamSearchVisibilityAvailableInternal g

  tid2 <- Util.createNonBindingTeam "foo" owner []
  Util.withCustomSearchFeature FeatureTeamSearchVisibilityDisabledByDefault $ do
    getTeamSearchVisibility tid2 Public.TeamFeatureDisabled
    getTeamSearchVisibilityInternal tid2 Public.TeamFeatureDisabled
    setTeamSearchVisibilityInternal tid2 Public.TeamFeatureEnabled
    getTeamSearchVisibility tid2 Public.TeamFeatureEnabled
    getTeamSearchVisibilityInternal tid2 Public.TeamFeatureEnabled
    setTeamSearchVisibilityInternal tid2 Public.TeamFeatureDisabled
    getTeamSearchVisibility tid2 Public.TeamFeatureDisabled
    getTeamSearchVisibilityInternal tid2 Public.TeamFeatureDisabled
  tid3 <- Util.createNonBindingTeam "foo" owner []
  Util.withCustomSearchFeature FeatureTeamSearchVisibilityEnabledByDefault $ do
    getTeamSearchVisibility tid3 Public.TeamFeatureEnabled
    getTeamSearchVisibilityInternal tid3 Public.TeamFeatureEnabled
    setTeamSearchVisibilityInternal tid3 Public.TeamFeatureDisabled
    getTeamSearchVisibility tid3 Public.TeamFeatureDisabled
    getTeamSearchVisibilityInternal tid3 Public.TeamFeatureDisabled
    setTeamSearchVisibilityInternal tid3 Public.TeamFeatureEnabled
    getTeamSearchVisibility tid3 Public.TeamFeatureEnabled
    getTeamSearchVisibilityInternal tid3 Public.TeamFeatureEnabled

testSimpleFlag :: Public.TeamFeatureName -> TestM ()
testSimpleFlag feature = do
  owner <- Util.randomUser
  member <- Util.randomUser
  tid <- Util.createNonBindingTeam "foo" owner []
  Util.connectUsers owner (list1 member [])
  Util.addTeamMember owner tid (Public.newTeamMember member (rolePermissions RoleMember) Nothing)

  let getFlag :: HasCallStack => Public.TeamFeatureName -> Public.TeamFeatureStatus -> TestM ()
      getFlag f expected = flip assertFlag expected $ Util.getTeamFeatureFlag f member tid
      getFlagInternal :: HasCallStack => Public.TeamFeatureName -> Public.TeamFeatureStatus -> TestM ()
      getFlagInternal f expected = flip assertFlag expected $ Util.getTeamFeatureFlagInternal f tid
      setFlagInternal :: HasCallStack => Public.TeamFeatureName -> Public.TeamFeatureStatus -> TestM ()
      setFlagInternal f = Util.putTeamFeatureFlagInternal' f expect2xx tid

  -- Disabled by default
  getFlag feature Public.TeamFeatureDisabled
  getFlagInternal feature Public.TeamFeatureDisabled

  -- Settting should work
  setFlagInternal feature Public.TeamFeatureEnabled
  getFlag feature Public.TeamFeatureEnabled
  getFlagInternal feature Public.TeamFeatureEnabled

assertFlag :: HasCallStack => TestM ResponseLBS -> Public.TeamFeatureStatus -> TestM ()
assertFlag res expected = res !!! do
  statusCode === const 200
  responseJsonEither === const (Right expected)
