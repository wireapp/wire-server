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

import qualified API.Util as Util
import qualified API.Util.TeamFeature as Util
import Bilge
import Bilge.Assert
import Control.Lens (view)
import Control.Monad.Catch (MonadCatch)
import Data.Aeson (FromJSON, ToJSON)
import Data.Id
import Data.List1 (list1)
import Galley.Options (optSettings, setFeatureFlags)
import Galley.Types.Teams
import Imports
import Test.Tasty
import TestHelpers (test)
import TestSetup
import Wire.API.Team.Feature (FeatureHasStatus (..))
import qualified Wire.API.Team.Feature as Public
import qualified Wire.API.Team.Member as Public

tests :: IO TestSetup -> TestTree
tests s =
  testGroup "Team Features API" $
    [ test s "SSO" testSSO,
      test s "LegalHold" testLegalHold,
      test s "SearchVisibility" testSearchVisibility,
      test s "DigitalSignatures" $ testSimpleFlag @'Public.TeamFeatureDigitalSignatures,
      test s "ValidateSAMLEmails" $ testSimpleFlag @'Public.TeamFeatureValidateSAMLEmails
    ]

testSSO :: TestM ()
testSSO = do
  owner <- Util.randomUser
  member <- Util.randomUser
  tid <- Util.createNonBindingTeam "foo" owner []
  Util.connectUsers owner (list1 member [])
  Util.addTeamMember owner tid (Public.newTeamMember member (rolePermissions RoleMember) Nothing)

  let getSSO :: HasCallStack => Public.TeamFeatureStatusValue -> TestM ()
      getSSO = assertFlagNoConfig @'Public.TeamFeatureSSO $ Util.getTeamFeatureFlag Public.TeamFeatureSSO member tid
      getSSOInternal :: HasCallStack => Public.TeamFeatureStatusValue -> TestM ()
      getSSOInternal = assertFlagNoConfig @'Public.TeamFeatureSSO $ Util.getTeamFeatureFlagInternal Public.TeamFeatureSSO tid
      setSSOInternal :: HasCallStack => Public.TeamFeatureStatusValue -> TestM ()
      setSSOInternal = Util.putTeamFeatureFlagInternal @'Public.TeamFeatureSSO expect2xx tid . Public.TeamFeatureStatusNoConfig
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

  let getLegalHold :: HasCallStack => Public.TeamFeatureStatusValue -> TestM ()
      getLegalHold = assertFlagNoConfig @'Public.TeamFeatureLegalHold $ Util.getTeamFeatureFlag Public.TeamFeatureLegalHold member tid
      getLegalHoldInternal :: HasCallStack => Public.TeamFeatureStatusValue -> TestM ()
      getLegalHoldInternal = assertFlagNoConfig @'Public.TeamFeatureLegalHold $ Util.getTeamFeatureFlagInternal Public.TeamFeatureLegalHold tid

      setLegalHoldInternal :: HasCallStack => Public.TeamFeatureStatusValue -> TestM ()
      setLegalHoldInternal = Util.putTeamFeatureFlagInternal @'Public.TeamFeatureLegalHold expect2xx tid . Public.TeamFeatureStatusNoConfig
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

  let setTeamSearchVisibilityInternal ::
        (Monad m, MonadHttp m, MonadIO m, HasCallStack) =>
        TeamId ->
        Public.TeamFeatureStatusValue ->
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

testSimpleFlag ::
  forall (a :: Public.TeamFeatureName).
  ( HasCallStack,
    Typeable a,
    Public.FeatureHasNoConfig a,
    Public.KnownTeamFeatureName a,
    FromJSON (Public.TeamFeatureStatus a),
    ToJSON (Public.TeamFeatureStatus a)
  ) =>
  TestM ()
testSimpleFlag = do
  let feature = Public.knownTeamFeatureName @a
  owner <- Util.randomUser
  member <- Util.randomUser
  tid <- Util.createNonBindingTeam "foo" owner []
  Util.connectUsers owner (list1 member [])
  Util.addTeamMember owner tid (Public.newTeamMember member (rolePermissions RoleMember) Nothing)

  let getFlag :: HasCallStack => Public.TeamFeatureStatusValue -> TestM ()
      getFlag expected =
        flip (assertFlagNoConfig @a) expected $ Util.getTeamFeatureFlag feature member tid

      getFlagInternal :: HasCallStack => Public.TeamFeatureStatusValue -> TestM ()
      getFlagInternal expected =
        flip (assertFlagNoConfig @a) expected $ Util.getTeamFeatureFlagInternal feature tid

      setFlagInternal :: Public.TeamFeatureStatusValue -> TestM ()
      setFlagInternal statusValue =
        Util.putTeamFeatureFlagInternal @a expect2xx tid (Public.TeamFeatureStatusNoConfig statusValue)

  -- Disabled by default
  getFlag Public.TeamFeatureDisabled
  getFlagInternal Public.TeamFeatureDisabled

  -- Settting should work
  setFlagInternal Public.TeamFeatureEnabled
  getFlag Public.TeamFeatureEnabled
  getFlagInternal Public.TeamFeatureEnabled

assertFlagNoConfig ::
  forall (a :: Public.TeamFeatureName).
  ( HasCallStack,
    Typeable a,
    Public.FeatureHasNoConfig a,
    FeatureHasStatus (Public.TeamFeatureStatus a),
    FromJSON (Public.TeamFeatureStatus a),
    Public.KnownTeamFeatureName a
  ) =>
  TestM ResponseLBS ->
  Public.TeamFeatureStatusValue ->
  TestM ()
assertFlagNoConfig res expected = do
  res !!! do
    statusCode === const 200
    ( fmap featureStatus
        . responseJsonEither @(Public.TeamFeatureStatus a)
      )
      === const (Right expected)
