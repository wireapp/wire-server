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

module API.Util.TeamFeature where

import API.Util (HasGalley (viewGalley), zUser)
import qualified API.Util as Util
import Bilge
import qualified Bilge.TestSession as BilgeTest
import Control.Lens (view, (.~))
import Data.Aeson (ToJSON)
import Data.ByteString.Conversion (toByteString')
import Data.Id (TeamId, UserId)
import Galley.Options (optSettings, setFeatureFlags)
import Galley.Types.Teams
import Imports
import TestSetup
import qualified Wire.API.Team.Feature as Public

withCustomSearchFeature :: FeatureTeamSearchVisibility -> BilgeTest.SessionT TestM () -> TestM ()
withCustomSearchFeature flag action = do
  opts <- view tsGConf
  let opts' = opts & optSettings . setFeatureFlags . flagTeamSearchVisibility .~ flag
  Util.withSettingsOverrides opts' action

getTeamSearchVisibilityAvailable :: HasCallStack => (Request -> Request) -> UserId -> TeamId -> (MonadIO m, MonadHttp m) => m ResponseLBS
getTeamSearchVisibilityAvailable = getTeamFeatureFlagWithGalley Public.TeamFeatureSearchVisibility

getTeamSearchVisibilityAvailableInternal :: HasCallStack => (Request -> Request) -> TeamId -> (MonadIO m, MonadHttp m) => m ResponseLBS
getTeamSearchVisibilityAvailableInternal =
  getTeamFeatureFlagInternalWithGalley Public.TeamFeatureSearchVisibility

putTeamSearchVisibilityAvailableInternal ::
  HasCallStack =>
  (Request -> Request) ->
  TeamId ->
  Public.TeamFeatureStatusValue ->
  (MonadIO m, MonadHttp m) => m ()
putTeamSearchVisibilityAvailableInternal g tid statusValue =
  void $
    putTeamFeatureFlagInternalWithGalleyAndMod
      @'Public.TeamFeatureSearchVisibility
      g
      expect2xx
      tid
      (Public.TeamFeatureStatusNoConfig statusValue)

putLegalHoldEnabledInternal' ::
  HasCallStack =>
  (Request -> Request) ->
  TeamId ->
  Public.TeamFeatureStatusValue ->
  TestM ()
putLegalHoldEnabledInternal' g tid statusValue =
  void $ putTeamFeatureFlagInternal @'Public.TeamFeatureLegalHold g tid (Public.TeamFeatureStatusNoConfig statusValue)

--------------------------------------------------------------------------------

getTeamFeatureFlagInternal ::
  (HasGalley m, MonadIO m, MonadHttp m) =>
  Public.TeamFeatureName ->
  TeamId ->
  m ResponseLBS
getTeamFeatureFlagInternal feature tid = do
  g <- viewGalley
  getTeamFeatureFlagInternalWithGalley feature g tid

getTeamFeatureFlagInternalWithGalley :: (MonadIO m, MonadHttp m, HasCallStack) => Public.TeamFeatureName -> (Request -> Request) -> TeamId -> m ResponseLBS
getTeamFeatureFlagInternalWithGalley feature g tid = do
  get $
    g
      . paths ["i", "teams", toByteString' tid, "features", toByteString' feature]

getTeamFeatureFlag ::
  (HasGalley m, MonadIO m, MonadHttp m, HasCallStack) =>
  Public.TeamFeatureName ->
  UserId ->
  TeamId ->
  m ResponseLBS
getTeamFeatureFlag feature uid tid = do
  g <- viewGalley
  getTeamFeatureFlagWithGalley feature g uid tid

getAllTeamFeatures ::
  (HasCallStack, HasGalley m, MonadIO m, MonadHttp m) =>
  UserId ->
  TeamId ->
  m ResponseLBS
getAllTeamFeatures uid tid = do
  g <- viewGalley
  get $
    g
      . paths ["teams", toByteString' tid, "features"]
      . zUser uid

getAllTeamFeaturesPersonal ::
  (HasCallStack, HasGalley m, MonadIO m, MonadHttp m) =>
  UserId ->
  m ResponseLBS
getAllTeamFeaturesPersonal uid = do
  g <- viewGalley
  get $
    g
      . paths ["feature-configs"]
      . zUser uid

getTeamFeatureFlagWithGalley :: (MonadIO m, MonadHttp m, HasCallStack) => Public.TeamFeatureName -> (Request -> Request) -> UserId -> TeamId -> m ResponseLBS
getTeamFeatureFlagWithGalley feature galley uid tid = do
  get $
    galley
      . paths ["teams", toByteString' tid, "features", toByteString' feature]
      . zUser uid

getFeatureConfig :: (HasCallStack, HasGalley m, MonadIO m, MonadHttp m) => Public.TeamFeatureName -> UserId -> m ResponseLBS
getFeatureConfig feature uid = do
  g <- viewGalley
  getFeatureConfigWithGalley feature g uid

getFeatureConfigWithGalley :: (MonadIO m, MonadHttp m, HasCallStack) => Public.TeamFeatureName -> (Request -> Request) -> UserId -> m ResponseLBS
getFeatureConfigWithGalley feature galley uid = do
  get $
    galley
      . paths ["feature-configs", toByteString' feature]
      . zUser uid

getAllFeatureConfigs :: HasCallStack => UserId -> TestM ResponseLBS
getAllFeatureConfigs uid = do
  g <- view tsGalley
  getAllFeatureConfigsWithGalley g uid

getAllFeatureConfigsWithGalley :: (MonadIO m, MonadHttp m, HasCallStack) => (Request -> Request) -> UserId -> m ResponseLBS
getAllFeatureConfigsWithGalley galley uid = do
  get $
    galley
      . paths ["feature-configs"]
      . zUser uid

putTeamFeatureFlagInternal ::
  forall (a :: Public.TeamFeatureName).
  ( HasCallStack,
    Public.KnownTeamFeatureName a,
    ToJSON (Public.TeamFeatureStatus 'Public.WithoutLockStatus a)
  ) =>
  (Request -> Request) ->
  TeamId ->
  Public.TeamFeatureStatus 'Public.WithoutLockStatus a ->
  TestM ResponseLBS
putTeamFeatureFlagInternal reqmod tid status = do
  g <- view tsGalley
  putTeamFeatureFlagInternalWithGalleyAndMod @a g reqmod tid status

putTeamFeatureFlagInternalWithGalleyAndMod ::
  forall (a :: Public.TeamFeatureName) m.
  ( MonadIO m,
    MonadHttp m,
    HasCallStack,
    Public.KnownTeamFeatureName a,
    ToJSON (Public.TeamFeatureStatus 'Public.WithoutLockStatus a)
  ) =>
  (Request -> Request) ->
  (Request -> Request) ->
  TeamId ->
  Public.TeamFeatureStatus 'Public.WithoutLockStatus a ->
  m ResponseLBS
putTeamFeatureFlagInternalWithGalleyAndMod galley reqmod tid status =
  put $
    galley
      . paths ["i", "teams", toByteString' tid, "features", toByteString' (Public.knownTeamFeatureName @a)]
      . json status
      . reqmod

setLockStatusInternal ::
  forall (a :: Public.TeamFeatureName).
  ( HasCallStack,
    Public.KnownTeamFeatureName a,
    ToJSON Public.LockStatusValue
  ) =>
  (Request -> Request) ->
  TeamId ->
  Public.LockStatusValue ->
  TestM ResponseLBS
setLockStatusInternal reqmod tid lockStatus = do
  galley <- view tsGalley
  put $
    galley
      . paths ["i", "teams", toByteString' tid, "features", toByteString' (Public.knownTeamFeatureName @a), toByteString' lockStatus]
      . reqmod
