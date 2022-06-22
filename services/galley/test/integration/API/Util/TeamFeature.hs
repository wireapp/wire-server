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

module API.Util.TeamFeature where

import API.Util (HasGalley (viewGalley), zUser)
import qualified API.Util as Util
import Bilge
import Control.Lens (view, (.~))
import Data.Aeson (ToJSON)
import Data.ByteString.Conversion (toByteString')
import Data.Id (ConvId, TeamId, UserId)
import Data.Schema
import GHC.TypeLits (KnownSymbol)
import Galley.Options (optSettings, setFeatureFlags)
import Galley.Types.Teams
import Imports
import TestSetup
import Wire.API.Team.Feature (IsFeatureConfig)
import qualified Wire.API.Team.Feature as Public

withCustomSearchFeature :: FeatureTeamSearchVisibilityAvailability -> TestM () -> TestM ()
withCustomSearchFeature flag action = do
  Util.withSettingsOverrides (\opts -> opts & optSettings . setFeatureFlags . flagTeamSearchVisibility .~ flag) action

getTeamSearchVisibilityAvailable :: HasCallStack => (Request -> Request) -> UserId -> TeamId -> (MonadIO m, MonadHttp m) => m ResponseLBS
getTeamSearchVisibilityAvailable = getTeamFeatureFlagWithGalley @Public.SearchVisibilityAvailableConfig

getTeamSearchVisibilityAvailableInternal :: HasCallStack => (Request -> Request) -> TeamId -> (MonadIO m, MonadHttp m) => m ResponseLBS
getTeamSearchVisibilityAvailableInternal =
  getTeamFeatureFlagInternalWithGalley @Public.SearchVisibilityAvailableConfig

putTeamSearchVisibilityAvailableInternal ::
  HasCallStack =>
  (Request -> Request) ->
  TeamId ->
  Public.FeatureStatus ->
  (MonadIO m, MonadHttp m) => m ()
putTeamSearchVisibilityAvailableInternal g tid statusValue =
  void $
    putTeamFeatureFlagInternalWithGalleyAndMod
      @Public.SearchVisibilityAvailableConfig
      g
      expect2xx
      tid
      (Public.WithStatusNoLock statusValue Public.SearchVisibilityAvailableConfig)
      Public.FeatureTTLUnlimited

putLegalHoldEnabledInternal' ::
  HasCallStack =>
  (Request -> Request) ->
  TeamId ->
  Public.FeatureStatus ->
  TestM ()
putLegalHoldEnabledInternal' g tid statusValue =
  void $ putTeamFeatureFlagInternal @Public.LegalholdConfig g tid (Public.WithStatusNoLock statusValue Public.LegalholdConfig)

--------------------------------------------------------------------------------

getTeamFeatureFlagInternal ::
  forall cfg m.
  (HasGalley m, MonadIO m, MonadHttp m, IsFeatureConfig cfg, KnownSymbol (Public.FeatureSymbol cfg)) =>
  TeamId ->
  m ResponseLBS
getTeamFeatureFlagInternal tid = do
  g <- viewGalley
  getTeamFeatureFlagInternalWithGalley @cfg g tid

getTeamFeatureFlagInternalWithGalley :: forall cfg m. (MonadIO m, MonadHttp m, HasCallStack, IsFeatureConfig cfg, KnownSymbol (Public.FeatureSymbol cfg)) => (Request -> Request) -> TeamId -> m ResponseLBS
getTeamFeatureFlagInternalWithGalley g tid = do
  get $
    g
      . paths ["i", "teams", toByteString' tid, "features", Public.featureNameBS @cfg]

getTeamFeatureFlag ::
  forall cfg m.
  (HasGalley m, MonadIO m, MonadHttp m, HasCallStack, IsFeatureConfig cfg, KnownSymbol (Public.FeatureSymbol cfg)) =>
  UserId ->
  TeamId ->
  m ResponseLBS
getTeamFeatureFlag uid tid = do
  g <- viewGalley
  getTeamFeatureFlagWithGalley @cfg g uid tid

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

getTeamFeatureFlagWithGalley :: forall cfg m. (MonadIO m, MonadHttp m, HasCallStack, IsFeatureConfig cfg, KnownSymbol (Public.FeatureSymbol cfg)) => (Request -> Request) -> UserId -> TeamId -> m ResponseLBS
getTeamFeatureFlagWithGalley galley uid tid = do
  get $
    galley
      . paths ["teams", toByteString' tid, "features", Public.featureNameBS @cfg]
      . zUser uid

getFeatureConfig :: forall cfg m. (HasCallStack, HasGalley m, MonadIO m, MonadHttp m, IsFeatureConfig cfg, KnownSymbol (Public.FeatureSymbol cfg)) => UserId -> m ResponseLBS
getFeatureConfig uid = do
  g <- viewGalley
  getFeatureConfigWithGalley @cfg g uid

getFeatureConfigWithGalley :: forall cfg m. (MonadIO m, MonadHttp m, HasCallStack, IsFeatureConfig cfg, KnownSymbol (Public.FeatureSymbol cfg)) => (Request -> Request) -> UserId -> m ResponseLBS
getFeatureConfigWithGalley galley uid = do
  get $
    galley
      . paths ["feature-configs", toByteString' (Public.featureNameBS @cfg)]
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

putTeamFeatureFlagWithGalley ::
  forall cfg.
  ( HasCallStack,
    Public.IsFeatureConfig cfg,
    KnownSymbol (Public.FeatureSymbol cfg),
    ToJSON (Public.WithStatusNoLock cfg)
  ) =>
  (Request -> Request) ->
  UserId ->
  TeamId ->
  Public.WithStatusNoLock cfg ->
  TestM ResponseLBS
putTeamFeatureFlagWithGalley galley uid tid status =
  put $
    galley
      . paths ["teams", toByteString' tid, "features", Public.featureNameBS @cfg]
      . json status
      . zUser uid

putTeamFeatureFlagInternalTTL ::
  forall cfg.
  ( HasCallStack,
    Typeable cfg,
    Public.IsFeatureConfig cfg,
    KnownSymbol (Public.FeatureSymbol cfg),
    ToSchema cfg
  ) =>
  (Request -> Request) ->
  TeamId ->
  Public.WithStatusNoLock cfg ->
  Public.FeatureTTL ->
  TestM ResponseLBS
putTeamFeatureFlagInternalTTL reqmod tid status ttl = do
  g <- view tsGalley
  putTeamFeatureFlagInternalWithGalleyAndMod @cfg g reqmod tid status ttl

putTeamFeatureFlagInternal ::
  forall cfg.
  ( HasCallStack,
    Public.IsFeatureConfig cfg,
    KnownSymbol (Public.FeatureSymbol cfg),
    ToJSON (Public.WithStatusNoLock cfg)
  ) =>
  (Request -> Request) ->
  TeamId ->
  Public.WithStatusNoLock cfg ->
  TestM ResponseLBS
putTeamFeatureFlagInternal reqmod tid status = do
  g <- view tsGalley
  putTeamFeatureFlagInternalWithGalleyAndMod @cfg g reqmod tid status Public.FeatureTTLUnlimited

putTeamFeatureFlagInternalWithGalleyAndMod ::
  forall cfg m.
  ( MonadIO m,
    MonadHttp m,
    HasCallStack,
    Public.IsFeatureConfig cfg,
    KnownSymbol (Public.FeatureSymbol cfg),
    ToJSON (Public.WithStatusNoLock cfg)
  ) =>
  (Request -> Request) ->
  (Request -> Request) ->
  TeamId ->
  Public.WithStatusNoLock cfg ->
  Public.FeatureTTL ->
  m ResponseLBS
putTeamFeatureFlagInternalWithGalleyAndMod galley reqmod tid status ttl =
  put $
    galley
      . paths ["i", "teams", toByteString' tid, "features", Public.featureNameBS @cfg]
      . json status
      . query [("ttl", justBS ttl)]
      . reqmod
  where
    justBS = Just . toByteString'

setLockStatusInternal ::
  forall cfg.
  ( HasCallStack,
    Public.IsFeatureConfig cfg,
    KnownSymbol (Public.FeatureSymbol cfg),
    ToJSON Public.LockStatus
  ) =>
  (Request -> Request) ->
  TeamId ->
  Public.LockStatus ->
  TestM ResponseLBS
setLockStatusInternal reqmod tid lockStatus = do
  galley <- view tsGalley
  put $
    galley
      . paths ["i", "teams", toByteString' tid, "features", Public.featureNameBS @cfg, toByteString' lockStatus]
      . reqmod

getGuestLinkStatus ::
  HasCallStack =>
  (Request -> Request) ->
  UserId ->
  ConvId ->
  TestM ResponseLBS
getGuestLinkStatus galley u cid =
  get $
    galley
      . paths ["conversations", toByteString' cid, "features", Public.featureNameBS @Public.GuestLinksConfig]
      . zUser u
