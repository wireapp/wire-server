-- Disabling to stop warnings on HasCallStack
{-# LANGUAGE DeepSubsumption #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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
import API.Util qualified as Util
import Bilge
import Control.Lens ((.~))
import Data.Aeson (ToJSON)
import Data.ByteString.Conversion (toByteString')
import Data.Id (ConvId, TeamId, UserId)
import GHC.TypeLits (KnownSymbol)
import Galley.Options (featureFlags, settings)
import Galley.Types.Teams
import Imports
import TestSetup
import Wire.API.Team.Feature qualified as Public

withCustomSearchFeature :: FeatureTeamSearchVisibilityAvailability -> TestM () -> TestM ()
withCustomSearchFeature flag action = do
  Util.withSettingsOverrides (\opts -> opts & settings . featureFlags . flagTeamSearchVisibility .~ flag) action

putTeamSearchVisibilityAvailableInternal ::
  (HasCallStack) =>
  TeamId ->
  Public.FeatureStatus ->
  (MonadIO m, MonadHttp m, HasGalley m) => m ()
putTeamSearchVisibilityAvailableInternal tid statusValue =
  void $
    putTeamFeatureInternal
      @Public.SearchVisibilityAvailableConfig
      expect2xx
      tid
      (Public.WithStatusNoLock statusValue Public.SearchVisibilityAvailableConfig Public.FeatureTTLUnlimited)

putTeamFeatureInternal ::
  forall cfg m.
  ( Monad m,
    HasGalley m,
    MonadHttp m,
    HasCallStack,
    KnownSymbol (Public.FeatureSymbol cfg),
    ToJSON (Public.WithStatusNoLock cfg)
  ) =>
  (Request -> Request) ->
  TeamId ->
  Public.WithStatusNoLock cfg ->
  m ResponseLBS
putTeamFeatureInternal reqmod tid status = do
  galley <- viewGalley
  put $
    galley
      . paths ["i", "teams", toByteString' tid, "features", Public.featureNameBS @cfg]
      . json status
      . reqmod

putTeamFeature ::
  forall cfg.
  ( HasCallStack,
    KnownSymbol (Public.FeatureSymbol cfg),
    ToJSON (Public.WithStatusNoLock cfg)
  ) =>
  UserId ->
  TeamId ->
  Public.WithStatusNoLock cfg ->
  TestM ResponseLBS
putTeamFeature uid tid status = do
  galley <- viewGalley
  put $
    galley
      . paths ["teams", toByteString' tid, "features", Public.featureNameBS @cfg]
      . json status
      . zUser uid

getGuestLinkStatus ::
  (HasCallStack) =>
  (Request -> Request) ->
  UserId ->
  ConvId ->
  TestM ResponseLBS
getGuestLinkStatus galley u cid =
  get $
    galley
      . paths ["conversations", toByteString' cid, "features", Public.featureNameBS @Public.GuestLinksConfig]
      . zUser u

getTeamFeatureInternal ::
  forall cfg m.
  (HasGalley m, MonadIO m, MonadHttp m, KnownSymbol (Public.FeatureSymbol cfg)) =>
  TeamId ->
  m ResponseLBS
getTeamFeatureInternal tid = do
  g <- viewGalley
  get $
    g
      . paths ["i", "teams", toByteString' tid, "features", Public.featureNameBS @cfg]

getTeamFeature ::
  forall cfg m.
  (HasGalley m, MonadIO m, MonadHttp m, HasCallStack, KnownSymbol (Public.FeatureSymbol cfg)) =>
  UserId ->
  TeamId ->
  m ResponseLBS
getTeamFeature uid tid = do
  galley <- viewGalley
  get $
    galley
      . paths ["teams", toByteString' tid, "features", Public.featureNameBS @cfg]
      . zUser uid
