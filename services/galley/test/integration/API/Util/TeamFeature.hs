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
import Control.Lens ((%~))
import Data.ByteString.Conversion (toByteString')
import Data.Id (ConvId, TeamId, UserId)
import Galley.Options (featureFlags, settings)
import Galley.Types.Teams
import Imports
import TestSetup
import Wire.API.Team.Feature

withCustomSearchFeature :: FeatureDefaults SearchVisibilityAvailableConfig -> TestM () -> TestM ()
withCustomSearchFeature flag action = do
  Util.withSettingsOverrides
    ( \opts ->
        opts & settings . featureFlags %~ npUpdate @SearchVisibilityAvailableConfig flag
    )
    action

putTeamSearchVisibilityAvailableInternal ::
  (HasCallStack) =>
  TeamId ->
  FeatureStatus ->
  (MonadIO m, MonadHttp m, HasGalley m) => m ()
putTeamSearchVisibilityAvailableInternal tid statusValue =
  void $
    putTeamFeatureInternal
      @SearchVisibilityAvailableConfig
      expect2xx
      tid
      (Feature statusValue SearchVisibilityAvailableConfig)

putTeamFeatureInternal ::
  forall cfg m.
  ( Monad m,
    HasGalley m,
    MonadHttp m,
    HasCallStack,
    IsFeatureConfig cfg
  ) =>
  (Request -> Request) ->
  TeamId ->
  Feature cfg ->
  m ResponseLBS
putTeamFeatureInternal reqmod tid status = do
  galley <- viewGalley
  put $
    galley
      . paths ["i", "teams", toByteString' tid, "features", featureNameBS @cfg]
      . json status
      . reqmod

putTeamFeature ::
  forall cfg.
  (HasCallStack, IsFeatureConfig cfg) =>
  UserId ->
  TeamId ->
  Feature cfg ->
  TestM ResponseLBS
putTeamFeature uid tid status = do
  galley <- viewGalley
  put $
    galley
      . paths ["teams", toByteString' tid, "features", featureNameBS @cfg]
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
      . paths ["conversations", toByteString' cid, "features", featureNameBS @GuestLinksConfig]
      . zUser u

getTeamFeatureInternal ::
  forall cfg m.
  (HasGalley m, MonadIO m, MonadHttp m, IsFeatureConfig cfg) =>
  TeamId ->
  m ResponseLBS
getTeamFeatureInternal tid = do
  g <- viewGalley
  get $
    g
      . paths ["i", "teams", toByteString' tid, "features", featureNameBS @cfg]

getTeamFeature ::
  forall cfg m.
  (HasGalley m, MonadIO m, MonadHttp m, HasCallStack, IsFeatureConfig cfg) =>
  UserId ->
  TeamId ->
  m ResponseLBS
getTeamFeature uid tid = do
  galley <- viewGalley
  get $
    galley
      . paths ["teams", toByteString' tid, "features", featureNameBS @cfg]
      . zUser uid
