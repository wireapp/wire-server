{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.FeaturesConfigSubsystem where

import Data.Id (ConvId, TeamId, UserId)
import Data.Proxy (Proxy)
import Data.Qualified (Local)
import Imports
import Polysemy
import Wire.API.Team.Feature (AllTeamFeatures, DbFeature, LockableFeature)
import Wire.FeaturesConfigSubsystem.Types

data FeaturesConfigSubsystem m a where
  GetDbFeatureRawInternal :: forall cfg m. (GetFeatureConfig cfg) => TeamId -> FeaturesConfigSubsystem m (DbFeature cfg)
  GetFeature :: forall cfg m. (GetFeatureConfig cfg) => UserId -> TeamId -> FeaturesConfigSubsystem m (LockableFeature cfg)
  GetFeatureForTeam :: forall cfg m. (GetFeatureConfig cfg) => TeamId -> FeaturesConfigSubsystem m (LockableFeature cfg)
  GetFeatureForServer :: forall cfg m. (GetFeatureConfig cfg) => FeaturesConfigSubsystem m (LockableFeature cfg)
  GetFeatureForTeamUser :: forall cfg m. (GetFeatureConfig cfg) => UserId -> Maybe TeamId -> FeaturesConfigSubsystem m (LockableFeature cfg)
  GetAllTeamFeaturesForTeamMember :: Local UserId -> TeamId -> FeaturesConfigSubsystem m AllTeamFeatures
  GetAllTeamFeaturesForTeam :: TeamId -> FeaturesConfigSubsystem m AllTeamFeatures
  GetAllTeamFeaturesForServer :: FeaturesConfigSubsystem m AllTeamFeatures
  GuardSecondFactorDisabled ::
    UserId ->
    ConvId ->
    FeaturesConfigSubsystem m ()
  FeatureEnabledForTeam ::
    forall cfg m.
    (GetFeatureConfig cfg) =>
    Proxy cfg ->
    TeamId ->
    FeaturesConfigSubsystem m Bool
  GetAllTeamFeaturesForUser ::
    UserId ->
    FeaturesConfigSubsystem m AllTeamFeatures
  GetSingleFeatureForUser ::
    forall cfg m.
    (GetFeatureConfig cfg) =>
    UserId ->
    FeaturesConfigSubsystem m (LockableFeature cfg)
  GetFeatureInternal ::
    (GetFeatureConfig cfg) =>
    TeamId ->
    FeaturesConfigSubsystem m (LockableFeature cfg)

makeSem ''FeaturesConfigSubsystem
