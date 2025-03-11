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

module Galley.Effects.TeamFeatureStore where

import Data.Id
import Polysemy
import Wire.API.Team.Feature

data TeamFeatureStore m a where
  -- | Returns all stored feature values excluding lock status.
  GetDbFeature ::
    FeatureSingleton cfg ->
    TeamId ->
    TeamFeatureStore m (DbFeature cfg)
  SetDbFeature ::
    FeatureSingleton cfg ->
    TeamId ->
    LockableFeature cfg ->
    TeamFeatureStore m ()
  SetFeatureLockStatus ::
    FeatureSingleton cfg ->
    TeamId ->
    LockStatus ->
    TeamFeatureStore m ()
  GetAllDbFeatures ::
    TeamId ->
    TeamFeatureStore m (AllFeatures DbFeature)

getDbFeature ::
  (Member TeamFeatureStore r, IsFeatureConfig cfg) =>
  TeamId ->
  Sem r (DbFeature cfg)
getDbFeature tid = send (GetDbFeature featureSingleton tid)

setDbFeature ::
  (Member TeamFeatureStore r, IsFeatureConfig cfg) =>
  TeamId ->
  LockableFeature cfg ->
  Sem r ()
setDbFeature tid feat = send (SetDbFeature featureSingleton tid feat)

setFeatureLockStatus ::
  forall cfg r.
  (Member TeamFeatureStore r, IsFeatureConfig cfg) =>
  TeamId ->
  LockStatus ->
  Sem r ()
setFeatureLockStatus tid lockStatus =
  send (SetFeatureLockStatus (featureSingleton @cfg) tid lockStatus)

getAllDbFeatures :: (Member TeamFeatureStore r) => TeamId -> Sem r (AllFeatures DbFeature)
getAllDbFeatures tid = send (GetAllDbFeatures tid)
