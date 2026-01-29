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
module Wire.TeamFeatureStore where

import Data.Id
import Data.SOP (K (..))
import Imports
import Polysemy
import Wire.API.Team.Feature

type DbFeaturePatch = LockableFeaturePatch DbConfig

type AllDbFeaturePatches = AllFeatures (K (Maybe DbFeaturePatch))

data TeamFeatureStore m a where
  -- | Returns all stored feature values excluding lock status.
  GetDbFeature ::
    FeatureSingleton cfg ->
    TeamId ->
    TeamFeatureStore m (Maybe DbFeaturePatch)
  SetDbFeature ::
    FeatureSingleton cfg ->
    TeamId ->
    LockableFeature cfg ->
    TeamFeatureStore m ()
  PatchDbFeature ::
    FeatureSingleton cfg ->
    TeamId ->
    (LockableFeaturePatch cfg) ->
    TeamFeatureStore m ()
  SetFeatureLockStatus ::
    FeatureSingleton cfg ->
    TeamId ->
    LockStatus ->
    TeamFeatureStore m ()
  GetAllDbFeatures ::
    TeamId ->
    TeamFeatureStore m AllDbFeaturePatches

getDbFeature ::
  forall cfg r.
  (Member TeamFeatureStore r, IsFeatureConfig cfg) =>
  TeamId ->
  Sem r (Maybe DbFeaturePatch)
getDbFeature tid = send (GetDbFeature (featureSingleton @cfg) tid)

setDbFeature ::
  forall cfg r.
  (Member TeamFeatureStore r, IsFeatureConfig cfg) =>
  TeamId ->
  LockableFeature cfg ->
  Sem r ()
setDbFeature tid feat = send (SetDbFeature (featureSingleton @cfg) tid feat)

patchDbFeature ::
  forall cfg r.
  (Member TeamFeatureStore r, IsFeatureConfig cfg) =>
  TeamId ->
  (LockableFeaturePatch cfg) ->
  Sem r ()
patchDbFeature tid featPatch = send (PatchDbFeature (featureSingleton @cfg) tid featPatch)

setFeatureLockStatus ::
  forall cfg r.
  (Member TeamFeatureStore r, IsFeatureConfig cfg) =>
  TeamId ->
  LockStatus ->
  Sem r ()
setFeatureLockStatus tid lockStatus =
  send (SetFeatureLockStatus (featureSingleton @cfg) tid lockStatus)

getAllDbFeatures :: (Member TeamFeatureStore r) => TeamId -> Sem r AllDbFeaturePatches
getAllDbFeatures tid = send (GetAllDbFeatures tid)
