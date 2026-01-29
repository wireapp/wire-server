{-# LANGUAGE RecordWildCards #-}

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

module Wire.TeamFeatureStore.Postgres (interpretTeamFeatureStoreToPostgres) where

import Data.Constraint
import Data.Id
import Data.Map qualified as Map
import Data.Proxy
import Data.SOP (K (..), hcpure)
import Data.Vector qualified as Vector
import Imports
import Polysemy
import Wire.API.Team.Feature
import Wire.API.Team.Feature.TH
import Wire.Postgres
import Wire.TeamFeatureStore
import Wire.TeamFeatureStore.Postgres.Queries

interpretTeamFeatureStoreToPostgres ::
  (PGConstraints r) =>
  Sem (TeamFeatureStore ': r) a ->
  Sem r a
interpretTeamFeatureStoreToPostgres = interpret $ \case
  GetDbFeature sing tid -> do
    getDbFeatureImpl sing tid
  SetDbFeature sing tid feat -> do
    setDbFeatureImpl sing tid feat
  SetFeatureLockStatus sing tid lock -> do
    setFeatureLockStatusImpl sing tid lock
  GetAllDbFeatures tid -> do
    getAllDbFeaturesImpl tid
  PatchDbFeature sing tid feat -> do
    patchDbFeatureImpl sing tid feat

getDbFeatureImpl ::
  forall cfg r.
  (PGConstraints r) =>
  FeatureSingleton cfg ->
  TeamId ->
  Sem r (Maybe DbFeaturePatch)
getDbFeatureImpl sing tid = case featureSingIsFeature sing of
  Dict -> do
    mRow <- runStatement (tid, featureName @cfg) select
    pure $ (\(status, lockStatus, config) -> LockableFeaturePatch {..}) <$> mRow

setDbFeatureImpl ::
  forall cfg r.
  (PGConstraints r) =>
  FeatureSingleton cfg ->
  TeamId ->
  LockableFeature cfg ->
  Sem r ()
setDbFeatureImpl sing tid feat =
  patchDbFeatureImpl
    sing
    tid
    ( LockableFeaturePatch
        { status = Just feat.status,
          lockStatus = Just feat.lockStatus,
          config = Just feat.config
        }
    )

patchDbFeatureImpl ::
  forall cfg r.
  (PGConstraints r) =>
  FeatureSingleton cfg ->
  TeamId ->
  LockableFeaturePatch cfg ->
  Sem r ()
patchDbFeatureImpl sing tid patch = case featureSingIsFeature sing of
  Dict -> do
    runStatement
      ( tid,
        featureName @cfg,
        patch.status,
        patch.lockStatus,
        serialiseDbConfig <$> patch.config
      )
      upsertPatch

setFeatureLockStatusImpl ::
  forall cfg r.
  (PGConstraints r) =>
  FeatureSingleton cfg ->
  TeamId ->
  LockStatus ->
  Sem r ()
setFeatureLockStatusImpl sing tid lockStatus = case featureSingIsFeature sing of
  Dict -> do
    runStatement (tid, featureName @cfg, lockStatus) writeLockStatus

getAllDbFeaturesImpl ::
  (PGConstraints r) =>
  TeamId ->
  Sem r AllDbFeaturePatches
getAllDbFeaturesImpl tid = do
  rows <- runStatement tid selectAll
  let m = Map.fromList $ do
        (name, status, lockStatus, config) <- Vector.toList rows
        pure (name, LockableFeaturePatch {..})
  pure $ mkAllDbFeaturePatches m
  where
    mkAllDbFeaturePatches :: Map Text DbFeaturePatch -> AllDbFeaturePatches
    mkAllDbFeaturePatches m = hcpure (Proxy @IsFeatureConfig) $ get m

    get :: forall cfg. (IsFeatureConfig cfg) => Map Text DbFeaturePatch -> K (Maybe DbFeaturePatch) cfg
    get m = K (Map.lookup (featureName @cfg) m)
