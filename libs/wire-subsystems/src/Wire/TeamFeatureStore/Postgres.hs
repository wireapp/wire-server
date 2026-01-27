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
import Data.Map qualified as M
import Data.Map qualified as Map
import Data.Proxy
import Data.SOP (K (..), hcpure)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import Wire.API.PostgresMarshall
import Wire.API.Team.Feature
import Wire.API.Team.Feature.TH
import Wire.ConversationStore.Cassandra.Instances ()
import Wire.Postgres
import Wire.TeamFeatureStore

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
    where
      select :: Hasql.Statement (TeamId, Text) (Maybe (Maybe FeatureStatus, Maybe LockStatus, Maybe DbConfig))
      select =
        dimapPG
          [maybeStatement|SELECT 
                            status :: int?,
                            lock_status :: int?,
                            config :: jsonb?
                          FROM team_features
                          WHERE team = ($1 :: uuid) AND feature = ($2 :: text)
                          |]

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
  where
    upsertPatch :: Hasql.Statement (TeamId, Text, Maybe FeatureStatus, Maybe LockStatus, Maybe DbConfig) ()
    upsertPatch =
      lmapPG
        [resultlessStatement|INSERT INTO team_features (team, feature, status, lock_status, config)
                             VALUES ($1 :: uuid, $2 :: text, $3 :: int?, $4 :: int?, $5 :: jsonb?)
                             ON CONFLICT (team, feature) DO UPDATE
                             SET status = COALESCE(EXCLUDED.status, team_features.status),
                                 lock_status = COALESCE(EXCLUDED.lock_status, team_features.lock_status),
                                 config = COALESCE(EXCLUDED.config, team_features.config)
                            |]

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
  where
    writeLockStatus :: Hasql.Statement (TeamId, Text, LockStatus) ()
    writeLockStatus =
      lmapPG
        [resultlessStatement|INSERT INTO team_features (team, feature, lock_status)
                             VALUES ($1 :: uuid, $2 :: text, $3 :: int)
                             ON CONFLICT (team, feature) DO UPDATE
                             SET lock_status = EXCLUDED.lock_status
                            |]

getAllDbFeaturesImpl ::
  (PGConstraints r) =>
  TeamId ->
  Sem r AllDbFeaturePatches
getAllDbFeaturesImpl tid = do
  rows <- runStatement tid selectAll
  let m = M.fromList $ do
        (name, status, lockStatus, config) <- Vector.toList rows
        pure (name, LockableFeaturePatch {..})
  pure $ mkAllDbFeaturePatches m
  where
    mkAllDbFeaturePatches :: Map Text DbFeaturePatch -> AllDbFeaturePatches
    mkAllDbFeaturePatches m = hcpure (Proxy @IsFeatureConfig) $ get m

    get :: forall cfg. (IsFeatureConfig cfg) => Map Text DbFeaturePatch -> K (Maybe DbFeaturePatch) cfg
    get m = K (Map.lookup (featureName @cfg) m)

    selectAll :: Hasql.Statement TeamId (Vector (Text, Maybe FeatureStatus, Maybe LockStatus, Maybe DbConfig))
    selectAll =
      dimapPG
        [vectorStatement|SELECT (feature :: text),
                                 (status :: int?),
                                 (lock_status :: int?),
                                 (config :: jsonb?)
                          FROM team_features
                          WHERE team = ($1 :: uuid)
                        |]
