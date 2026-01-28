{-# LANGUAGE RecordWildCards #-}

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

module Wire.TeamFeatureStore.Cassandra (interpretTeamFeatureStoreToCassandra) where

import Cassandra
import Data.Constraint
import Data.Id
import Data.Map qualified as Map
import Data.Proxy
import Data.SOP (K (..), hcpure)
import Imports
import Polysemy
import Polysemy.Input
import Wire.API.Team.Feature
import Wire.API.Team.Feature.TH
import Wire.TeamFeatureStore (AllDbFeaturePatches, DbFeaturePatch, TeamFeatureStore (..))
import Wire.TeamFeatureStore.Cassandra.Queries
import Wire.Util

interpretTeamFeatureStoreToCassandra ::
  ( Member (Embed IO) r,
    Member (Input ClientState) r
  ) =>
  Sem (TeamFeatureStore ': r) a ->
  Sem r a
interpretTeamFeatureStoreToCassandra = interpret $ \case
  GetDbFeature sing tid -> do
    getDbFeatureImpl sing tid
  SetDbFeature sing tid feat -> do
    setDbFeatureImpl sing tid feat
  SetFeatureLockStatus sing tid lock -> do
    setFeatureLockStatusImpl sing tid (Tagged lock)
  GetAllDbFeatures tid -> do
    getAllDbFeaturesImpl tid
  PatchDbFeature sing tid feat -> do
    patchDbFeatureImpl sing tid feat

getDbFeatureImpl ::
  forall cfg r.
  ( Member (Input ClientState) r,
    Member (Embed IO) r
  ) =>
  FeatureSingleton cfg ->
  TeamId ->
  Sem r (Maybe DbFeaturePatch)
getDbFeatureImpl sing tid = case featureSingIsFeature sing of
  Dict -> do
    mRow <- (embedClientInput (retry x1 $ query1 select (params LocalQuorum (tid, featureName @cfg))))
    pure $ (\(status, lockStatus, config) -> LockableFeaturePatch {..}) <$> mRow

setDbFeatureImpl ::
  forall cfg r.
  ( Member (Input ClientState) r,
    Member (Embed IO) r
  ) =>
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
  ( Member (Input ClientState) r,
    Member (Embed IO) r
  ) =>
  FeatureSingleton cfg ->
  TeamId ->
  LockableFeaturePatch cfg ->
  Sem r ()
patchDbFeatureImpl sing tid patch = case featureSingIsFeature sing of
  Dict -> embedClientInput $ do
    retry x5 . batch $ do
      setType BatchLogged
      setConsistency LocalQuorum
      for_ patch.status $ \featureStatus -> addPrepQuery writeStatus (featureStatus, tid, featureName @cfg)
      for_ patch.lockStatus $ \lockStatus -> addPrepQuery writeLockStatus (lockStatus, tid, featureName @cfg)
      for_ patch.config $ \config -> addPrepQuery writeConfig (serialiseDbConfig config, tid, featureName @cfg)

setFeatureLockStatusImpl ::
  forall cfg r.
  ( Member (Input ClientState) r,
    Member (Embed IO) r
  ) =>
  FeatureSingleton cfg ->
  TeamId ->
  Tagged cfg LockStatus ->
  Sem r ()
setFeatureLockStatusImpl sing tid (Tagged lockStatus) = case featureSingIsFeature sing of
  Dict -> do
    embedClientInput $
      retry x5 $
        write writeLockStatus (params LocalQuorum (lockStatus, tid, featureName @cfg))

getAllDbFeaturesImpl ::
  ( Member (Embed IO) r,
    Member (Input ClientState) r
  ) =>
  TeamId ->
  Sem r AllDbFeaturePatches
getAllDbFeaturesImpl tid = do
  rows <- embedClientInput $ retry x1 $ query selectAllByTeam (params LocalQuorum (Identity tid))
  let m = Map.fromList $ do
        (name, status, lockStatus, config) <- rows
        pure (name, LockableFeaturePatch {..})
  pure $ mkAllDbFeaturePatches m
  where
    mkAllDbFeaturePatches :: Map Text DbFeaturePatch -> AllDbFeaturePatches
    mkAllDbFeaturePatches m = hcpure (Proxy @IsFeatureConfig) $ get m

    get :: forall cfg. (IsFeatureConfig cfg) => Map Text DbFeaturePatch -> K (Maybe DbFeaturePatch) cfg
    get m = K (Map.lookup (featureName @cfg) m)
