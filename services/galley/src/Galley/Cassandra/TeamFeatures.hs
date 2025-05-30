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

module Galley.Cassandra.TeamFeatures
  ( interpretTeamFeatureStoreToCassandra,
    getAllTeamFeaturesForServer,
  )
where

import Cassandra
import Data.Aeson.Types qualified as A
import Data.Constraint
import Data.Id
import Data.Map qualified as M
import Data.Text.Lazy qualified as LT
import Galley.API.Error
import Galley.API.Teams.Features.Get
import Galley.Cassandra.Instances ()
import Galley.Cassandra.Store
import Galley.Cassandra.Util
import Galley.Effects.TeamFeatureStore qualified as TFS
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog
import Wire.API.Team.Feature
import Wire.API.Team.Feature.TH

interpretTeamFeatureStoreToCassandra ::
  ( Member (Embed IO) r,
    Member (Input ClientState) r,
    Member (Error InternalError) r,
    Member TinyLog r
  ) =>
  Sem (TFS.TeamFeatureStore ': r) a ->
  Sem r a
interpretTeamFeatureStoreToCassandra = interpret $ \case
  TFS.GetDbFeature sing tid -> do
    logEffect "TeamFeatureStore.GetFeatureConfig"
    getDbFeatureDyn sing tid
  TFS.SetDbFeature sing tid feat -> do
    logEffect "TeamFeatureStore.SetFeatureConfig"
    setDbFeatureDyn sing tid feat
  TFS.SetFeatureLockStatus sing tid lock -> do
    logEffect "TeamFeatureStore.SetFeatureLockStatus"
    setFeatureLockStatusDyn sing tid (Tagged lock)
  TFS.GetAllDbFeatures tid -> do
    logEffect "TeamFeatureStore.GetAllTeamFeatures"
    getAllDbFeaturesDyn tid
  TFS.PatchDbFeature sing tid feat -> do
    logEffect "TeamFeatureStore.PatchDbFeature"
    patchDbFeatureDyn sing tid feat

getDbFeatureDyn ::
  forall cfg r.
  ( Member (Input ClientState) r,
    Member (Embed IO) r,
    Member (Error InternalError) r
  ) =>
  FeatureSingleton cfg ->
  TeamId ->
  Sem r (DbFeature cfg)
getDbFeatureDyn sing tid = case featureSingIsFeature sing of
  Dict -> do
    let q :: PrepQuery R (TeamId, Text) (Maybe FeatureStatus, Maybe LockStatus, Maybe DbConfig)
        q = "select status, lock_status, config from team_features_dyn where team = ? and feature = ?"
    embedClient (retry x1 $ query1 q (params LocalQuorum (tid, featureName @cfg))) >>= \case
      Nothing -> pure mempty
      Just (status, lockStatus, config) ->
        runFeatureParser . parseDbFeature $
          LockableFeaturePatch {..}

setDbFeatureDyn ::
  forall cfg r.
  ( Member (Input ClientState) r,
    Member (Embed IO) r
  ) =>
  FeatureSingleton cfg ->
  TeamId ->
  LockableFeature cfg ->
  Sem r ()
setDbFeatureDyn sing tid feat =
  patchDbFeatureDyn
    sing
    tid
    ( LockableFeaturePatch
        { status = Just feat.status,
          lockStatus = Just feat.lockStatus,
          config = Just feat.config
        }
    )

patchDbFeatureDyn ::
  forall cfg r.
  ( Member (Input ClientState) r,
    Member (Embed IO) r
  ) =>
  FeatureSingleton cfg ->
  TeamId ->
  LockableFeaturePatch cfg ->
  Sem r ()
patchDbFeatureDyn sing tid patch = case featureSingIsFeature sing of
  Dict -> embedClient $ do
    retry x5 . batch $ do
      setType BatchLogged
      setConsistency LocalQuorum
      for_ patch.status $ \featureStatus -> addPrepQuery writeStatus (featureStatus, tid, featureName @cfg)
      for_ patch.lockStatus $ \lockStatus -> addPrepQuery writeLockStatus (lockStatus, tid, featureName @cfg)
      for_ patch.config $ \config -> addPrepQuery writeConfig (serialiseDbConfig config, tid, featureName @cfg)
  where
    writeStatus :: PrepQuery W (FeatureStatus, TeamId, Text) ()
    writeStatus = "update team_features_dyn set status = ? where team = ? and feature = ?"

    writeLockStatus :: PrepQuery W (LockStatus, TeamId, Text) ()
    writeLockStatus = "update team_features_dyn set lock_status = ? where team = ? and feature = ?"

    writeConfig :: PrepQuery W (DbConfig, TeamId, Text) ()
    writeConfig = "update team_features_dyn set config = ? where team = ? and feature = ?"

setFeatureLockStatusDyn ::
  forall cfg r.
  ( Member (Input ClientState) r,
    Member (Embed IO) r
  ) =>
  FeatureSingleton cfg ->
  TeamId ->
  Tagged cfg LockStatus ->
  Sem r ()
setFeatureLockStatusDyn sing tid (Tagged lockStatus) = case featureSingIsFeature sing of
  Dict -> do
    let q :: PrepQuery W (LockStatus, TeamId, Text) ()
        q = "update team_features_dyn set  lock_status = ? where team = ? and feature = ?"
    embedClient $
      retry x5 $
        write q (params LocalQuorum (lockStatus, tid, featureName @cfg))

getAllDbFeaturesDyn ::
  ( Member (Embed IO) r,
    Member (Error InternalError) r,
    Member (Input ClientState) r
  ) =>
  TeamId ->
  Sem r (AllFeatures DbFeature)
getAllDbFeaturesDyn tid = do
  let q :: PrepQuery R (Identity TeamId) (Text, Maybe FeatureStatus, Maybe LockStatus, Maybe DbConfig)
      q = "select feature, status, lock_status, config from team_features_dyn where team = ?"
  rows <- embedClient $ retry x1 $ query q (params LocalQuorum (Identity tid))
  let m = M.fromList $ do
        (name, status, lockStatus, config) <- rows
        pure (name, LockableFeaturePatch {..})
  runFeatureParser $ mkAllFeatures m

runFeatureParser ::
  forall r a.
  (Member (Error InternalError) r) =>
  A.Parser a ->
  Sem r a
runFeatureParser p =
  mapError (InternalErrorWithDescription . LT.pack)
    . fromEither
    $ A.parseEither (const p) ()
