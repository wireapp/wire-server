{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Data.Default
import Data.Id
import Data.Map qualified as M
import Data.Text.Lazy qualified as LT
import Galley.API.Error
import Galley.API.Teams.Features.Get
import Galley.Cassandra.FeatureTH
import Galley.Cassandra.GetAllTeamFeatures
import Galley.Cassandra.Instances ()
import Galley.Cassandra.MakeFeature
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
    getDbFeature sing tid
  TFS.SetDbFeature sing tid feat -> do
    logEffect "TeamFeatureStore.SetFeatureConfig"
    setDbFeature sing tid feat
  TFS.SetFeatureLockStatus sing tid lock -> do
    logEffect "TeamFeatureStore.SetFeatureLockStatus"
    setFeatureLockStatus sing tid (Tagged lock)
  TFS.GetAllDbFeatures tid -> do
    logEffect "TeamFeatureStore.GetAllTeamFeatures"
    getAllDbFeatures tid
  TFS.SetMigrationState tid state -> do
    logEffect "TeamFeatureStore.SetMigrationState"
    setMigrationState tid state

setMigrationState ::
  ( Member (Input ClientState) r,
    Member (Embed IO) r
  ) =>
  TeamId ->
  TeamFeatureMigrationState ->
  Sem r ()
setMigrationState tid state = embedClient $ do
  retry x5 $
    write cql (params LocalQuorum (state, tid))
  where
    cql :: PrepQuery W (TeamFeatureMigrationState, TeamId) ()
    cql = "UPDATE team_features SET migration_state = ? WHERE team_id = ?"

getMigrationState ::
  ( Member (Input ClientState) r,
    Member (Embed IO) r
  ) =>
  TeamId ->
  Sem r TeamFeatureMigrationState
getMigrationState tid = embedClient $ do
  fromMaybe def . (runIdentity =<<) <$> retry x1 (query1 cql (params LocalQuorum (Identity tid)))
  where
    cql :: PrepQuery R (Identity TeamId) (Identity (Maybe TeamFeatureMigrationState))
    cql = "SELECT migration_state FROM team_features WHERE team_id = ?"

getDbFeature ::
  ( Member (Input ClientState) r,
    Member (Embed IO) r,
    Member (Error InternalError) r
  ) =>
  FeatureSingleton cfg ->
  TeamId ->
  Sem r (DbFeature cfg)
getDbFeature cfg tid = do
  migrationState <- getMigrationState tid
  case migrationState of
    MigrationCompleted -> getDbFeatureDyn cfg tid
    _ -> embedClient $ $(featureCases [|fetchFeature|]) cfg tid

setDbFeature ::
  ( Member (Input ClientState) r,
    Member (Error InternalError) r,
    Member (Embed IO) r
  ) =>
  FeatureSingleton cfg ->
  TeamId ->
  LockableFeature cfg ->
  Sem r ()
setDbFeature feature tid cfg = do
  migrationState <- getMigrationState tid
  case migrationState of
    MigrationNotStarted -> embedClient $ $(featureCases [|storeFeature|]) feature tid cfg
    MigrationInProgress -> readOnlyError
    MigrationCompleted -> setDbFeatureDyn feature tid cfg

setFeatureLockStatus ::
  ( Member (Input ClientState) r,
    Member (Error InternalError) r,
    Member (Embed IO) r
  ) =>
  FeatureSingleton cfg ->
  TeamId ->
  Tagged cfg LockStatus ->
  Sem r ()
setFeatureLockStatus feature tid ls = do
  migrationState <- getMigrationState tid
  case migrationState of
    MigrationNotStarted -> embedClient $ $(featureCases [|storeFeatureLockStatus|]) feature tid ls
    MigrationInProgress -> readOnlyError
    MigrationCompleted -> setFeatureLockStatusDyn feature tid ls

getAllDbFeatures ::
  ( Member (Input ClientState) r,
    Member (Error InternalError) r,
    Member (Embed IO) r
  ) =>
  TeamId ->
  Sem r (AllFeatures DbFeature)
getAllDbFeatures tid = do
  migrationState <- getMigrationState tid
  case migrationState of
    MigrationCompleted -> getAllDbFeaturesDyn tid
    _ -> embedClient $ getAllDbFeaturesLegacy tid

readOnlyError :: (Member (Error InternalError) r) => Sem r a
readOnlyError = throw (InternalErrorWithDescription "migration in progress")

--------------------------------------------------------------------------------
-- Dynamic features

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
setDbFeatureDyn sing tid feat = case featureSingIsFeature sing of
  Dict -> do
    let q :: PrepQuery W (Maybe FeatureStatus, Maybe LockStatus, Maybe DbConfig, TeamId, Text) ()
        q = "update team_features_dyn set status = ?, lock_status = ?, config = ? where team = ? and feature = ?"
        dbFeat = serialiseDbFeature feat
    embedClient $
      retry x5 $
        write
          q
          ( params
              LocalQuorum
              ( dbFeat.status,
                dbFeat.lockStatus,
                dbFeat.config,
                tid,
                featureName @cfg
              )
          )

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
