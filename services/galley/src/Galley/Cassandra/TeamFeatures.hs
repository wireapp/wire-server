{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wwarn #-}

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

import Cassandra hiding (Tagged)
import Data.Constraint
import Data.Default
import Data.Id
import Data.Map qualified as M
import Data.SOP
import Data.Tagged
import Galley.API.Teams.Features.Get
import Galley.Cassandra.Instances ()
import Galley.Cassandra.Store
import Galley.Cassandra.Util
import Galley.Effects.TeamFeatureStore qualified as TFS
import Imports
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog
import Wire.API.Team.Feature
import Wire.API.Team.Feature.TH

interpretTeamFeatureStoreToCassandra ::
  ( Member (Embed IO) r,
    Member (Input ClientState) r,
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

getDbFeature ::
  forall cfg r.
  ( Member (Embed IO) r,
    Member (Input ClientState) r
  ) =>
  FeatureSingleton cfg ->
  TeamId ->
  Sem r (Tagged cfg DbFeature)
getDbFeature sing tid = case featureSingIsFeature sing of
  Dict -> do
    let q :: PrepQuery R (TeamId, Text) (Maybe FeatureStatus, Maybe LockStatus, Maybe DbConfig)
        q = "select status, lock_status, config from team_features_dyn where team = ? and feature = ?"
    (embedClient $ retry x1 $ query1 q (params LocalQuorum (tid, featureName @cfg))) >>= \case
      Nothing -> pure (Tagged def)
      Just (status, lockStatus, config) ->
        pure (Tagged DbFeature {..})

setDbFeature ::
  forall cfg r.
  ( Member (Input ClientState) r,
    Member (Embed IO) r
  ) =>
  FeatureSingleton cfg ->
  TeamId ->
  Tagged cfg DbFeature ->
  Sem r ()
setDbFeature sing tid (Tagged feat) = case featureSingIsFeature sing of
  Dict -> do
    let q :: PrepQuery W (Maybe FeatureStatus, Maybe LockStatus, Maybe DbConfig, TeamId, Text) ()
        q = "update team_features_dyn set status = ?, lock_status = ?, config = ? where team = ? and feature = ?"
    embedClient $
      retry x5 $
        write
          q
          ( params
              LocalQuorum
              ( feat.status,
                feat.lockStatus,
                feat.config,
                tid,
                featureName @cfg
              )
          )

setFeatureLockStatus ::
  forall cfg r.
  ( Member (Input ClientState) r,
    Member (Embed IO) r
  ) =>
  FeatureSingleton cfg ->
  TeamId ->
  Tagged cfg LockStatus ->
  Sem r ()
setFeatureLockStatus sing tid (Tagged lockStatus) = case featureSingIsFeature sing of
  Dict -> do
    let q :: PrepQuery W (LockStatus, TeamId, Text) ()
        q = "update team_features_dyn set  lock_status = ? where team = ? and feature = ?"
    embedClient $
      retry x5 $
        write q (params LocalQuorum (lockStatus, tid, featureName @cfg))

getAllDbFeatures ::
  ( Member (Embed IO) r,
    Member (Input ClientState) r
  ) =>
  TeamId ->
  Sem r (AllFeatures (K DbFeature))
getAllDbFeatures tid = do
  let q :: PrepQuery R (Identity TeamId) (Text, Maybe FeatureStatus, Maybe LockStatus, Maybe DbConfig)
      q = "select feature, status, lock_status, config from team_features_dyn where team = ?"
  rows <- embedClient $ retry x1 $ query q (params LocalQuorum (Identity tid))
  let m = M.fromList $ do
        (name, status, lockStatus, config) <- rows
        pure (name, DbFeature {..})
  pure $ mkAllFeatures m
