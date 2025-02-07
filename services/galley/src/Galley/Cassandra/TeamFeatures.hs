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

import Cassandra
import Data.Id
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
    embedClient $ getDbFeature sing tid
  TFS.SetDbFeature sing tid feat -> do
    logEffect "TeamFeatureStore.SetFeatureConfig"
    embedClient $ setDbFeature sing tid feat
  TFS.SetFeatureLockStatus sing tid lock -> do
    logEffect "TeamFeatureStore.SetFeatureLockStatus"
    embedClient $ setFeatureLockStatus sing tid (Tagged lock)
  TFS.GetAllDbFeatures tid -> do
    logEffect "TeamFeatureStore.GetAllTeamFeatures"
    embedClient $ getAllDbFeatures tid

getDbFeature :: (MonadClient m) => FeatureSingleton cfg -> TeamId -> m (DbFeature cfg)
getDbFeature = todo

setDbFeature :: (MonadClient m) => FeatureSingleton cfg -> TeamId -> LockableFeature cfg -> m ()
setDbFeature = todo

setFeatureLockStatus :: (MonadClient m) => FeatureSingleton cfg -> TeamId -> Tagged cfg LockStatus -> m ()
setFeatureLockStatus = todo

getAllDbFeatures :: TeamId -> m (AllFeatures DbFeature)
getAllDbFeatures = todo
