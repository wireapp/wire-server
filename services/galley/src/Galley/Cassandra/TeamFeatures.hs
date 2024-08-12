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
    getFeatureConfigMulti,
    getAllFeatureConfigsForServer,
  )
where

import Cassandra
import Data.Id
import Galley.API.Teams.Features.Get
import Galley.Cassandra.FeatureTH
import Galley.Cassandra.GetAllTeamFeatureConfigs
import Galley.Cassandra.Instances ()
import Galley.Cassandra.MakeFeature
import Galley.Cassandra.Store
import Galley.Cassandra.Util
import Galley.Effects.TeamFeatureStore qualified as TFS
import Imports
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog
import UnliftIO.Async (pooledMapConcurrentlyN)
import Wire.API.Team.Feature

interpretTeamFeatureStoreToCassandra ::
  ( Member (Embed IO) r,
    Member (Input ClientState) r,
    Member TinyLog r
  ) =>
  Sem (TFS.TeamFeatureStore ': r) a ->
  Sem r a
interpretTeamFeatureStoreToCassandra = interpret $ \case
  TFS.GetFeatureConfig sing tid -> do
    logEffect "TeamFeatureStore.GetFeatureConfig"
    embedClient $ getFeatureConfig sing tid
  TFS.GetFeatureConfigMulti sing tids -> do
    logEffect "TeamFeatureStore.GetFeatureConfigMulti"
    embedClient $ getFeatureConfigMulti sing tids
  TFS.SetFeatureConfig sing tid feat -> do
    logEffect "TeamFeatureStore.SetFeatureConfig"
    embedClient $ setFeatureConfig sing tid feat
  TFS.SetFeatureLockStatus sing tid lock -> do
    logEffect "TeamFeatureStore.SetFeatureLockStatus"
    embedClient $ setFeatureLockStatus sing tid (Tagged lock)
  TFS.GetAllFeatureConfigs tid -> do
    logEffect "TeamFeatureStore.GetAllFeatureConfigs"
    embedClient $ getAllFeatureConfigs tid

getFeatureConfigMulti ::
  forall cfg m.
  (MonadClient m, MonadUnliftIO m) =>
  FeatureSingleton cfg ->
  [TeamId] ->
  m [(TeamId, DbFeature cfg)]
getFeatureConfigMulti proxy =
  pooledMapConcurrentlyN 8 (\tid -> getFeatureConfig proxy tid <&> (tid,))

getFeatureConfig :: (MonadClient m) => FeatureSingleton cfg -> TeamId -> m (DbFeature cfg)
getFeatureConfig = $(featureCases [|fetchFeature|])

setFeatureConfig :: (MonadClient m) => FeatureSingleton cfg -> TeamId -> LockableFeature cfg -> m ()
setFeatureConfig = $(featureCases [|storeFeature|])

setFeatureLockStatus :: (MonadClient m) => FeatureSingleton cfg -> TeamId -> Tagged cfg LockStatus -> m ()
setFeatureLockStatus = $(featureCases [|storeFeatureLockStatus|])
