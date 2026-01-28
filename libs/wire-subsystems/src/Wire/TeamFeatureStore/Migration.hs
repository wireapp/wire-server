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

module Wire.TeamFeatureStore.Migration where

import Cassandra hiding (Value)
import Data.Conduit
import Data.Conduit.List qualified as C
import Hasql.Pool qualified as Hasql
import Imports
import Wire.API.Team.Feature
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.State
import Polysemy.TinyLog
import Prometheus qualified
import System.Logger qualified as Log
import Wire.TeamFeatureStore.Cassandra.Queries qualified as Cql
import Data.Id
import Polysemy.Conc
import Wire.Migration
import Wire.Postgres
import Data.ByteString.Conversion
import Wire.MigrationLock
import Polysemy.Time
import Polysemy.Async


type TeamFeatureRow =  (TeamId, Text, Maybe FeatureStatus, Maybe LockStatus, Maybe DbConfig) 

migrateAllTeamFeatures ::
  ( Member (Input Hasql.Pool) r,
    Member (Embed IO) r,
    Member (Input ClientState) r,
    Member TinyLog r,
    Member (State Int) r,
    Member Async r,
    Member (Error MigrationLockError) r,
    Member Race r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  ConduitM () Void (Sem r) ()
migrateAllTeamFeatures  migOpts migCounter = do
  lift $ info $ Log.msg (Log.val "migrateAllTeamFeatures  ")
  withCount (paginateSem Cql.selectAll (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize id
    .| C.mapM_ (traverse_ (\row@(tid, feat, _, _, _) -> handleErrors (migrateTeamFeature migCounter) (toByteString' (idToText tid <> " - " <> feat)) row))

migrateTeamFeature ::
  ( PGConstraints r,
    Member TinyLog r,
    Member Async r,
    Member (Error MigrationLockError) r,
    Member Race r
  ) =>
  Prometheus.Counter ->
  TeamFeatureRow ->
  Sem r ()
migrateTeamFeature migCounter (tid, feature, _,_,_) = do
  void . withMigrationLocks LockExclusive (Seconds 10) [(tid, feature)] $ do
    -- migrate and delete
    liftIO $ Prometheus.incCounter migCounter
