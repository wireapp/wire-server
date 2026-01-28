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

module Wire.TeamFeatureStore.Migrating where

import Cassandra (ClientState)
import Data.Constraint
import Data.Id
import Data.SOP (K (..), hzipWith)
import Imports
import Polysemy
import Polysemy.Async
import Polysemy.Conc.Effect.Race
import Polysemy.Error
import Polysemy.Input
import Polysemy.Time
import Polysemy.TinyLog
import Wire.API.Team.Feature
import Wire.API.Team.Feature.TH
import Wire.MigrationLock
import Wire.Postgres
import Wire.TeamFeatureStore
import Wire.TeamFeatureStore.Cassandra
import Wire.TeamFeatureStore.Postgres

interpretTeamFeatureStoreToCassandraAndPostgres ::
  ( PGConstraints r,
    Member (Input ClientState) r,
    Member TinyLog r,
    Member Async r,
    Member Race r,
    Member (Error MigrationLockError) r
  ) =>
  Sem (TeamFeatureStore ': r) a ->
  Sem r a
interpretTeamFeatureStoreToCassandraAndPostgres = interpret $ \case
  GetDbFeature sing tid -> getDbFeatureImpl sing tid
  GetAllDbFeatures tid -> getAllDbFeaturesImpl tid
  SetDbFeature sing tid feat -> setDbFeatureImpl sing tid feat
  SetFeatureLockStatus sing tid lock -> setFeatureLockStatusImpl sing tid lock
  PatchDbFeature sing tid feat -> patchDbFeatureImpl sing tid feat

-- Read path under lock:
-- - Prefer Postgres; fallback to Cassandra; if neither exists → Nothing.
getDbFeatureImpl ::
  forall cfg r.
  ( PGConstraints r,
    Member TinyLog r,
    Member Async r,
    Member Race r,
    Member (Input ClientState) r,
    Member (Error MigrationLockError) r
  ) =>
  FeatureSingleton cfg ->
  TeamId ->
  Sem r (Maybe DbFeaturePatch)
getDbFeatureImpl sing tid = case featureSingIsFeature sing of
  Dict ->
    withSharedLock (tid, featureName @cfg) $ do
      mFeature <- interpretTeamFeatureStoreToPostgres $ send (GetDbFeature sing tid)
      maybe
        (interpretTeamFeatureStoreToCassandra $ send (GetDbFeature sing tid))
        (pure . Just)
        mFeature

-- Read all feature, no lock:
-- - Read all features from Postgres.
-- - Read all features from Cassandra.
-- - Merge per‑feature with precedence: Postgres wins, fallback to Cassandra, otherwise Nothing.
getAllDbFeaturesImpl ::
  forall r.
  ( PGConstraints r,
    Member (Input ClientState) r
  ) =>
  TeamId ->
  Sem r AllDbFeaturePatches
getAllDbFeaturesImpl tid = do
  mergeDbFeaturePatches
    <$> interpretTeamFeatureStoreToPostgres (send (GetAllDbFeatures tid))
    <*> interpretTeamFeatureStoreToCassandra (send (GetAllDbFeatures tid))
  where
    mergeDbFeaturePatches = hzipWith $ \(K psqlPatch) (K cassPatch) -> K (psqlPatch <|> cassPatch)

setDbFeatureImpl ::
  forall cfg r.
  ( PGConstraints r,
    Member TinyLog r,
    Member Async r,
    Member Race r,
    Member (Input ClientState) r,
    Member (Error MigrationLockError) r
  ) =>
  FeatureSingleton cfg ->
  TeamId ->
  LockableFeature cfg ->
  Sem r ()
setDbFeatureImpl sing tid feat = case featureSingIsFeature sing of
  Dict ->
    withWritePathUnderLock sing tid psql cass
    where
      psql = interpretTeamFeatureStoreToPostgres $ send (SetDbFeature sing tid feat)
      cass = interpretTeamFeatureStoreToCassandra $ send (SetDbFeature sing tid feat)

setFeatureLockStatusImpl ::
  forall cfg r.
  ( PGConstraints r,
    Member TinyLog r,
    Member Async r,
    Member Race r,
    Member (Input ClientState) r,
    Member (Error MigrationLockError) r
  ) =>
  FeatureSingleton cfg ->
  TeamId ->
  LockStatus ->
  Sem r ()
setFeatureLockStatusImpl sing tid lock = case featureSingIsFeature sing of
  Dict ->
    withWritePathUnderLock sing tid psql cass
    where
      psql = interpretTeamFeatureStoreToPostgres $ send (SetFeatureLockStatus sing tid lock)
      cass = interpretTeamFeatureStoreToCassandra $ send (SetFeatureLockStatus sing tid lock)

patchDbFeatureImpl ::
  forall cfg r.
  ( PGConstraints r,
    Member TinyLog r,
    Member Async r,
    Member Race r,
    Member (Input ClientState) r,
    Member (Error MigrationLockError) r
  ) =>
  FeatureSingleton cfg ->
  TeamId ->
  LockableFeaturePatch cfg ->
  Sem r ()
patchDbFeatureImpl sing tid feat = case featureSingIsFeature sing of
  Dict ->
    withWritePathUnderLock sing tid psql cass
    where
      psql = interpretTeamFeatureStoreToPostgres $ send (PatchDbFeature sing tid feat)
      cass = interpretTeamFeatureStoreToCassandra $ send (PatchDbFeature sing tid feat)

-- Write path under lock:
-- 1. Check Postgres for row.
-- 2. If exists -> write Postgres.
-- 3. Else check Cassandra.
-- 4. If exists -> write Cassandra.
-- 5. Else → write Postgres (new canonical row).
withWritePathUnderLock ::
  forall cfg r a.
  ( PGConstraints r,
    Member TinyLog r,
    Member Async r,
    Member Race r,
    Member (Input ClientState) r,
    Member (Error MigrationLockError) r,
    IsFeatureConfig cfg
  ) =>
  FeatureSingleton cfg ->
  TeamId ->
  Sem r a ->
  Sem r a ->
  Sem r a
withWritePathUnderLock sing tid psql cass =
  withSharedLock (tid, featureName @cfg) $ do
    mFeaturePsql <- interpretTeamFeatureStoreToPostgres $ send (GetDbFeature sing tid)
    if isJust mFeaturePsql
      then psql
      else do
        mFeatureCql <- interpretTeamFeatureStoreToCassandra $ send (GetDbFeature sing tid)
        if isJust mFeatureCql then cass else psql

withSharedLock ::
  ( PGConstraints r,
    Member TinyLog r,
    Member Async r,
    Member Race r,
    Member (Error MigrationLockError) r,
    MigrationLockable x
  ) =>
  x -> Sem r a -> Sem r a
withSharedLock lockable = withMigrationLocks LockShared (MilliSeconds 500) [lockable]
