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

module Galley.Effects.TeamFeatureStore
  ( TeamFeatureStore (..),
    FeaturePersistentConstraint,
    getFeatureConfig,
    getFeatureConfigMulti,
    setFeatureConfig,
    getFeatureLockStatus,
    setFeatureLockStatus,
  )
where

import Data.Id
import Data.Kind (Constraint)
import Data.Proxy
import Imports
import Polysemy
import Wire.API.Team.Feature

type family FeaturePersistentConstraint db :: * -> Constraint

data TeamFeatureStore db m a where
  -- the proxy argument makes sure that makeSem below generates type-inference-friendly code
  GetFeatureConfig ::
    FeaturePersistentConstraint db cfg =>
    Proxy cfg ->
    TeamId ->
    TeamFeatureStore db m (Maybe (WithStatusNoLock cfg))
  GetFeatureConfigMulti ::
    FeaturePersistentConstraint db cfg =>
    Proxy cfg ->
    [TeamId] ->
<<<<<<< HEAD
    TeamFeatureStore m [(TeamId, TeamFeatureStatusValue, Int64)]
  GetFeatureStatusNoConfigAndLockStatus' ::
    forall (a :: TeamFeatureName) m.
    (FeatureHasNoConfig 'WithoutLockStatus a, HasStatusCol a, HasLockStatusCol a) =>
    Proxy a ->
    TeamId ->
    TeamFeatureStore m (Maybe (TeamFeatureStatus 'WithoutLockStatus a), Maybe LockStatusValue)
  -- the proxy argument makes sure that makeSem below generates type-inference-friendly code
  SetFeatureStatusNoConfig' ::
    forall (a :: TeamFeatureName) m.
    ( FeatureHasNoConfig 'WithoutLockStatus a,
      HasStatusCol a
    ) =>
    Proxy a ->
    TeamId ->
    TeamFeatureStatus 'WithoutLockStatus a ->
    Maybe TeamFeatureTTLValue ->
    TeamFeatureStore m (TeamFeatureStatus 'WithoutLockStatus a)
  GetApplockFeatureStatus ::
    TeamId ->
    TeamFeatureStore m (Maybe (TeamFeatureStatus ps 'TeamFeatureAppLock))
  SetApplockFeatureStatus ::
    TeamId ->
    TeamFeatureStatus 'WithoutLockStatus 'TeamFeatureAppLock ->
    TeamFeatureStore m (TeamFeatureStatus 'WithoutLockStatus 'TeamFeatureAppLock)
  GetSelfDeletingMessagesStatus ::
=======
    TeamFeatureStore db m [(TeamId, Maybe (WithStatusNoLock cfg))]
  SetFeatureConfig ::
    FeaturePersistentConstraint db cfg =>
    Proxy cfg ->
>>>>>>> 447bf419f (Refactor features)
    TeamId ->
    WithStatusNoLock cfg ->
    TeamFeatureStore db m ()
  GetFeatureLockStatus ::
    FeaturePersistentConstraint db cfg =>
    Proxy cfg ->
    TeamId ->
    TeamFeatureStore db m (Maybe LockStatus)
  SetFeatureLockStatus ::
    FeaturePersistentConstraint db cfg =>
    Proxy cfg ->
    TeamId ->
    LockStatus ->
    TeamFeatureStore db m ()

makeSem ''TeamFeatureStore
<<<<<<< HEAD

getFeatureStatusNoConfig ::
  forall (a :: TeamFeatureName) r.
  (Member TeamFeatureStore r, FeatureHasNoConfig 'WithoutLockStatus a, HasStatusCol a) =>
  TeamId ->
  Sem r (Maybe (TeamFeatureStatus 'WithoutLockStatus a))
getFeatureStatusNoConfig = getFeatureStatusNoConfig' (Proxy @a)

getFeatureStatusNoConfigAndLockStatus ::
  forall (a :: TeamFeatureName) r.
  (Member TeamFeatureStore r, FeatureHasNoConfig 'WithoutLockStatus a, HasStatusCol a, HasLockStatusCol a) =>
  TeamId ->
  Sem r (Maybe (TeamFeatureStatus 'WithoutLockStatus a), Maybe LockStatusValue)
getFeatureStatusNoConfigAndLockStatus = getFeatureStatusNoConfigAndLockStatus' (Proxy @a)

setFeatureStatusNoConfig ::
  forall (a :: TeamFeatureName) r.
  (Member TeamFeatureStore r, FeatureHasNoConfig 'WithoutLockStatus a, HasStatusCol a) =>
  TeamId ->
  TeamFeatureStatus 'WithoutLockStatus a ->
  Maybe TeamFeatureTTLValue ->
  Sem r (TeamFeatureStatus 'WithoutLockStatus a)
setFeatureStatusNoConfig = setFeatureStatusNoConfig' (Proxy @a)

setLockStatus ::
  forall (a :: TeamFeatureName) r.
  (Member TeamFeatureStore r, HasLockStatusCol a) =>
  TeamId ->
  LockStatus ->
  Sem r LockStatus
setLockStatus = setLockStatus' (Proxy @a)

getLockStatus ::
  forall (a :: TeamFeatureName) r.
  (Member TeamFeatureStore r, MaybeHasLockStatusCol a) =>
  TeamId ->
  Sem r (Maybe LockStatusValue)
getLockStatus = getLockStatus' (Proxy @a)
=======
>>>>>>> 447bf419f (Refactor features)
