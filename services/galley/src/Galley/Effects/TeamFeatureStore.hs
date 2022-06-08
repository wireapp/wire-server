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
    getFeatureStatusNoConfig,
    getFeatureStatusNoConfigAndLockStatus,
    setFeatureStatusNoConfig,
    getFeatureStatusNoConfigMulti,
    getApplockFeatureStatus,
    setApplockFeatureStatus,
    getSelfDeletingMessagesStatus,
    setSelfDeletingMessagesStatus,
    setLockStatus,
    getLockStatus,
  )
where

import Data.Id
import Data.Proxy
import Galley.Data.TeamFeatures
import Imports
import Polysemy
import Wire.API.Team.Feature

data TeamFeatureStore m a where
  -- the proxy argument makes sure that makeSem below generates type-inference-friendly code
  GetFeatureStatusNoConfig' ::
    forall (a :: TeamFeatureName) m.
    ( FeatureHasNoConfig 'WithoutLockStatus a,
      HasStatusCol a
    ) =>
    Proxy a ->
    TeamId ->
    TeamFeatureStore m (Maybe (TeamFeatureStatus 'WithoutLockStatus a))
  -- | Returns only teams which have a status stored.
  --
  -- the proxy argument makes sure that makeSem below generates type-inference-friendly code
  GetFeatureStatusNoConfigMulti ::
    forall (a :: TeamFeatureName) m.
    ( FeatureHasNoConfig 'WithoutLockStatus a,
      HasStatusCol a
    ) =>
    Proxy a ->
    [TeamId] ->
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
    TeamId ->
    TeamFeatureStore m (Maybe (TeamFeatureStatus 'WithoutLockStatus 'TeamFeatureSelfDeletingMessages), Maybe LockStatusValue)
  SetSelfDeletingMessagesStatus ::
    TeamId ->
    TeamFeatureStatus 'WithoutLockStatus 'TeamFeatureSelfDeletingMessages ->
    TeamFeatureStore m (TeamFeatureStatus 'WithoutLockStatus 'TeamFeatureSelfDeletingMessages)
  SetLockStatus' ::
    forall (a :: TeamFeatureName) m.
    ( HasLockStatusCol a
    ) =>
    Proxy a ->
    TeamId ->
    LockStatus ->
    TeamFeatureStore m LockStatus
  GetLockStatus' ::
    forall (a :: TeamFeatureName) m.
    ( MaybeHasLockStatusCol a
    ) =>
    Proxy a ->
    TeamId ->
    TeamFeatureStore m (Maybe LockStatusValue)

makeSem ''TeamFeatureStore

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
