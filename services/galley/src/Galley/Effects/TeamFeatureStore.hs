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
    TeamFeatureStore db m [(TeamId, Maybe (WithStatusNoLock cfg))]
  SetFeatureConfig ::
    FeaturePersistentConstraint db cfg =>
    Proxy cfg ->
    TeamId ->
    WithStatusNoLock cfg ->
    Maybe TeamFeatureTTLValue ->
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
