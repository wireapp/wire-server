-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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
    setFeatureStatusNoConfig,
    getApplockFeatureStatus,
    setApplockFeatureStatus,
    getSelfDeletingMessagesStatus,
    setSelfDeletingMessagesStatus,
    setPaymentStatus,
    getPaymentStatus,
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
    ( FeatureHasNoConfig a,
      HasStatusCol a
    ) =>
    Proxy a ->
    TeamId ->
    TeamFeatureStore m (Maybe (TeamFeatureStatus a))
  -- the proxy argument makes sure that makeSem below generates type-inference-friendly code
  SetFeatureStatusNoConfig' ::
    forall (a :: TeamFeatureName) m.
    ( FeatureHasNoConfig a,
      HasStatusCol a
    ) =>
    Proxy a ->
    TeamId ->
    TeamFeatureStatus a ->
    TeamFeatureStore m (TeamFeatureStatus a)
  GetApplockFeatureStatus ::
    TeamId ->
    TeamFeatureStore m (Maybe (TeamFeatureStatus 'TeamFeatureAppLock))
  SetApplockFeatureStatus ::
    TeamId ->
    TeamFeatureStatus 'TeamFeatureAppLock ->
    TeamFeatureStore m (TeamFeatureStatus 'TeamFeatureAppLock)
  GetSelfDeletingMessagesStatus ::
    TeamId ->
    TeamFeatureStore m (Maybe (TeamFeatureStatus 'TeamFeatureSelfDeletingMessages))
  SetSelfDeletingMessagesStatus ::
    TeamId ->
    TeamFeatureStatus 'TeamFeatureSelfDeletingMessages ->
    TeamFeatureStore m (TeamFeatureStatus 'TeamFeatureSelfDeletingMessages)
  SetPaymentStatus' ::
    forall (a :: TeamFeatureName) m.
    ( HasPaymentStatusCol a
    ) =>
    Proxy a ->
    TeamId ->
    PaymentStatus ->
    TeamFeatureStore m PaymentStatus
  GetPaymentStatus' ::
    forall (a :: TeamFeatureName) m.
    ( MaybeHasPaymentStatusCol a
    ) =>
    Proxy a ->
    TeamId ->
    TeamFeatureStore m (Maybe PaymentStatus)

makeSem ''TeamFeatureStore

getFeatureStatusNoConfig ::
  forall (a :: TeamFeatureName) r.
  (Member TeamFeatureStore r, FeatureHasNoConfig a, HasStatusCol a) =>
  TeamId ->
  Sem r (Maybe (TeamFeatureStatus a))
getFeatureStatusNoConfig = getFeatureStatusNoConfig' (Proxy @a)

setFeatureStatusNoConfig ::
  forall (a :: TeamFeatureName) r.
  (Member TeamFeatureStore r, FeatureHasNoConfig a, HasStatusCol a) =>
  TeamId ->
  TeamFeatureStatus a ->
  Sem r (TeamFeatureStatus a)
setFeatureStatusNoConfig = setFeatureStatusNoConfig' (Proxy @a)

setPaymentStatus ::
  forall (a :: TeamFeatureName) r.
  (Member TeamFeatureStore r, HasPaymentStatusCol a) =>
  TeamId ->
  PaymentStatus ->
  Sem r PaymentStatus
setPaymentStatus = setPaymentStatus' (Proxy @a)

getPaymentStatus ::
  forall (a :: TeamFeatureName) r.
  (Member TeamFeatureStore r, MaybeHasPaymentStatusCol a) =>
  TeamId ->
  Sem r (Maybe PaymentStatus)
getPaymentStatus = getPaymentStatus' (Proxy @a)
