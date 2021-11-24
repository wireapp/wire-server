-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Galley.Cassandra.TeamFeatures (interpretTeamFeatureStoreToCassandra) where

import Cassandra
import Data.Id
import Data.Proxy
import Galley.Cassandra.Instances ()
import Galley.Cassandra.Store
import Galley.Data.TeamFeatures
import Galley.Effects.TeamFeatureStore (TeamFeatureStore (..))
import Imports
import Polysemy
import Polysemy.Input
import Wire.API.Team.Feature

getFeatureStatusNoConfig ::
  forall (ps :: IncludePaymentStatus) (a :: TeamFeatureName) m.
  ( MonadClient m,
    FeatureHasNoConfig ps a,
    HasStatusCol a
  ) =>
  Proxy ps ->
  Proxy a ->
  TeamId ->
  m (Maybe (TeamFeatureStatus ps a))
getFeatureStatusNoConfig _ _ tid = do
  let q = query1 select (params LocalQuorum (Identity tid))
  mStatusValue <- (>>= runIdentity) <$> retry x1 q
  pure $ TeamFeatureStatusNoConfig <$> mStatusValue
  where
    select :: PrepQuery R (Identity TeamId) (Identity (Maybe TeamFeatureStatusValue))
    select = fromString $ "select " <> statusCol @a <> " from team_features where team_id = ?"

setFeatureStatusNoConfig ::
  forall (ps :: IncludePaymentStatus) (a :: TeamFeatureName) m.
  ( MonadClient m,
    FeatureHasNoConfig ps a,
    HasStatusCol a
  ) =>
  Proxy ps ->
  Proxy a ->
  TeamId ->
  TeamFeatureStatus ps a ->
  m (TeamFeatureStatus ps a)
setFeatureStatusNoConfig _ _ tid status = do
  let flag = tfwoStatus status
  retry x5 $ write insert (params LocalQuorum (tid, flag))
  pure status
  where
    insert :: PrepQuery W (TeamId, TeamFeatureStatusValue) ()
    insert = fromString $ "insert into team_features (team_id, " <> statusCol @a <> ") values (?, ?)"

getApplockFeatureStatus ::
  forall m.
  (MonadClient m) =>
  TeamId ->
  m (Maybe (TeamFeatureStatus 'WithoutPaymentStatus 'TeamFeatureAppLock))
getApplockFeatureStatus tid = do
  let q = query1 select (params LocalQuorum (Identity tid))
  mTuple <- retry x1 q
  pure $
    mTuple >>= \(mbStatusValue, mbEnforce, mbTimeout) ->
      TeamFeatureStatusWithConfig <$> mbStatusValue <*> (TeamFeatureAppLockConfig <$> mbEnforce <*> mbTimeout)
  where
    select :: PrepQuery R (Identity TeamId) (Maybe TeamFeatureStatusValue, Maybe EnforceAppLock, Maybe Int32)
    select =
      fromString $
        "select " <> statusCol @'TeamFeatureAppLock <> ", app_lock_enforce, app_lock_inactivity_timeout_secs "
          <> "from team_features where team_id = ?"

setApplockFeatureStatus ::
  (MonadClient m) =>
  TeamId ->
  TeamFeatureStatus 'WithoutPaymentStatus 'TeamFeatureAppLock ->
  m (TeamFeatureStatus 'WithoutPaymentStatus 'TeamFeatureAppLock)
setApplockFeatureStatus tid status = do
  let statusValue = tfwcStatus status
      enforce = applockEnforceAppLock . tfwcConfig $ status
      timeout = applockInactivityTimeoutSecs . tfwcConfig $ status
  retry x5 $ write insert (params LocalQuorum (tid, statusValue, enforce, timeout))
  pure status
  where
    insert :: PrepQuery W (TeamId, TeamFeatureStatusValue, EnforceAppLock, Int32) ()
    insert =
      fromString $
        "insert into team_features (team_id, "
          <> statusCol @'TeamFeatureAppLock
          <> ", app_lock_enforce, app_lock_inactivity_timeout_secs) values (?, ?, ?, ?)"

getSelfDeletingMessagesStatus ::
  forall m.
  (MonadClient m) =>
  TeamId ->
  m (Maybe (TeamFeatureStatus 'WithoutPaymentStatus 'TeamFeatureSelfDeletingMessages), Maybe PaymentStatusValue)
getSelfDeletingMessagesStatus tid = do
  let q = query1 select (params LocalQuorum (Identity tid))
  mTuple <- retry x1 q
  pure
    ( mTuple >>= \(mbStatusValue, mbTimeout, _) ->
        TeamFeatureStatusWithConfig <$> mbStatusValue <*> (TeamFeatureSelfDeletingMessagesConfig <$> mbTimeout),
      mTuple >>= \(_, _, mbPaymentStatus) -> mbPaymentStatus
    )
  where
    select :: PrepQuery R (Identity TeamId) (Maybe TeamFeatureStatusValue, Maybe Int32, Maybe PaymentStatusValue)
    select =
      fromString $
        "select "
          <> statusCol @'TeamFeatureSelfDeletingMessages
          <> ", self_deleting_messages_ttl, "
          <> paymentStatusCol @'TeamFeatureSelfDeletingMessages
          <> " from team_features where team_id = ?"

setSelfDeletingMessagesStatus ::
  (MonadClient m) =>
  TeamId ->
  TeamFeatureStatus 'WithoutPaymentStatus 'TeamFeatureSelfDeletingMessages ->
  m (TeamFeatureStatus 'WithoutPaymentStatus 'TeamFeatureSelfDeletingMessages)
setSelfDeletingMessagesStatus tid status = do
  let statusValue = tfwcStatus status
      timeout = sdmEnforcedTimeoutSeconds . tfwcConfig $ status
  retry x5 $ write insert (params LocalQuorum (tid, statusValue, timeout))
  pure status
  where
    insert :: PrepQuery W (TeamId, TeamFeatureStatusValue, Int32) ()
    insert =
      fromString $
        "insert into team_features (team_id, "
          <> statusCol @'TeamFeatureSelfDeletingMessages
          <> ", self_deleting_messages_ttl) "
          <> "values (?, ?, ?)"

setPaymentStatus ::
  forall (a :: TeamFeatureName) m.
  ( MonadClient m,
    HasPaymentStatusCol a
  ) =>
  Proxy a ->
  TeamId ->
  PaymentStatus ->
  m PaymentStatus
setPaymentStatus _ tid (PaymentStatus paymentStatus) = do
  retry x5 $ write insert (params LocalQuorum (tid, paymentStatus))
  pure (PaymentStatus paymentStatus)
  where
    insert :: PrepQuery W (TeamId, PaymentStatusValue) ()
    insert =
      fromString $
        "insert into team_features (team_id, " <> paymentStatusCol @a <> ") values (?, ?)"

getPaymentStatus ::
  forall (a :: TeamFeatureName) m.
  ( MonadClient m,
    MaybeHasPaymentStatusCol a
  ) =>
  Proxy a ->
  TeamId ->
  m (Maybe PaymentStatus)
getPaymentStatus _ tid =
  case maybePaymentStatusCol @a of
    Nothing -> pure Nothing
    Just paymentStatusColName -> do
      let q = query1 select (params LocalQuorum (Identity tid))
      mTuple <- (>>= runIdentity) <$> retry x1 q
      pure $ PaymentStatus <$> mTuple
      where
        select :: PrepQuery R (Identity TeamId) (Identity (Maybe PaymentStatusValue))
        select =
          fromString $
            "select "
              <> paymentStatusColName
              <> " from team_features where team_id = ?"

interpretTeamFeatureStoreToCassandra ::
  Members '[Embed IO, Input ClientState] r =>
  Sem (TeamFeatureStore ': r) a ->
  Sem r a
interpretTeamFeatureStoreToCassandra = interpret $ \case
  GetFeatureStatusNoConfig' ps tfn tid -> embedClient $ getFeatureStatusNoConfig ps tfn tid
  SetFeatureStatusNoConfig' ps tfn tid value -> embedClient $ setFeatureStatusNoConfig ps tfn tid value
  SetPaymentStatus' p tid value -> embedClient $ setPaymentStatus p tid value
  GetPaymentStatus' p tid -> embedClient $ getPaymentStatus p tid
  GetApplockFeatureStatus tid -> embedClient $ getApplockFeatureStatus tid
  SetApplockFeatureStatus tid value -> embedClient $ setApplockFeatureStatus tid value
  GetSelfDeletingMessagesStatus tid -> embedClient $ getSelfDeletingMessagesStatus tid
  SetSelfDeletingMessagesStatus tid value -> embedClient $ setSelfDeletingMessagesStatus tid value
