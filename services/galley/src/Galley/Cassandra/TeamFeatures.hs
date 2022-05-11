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

getFeatureStatusNoConfigAndLockStatus ::
  forall (a :: TeamFeatureName) m.
  (MonadClient m, FeatureHasNoConfig 'WithoutLockStatus a, HasStatusCol a, HasLockStatusCol a) =>
  Proxy a ->
  TeamId ->
  m (Maybe (TeamFeatureStatus 'WithoutLockStatus a), Maybe LockStatusValue)
getFeatureStatusNoConfigAndLockStatus _ tid = do
  let q = query1 select (params LocalQuorum (Identity tid))
  mTuple <- retry x1 q
  pure (mTuple >>= (fmap TeamFeatureStatusNoConfig . fst), mTuple >>= snd)
  where
    select :: PrepQuery R (Identity TeamId) (Maybe TeamFeatureStatusValue, Maybe LockStatusValue)
    select =
      fromString $
        "select "
          <> statusCol @a
          <> ", "
          <> lockStatusCol @a
          <> " from team_features where team_id = ?"

getFeatureStatusNoConfig ::
  forall (a :: TeamFeatureName) m.
  ( MonadClient m,
    FeatureHasNoConfig 'WithoutLockStatus a,
    HasStatusCol a
  ) =>
  Proxy a ->
  TeamId ->
  m (Maybe (TeamFeatureStatus 'WithoutLockStatus a))
getFeatureStatusNoConfig _ tid = do
  let q = query1 select (params LocalQuorum (Identity tid))
  mStatusValue <- (>>= runIdentity) <$> retry x1 q
  pure $ TeamFeatureStatusNoConfig <$> mStatusValue
  where
    select :: PrepQuery R (Identity TeamId) (Identity (Maybe TeamFeatureStatusValue))
    select = fromString $ "select " <> statusCol @a <> " from team_features where team_id = ?"

getFeatureStatusNoConfigMulti ::
  forall (a :: TeamFeatureName) m.
  ( MonadClient m,
    FeatureHasNoConfig 'WithoutLockStatus a,
    HasStatusCol a
  ) =>
  Proxy a ->
  [TeamId] ->
  m [(TeamId, TeamFeatureStatusValue, Int64)]
getFeatureStatusNoConfigMulti _ tids = do
  mapMaybe
    ( \(t, mStatus, mTime) -> do
        status <- mStatus
        time <- mTime
        pure (t, status, time)
    )
    <$> retry x1 (query select (params LocalQuorum (Identity tids)))
  where
    select :: PrepQuery R (Identity [TeamId]) (TeamId, Maybe TeamFeatureStatusValue, Maybe Int64)
    select = fromString $ "select team_id, " <> statusCol @a <> ", writetime(" <> statusCol @a <> ") from team_features where team_id in ?"

setFeatureStatusNoConfig ::
  forall (a :: TeamFeatureName) m.
  ( MonadClient m,
    FeatureHasNoConfig 'WithoutLockStatus a,
    HasStatusCol a
  ) =>
  Proxy a ->
  TeamId ->
  TeamFeatureStatus 'WithoutLockStatus a ->
  Maybe TeamFeatureTTLValue ->
  m (TeamFeatureStatus 'WithoutLockStatus a)
setFeatureStatusNoConfig _ tid status ttl = do
  let flag = tfwoStatus status
  retry x5 $ write insert (params LocalQuorum (tid, flag))
  pure status
  where
    minutes = (* 60)
    hours = minutes . (* 60)
    days = hours . (* 24)

    insert :: PrepQuery W (TeamId, TeamFeatureStatusValue) ()
    insert =
      fromString $
        "insert into team_features (team_id, " <> statusCol @a <> ") values (?, ?)"
          <> case ttl of
            Just (TeamFeatureTTLDays d) | d > 0 -> " using ttl " <> show (days d)
            _ -> mempty

getApplockFeatureStatus ::
  forall m.
  (MonadClient m) =>
  TeamId ->
  m (Maybe (TeamFeatureStatus 'WithoutLockStatus 'TeamFeatureAppLock))
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
  TeamFeatureStatus 'WithoutLockStatus 'TeamFeatureAppLock ->
  m (TeamFeatureStatus 'WithoutLockStatus 'TeamFeatureAppLock)
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
  m (Maybe (TeamFeatureStatus 'WithoutLockStatus 'TeamFeatureSelfDeletingMessages), Maybe LockStatusValue)
getSelfDeletingMessagesStatus tid = do
  let q = query1 select (params LocalQuorum (Identity tid))
  mTuple <- retry x1 q
  pure
    ( mTuple >>= \(mbStatusValue, mbTimeout, _) ->
        TeamFeatureStatusWithConfig <$> mbStatusValue <*> (TeamFeatureSelfDeletingMessagesConfig <$> mbTimeout),
      mTuple >>= \(_, _, mbLockStatus) -> mbLockStatus
    )
  where
    select :: PrepQuery R (Identity TeamId) (Maybe TeamFeatureStatusValue, Maybe Int32, Maybe LockStatusValue)
    select =
      fromString $
        "select "
          <> statusCol @'TeamFeatureSelfDeletingMessages
          <> ", self_deleting_messages_ttl, "
          <> lockStatusCol @'TeamFeatureSelfDeletingMessages
          <> " from team_features where team_id = ?"

setSelfDeletingMessagesStatus ::
  (MonadClient m) =>
  TeamId ->
  TeamFeatureStatus 'WithoutLockStatus 'TeamFeatureSelfDeletingMessages ->
  m (TeamFeatureStatus 'WithoutLockStatus 'TeamFeatureSelfDeletingMessages)
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

setLockStatus ::
  forall (a :: TeamFeatureName) m.
  ( MonadClient m,
    HasLockStatusCol a
  ) =>
  Proxy a ->
  TeamId ->
  LockStatus ->
  m LockStatus
setLockStatus _ tid (LockStatus lockStatus) = do
  retry x5 $ write insert (params LocalQuorum (tid, lockStatus))
  pure (LockStatus lockStatus)
  where
    insert :: PrepQuery W (TeamId, LockStatusValue) ()
    insert =
      fromString $
        "insert into team_features (team_id, " <> lockStatusCol @a <> ") values (?, ?)"

getLockStatus ::
  forall (a :: TeamFeatureName) m.
  ( MonadClient m,
    MaybeHasLockStatusCol a
  ) =>
  Proxy a ->
  TeamId ->
  m (Maybe LockStatusValue)
getLockStatus _ tid =
  case maybeLockStatusCol @a of
    Nothing -> pure Nothing
    Just lockStatusColName -> do
      let q = query1 select (params LocalQuorum (Identity tid))
      (>>= runIdentity) <$> retry x1 q
      where
        select :: PrepQuery R (Identity TeamId) (Identity (Maybe LockStatusValue))
        select =
          fromString $
            "select "
              <> lockStatusColName
              <> " from team_features where team_id = ?"

interpretTeamFeatureStoreToCassandra ::
  Members '[Embed IO, Input ClientState] r =>
  Sem (TeamFeatureStore ': r) a ->
  Sem r a
interpretTeamFeatureStoreToCassandra = interpret $ \case
  GetFeatureStatusNoConfig' tfn tid -> embedClient $ getFeatureStatusNoConfig tfn tid
  GetFeatureStatusNoConfigMulti tfn tids -> embedClient $ getFeatureStatusNoConfigMulti tfn tids
  GetFeatureStatusNoConfigAndLockStatus' tfn tid -> embedClient $ getFeatureStatusNoConfigAndLockStatus tfn tid
  SetFeatureStatusNoConfig' tfn tid value ttl -> embedClient $ setFeatureStatusNoConfig tfn tid value ttl
  SetLockStatus' p tid value -> embedClient $ setLockStatus p tid value
  GetLockStatus' p tid -> embedClient $ getLockStatus p tid
  GetApplockFeatureStatus tid -> embedClient $ getApplockFeatureStatus tid
  SetApplockFeatureStatus tid value -> embedClient $ setApplockFeatureStatus tid value
  GetSelfDeletingMessagesStatus tid -> embedClient $ getSelfDeletingMessagesStatus tid
  SetSelfDeletingMessagesStatus tid value -> embedClient $ setSelfDeletingMessagesStatus tid value
