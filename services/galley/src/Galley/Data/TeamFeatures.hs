{-# LANGUAGE ViewPatterns #-}

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

module Galley.Data.TeamFeatures
  ( getFeatureStatusNoConfig,
    setFeatureStatusNoConfig,
    getApplockFeatureStatus,
    setApplockFeatureStatus,
    getSelfDeletingMessagesStatus,
    setSelfDeletingMessagesStatus,
    HasStatusCol (..),
  )
where

import Cassandra
import Data.Id
import Galley.Data.Instances ()
import Imports
import Wire.API.Team.Feature
  ( TeamFeatureName (..),
    TeamFeatureStatus,
    TeamFeatureStatusNoConfig (..),
    TeamFeatureStatusValue (..),
    TeamFeatureStatusWithConfig (..),
  )
import qualified Wire.API.Team.Feature as Public

-- | Because not all so called team features are actually team-level features,
-- not all of them have a corresponding column in the database. Therefore,
-- instead of having a function like:
--
--   statusCol :: TeamFeatureName -> String
--
-- there is a need for turning it into a class and then defining an instance for
-- team features that do have a database column.
class HasStatusCol (a :: TeamFeatureName) where
  statusCol :: String

instance HasStatusCol 'TeamFeatureLegalHold where statusCol = "legalhold_status"

instance HasStatusCol 'TeamFeatureSSO where statusCol = "sso_status"

instance HasStatusCol 'TeamFeatureSearchVisibility where statusCol = "search_visibility_status"

instance HasStatusCol 'TeamFeatureValidateSAMLEmails where statusCol = "validate_saml_emails"

instance HasStatusCol 'TeamFeatureDigitalSignatures where statusCol = "digital_signatures"

instance HasStatusCol 'TeamFeatureAppLock where statusCol = "app_lock_status"

instance HasStatusCol 'TeamFeatureFileSharing where statusCol = "file_sharing"

instance HasStatusCol 'TeamFeatureConferenceCalling where statusCol = "conference_calling"

instance HasStatusCol 'TeamFeatureSelfDeletingMessages where statusCol = "self_deleting_messages_status"

getFeatureStatusNoConfig ::
  forall (a :: Public.TeamFeatureName) m.
  ( MonadClient m,
    Public.FeatureHasNoConfig a,
    HasStatusCol a
  ) =>
  TeamId ->
  m (Maybe (TeamFeatureStatus a))
getFeatureStatusNoConfig tid = do
  let q = query1 select (params Quorum (Identity tid))
  mStatusValue <- (>>= runIdentity) <$> retry x1 q
  pure $ TeamFeatureStatusNoConfig <$> mStatusValue
  where
    select :: PrepQuery R (Identity TeamId) (Identity (Maybe TeamFeatureStatusValue))
    select = fromString $ "select " <> statusCol @a <> " from team_features where team_id = ?"

setFeatureStatusNoConfig ::
  forall (a :: Public.TeamFeatureName) m.
  ( MonadClient m,
    Public.FeatureHasNoConfig a,
    HasStatusCol a
  ) =>
  TeamId ->
  TeamFeatureStatus a ->
  m (TeamFeatureStatus a)
setFeatureStatusNoConfig tid status = do
  let flag = Public.tfwoStatus status
  retry x5 $ write update (params Quorum (flag, tid))
  pure status
  where
    update :: PrepQuery W (TeamFeatureStatusValue, TeamId) ()
    update = fromString $ "update team_features set " <> statusCol @a <> " = ? where team_id = ?"

getApplockFeatureStatus ::
  forall m.
  (MonadClient m) =>
  TeamId ->
  m (Maybe (TeamFeatureStatus 'Public.TeamFeatureAppLock))
getApplockFeatureStatus tid = do
  let q = query1 select (params Quorum (Identity tid))
  mTuple <- retry x1 q
  pure $
    mTuple >>= \(mbStatusValue, mbEnforce, mbTimeout) ->
      TeamFeatureStatusWithConfig <$> mbStatusValue <*> (Public.TeamFeatureAppLockConfig <$> mbEnforce <*> mbTimeout)
  where
    select :: PrepQuery R (Identity TeamId) (Maybe TeamFeatureStatusValue, Maybe Public.EnforceAppLock, Maybe Int32)
    select =
      fromString $
        "select " <> statusCol @'Public.TeamFeatureAppLock <> ", app_lock_enforce, app_lock_inactivity_timeout_secs "
          <> "from team_features where team_id = ?"

setApplockFeatureStatus ::
  (MonadClient m) =>
  TeamId ->
  TeamFeatureStatus 'Public.TeamFeatureAppLock ->
  m (TeamFeatureStatus 'Public.TeamFeatureAppLock)
setApplockFeatureStatus tid status = do
  let statusValue = Public.tfwcStatus status
      enforce = Public.applockEnforceAppLock . Public.tfwcConfig $ status
      timeout = Public.applockInactivityTimeoutSecs . Public.tfwcConfig $ status
  retry x5 $ write update (params Quorum (statusValue, enforce, timeout, tid))
  pure status
  where
    update :: PrepQuery W (TeamFeatureStatusValue, Public.EnforceAppLock, Int32, TeamId) ()
    update =
      fromString $
        "update team_features set "
          <> statusCol @'Public.TeamFeatureAppLock
          <> " = ?, "
          <> "app_lock_enforce = ?, "
          <> "app_lock_inactivity_timeout_secs = ? "
          <> "where team_id = ?"

getSelfDeletingMessagesStatus ::
  forall m.
  (MonadClient m) =>
  TeamId ->
  m (Maybe (TeamFeatureStatus 'Public.TeamFeatureSelfDeletingMessages))
getSelfDeletingMessagesStatus tid = do
  let q = query1 select (params Quorum (Identity tid))
  mTuple <- retry x1 q
  pure $
    mTuple >>= \(mbStatusValue, mbTimeout) ->
      TeamFeatureStatusWithConfig <$> mbStatusValue <*> (Public.TeamFeatureSelfDeletingMessagesConfig <$> mbTimeout)
  where
    select :: PrepQuery R (Identity TeamId) (Maybe TeamFeatureStatusValue, Maybe Int32)
    select =
      fromString $
        "select "
          <> statusCol @'Public.TeamFeatureSelfDeletingMessages
          <> ", self_deleting_messages_ttl "
          <> "from team_features where team_id = ?"

setSelfDeletingMessagesStatus ::
  (MonadClient m) =>
  TeamId ->
  TeamFeatureStatus 'Public.TeamFeatureSelfDeletingMessages ->
  m (TeamFeatureStatus 'Public.TeamFeatureSelfDeletingMessages)
setSelfDeletingMessagesStatus tid status = do
  let statusValue = Public.tfwcStatus status
      timeout = Public.sdmEnforcedTimeoutSeconds . Public.tfwcConfig $ status
  retry x5 $ write insert (params Quorum (tid, statusValue, timeout))
  pure status
  where
    insert :: PrepQuery W (TeamId, TeamFeatureStatusValue, Int32) ()
    insert =
      fromString $
        "insert into team_features (team_id, "
          <> statusCol @'Public.TeamFeatureSelfDeletingMessages
          <> ", self_deleting_messages_ttl) "
          <> "values (?, ?, ?)"
