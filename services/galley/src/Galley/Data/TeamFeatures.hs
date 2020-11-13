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
  ( getFeatureStatus,
    setFeatureStatus,
    getApplockFeatureStatus,
    setApplockFeatureStatus,
  )
where

import Cassandra
import Data.Id
import Galley.Data.Instances ()
import Imports
import Wire.API.Team.Feature (TeamFeatureName (..), TeamFeatureStatus (..), TeamFeatureStatusValue (..))
import qualified Wire.API.Team.Feature as Public

toCol :: TeamFeatureName -> String
toCol TeamFeatureLegalHold = "legalhold_status"
toCol TeamFeatureSSO = "sso_status"
toCol TeamFeatureSearchVisibility = "search_visibility_status"
toCol TeamFeatureValidateSAMLEmails = "validate_saml_emails"
toCol TeamFeatureDigitalSignatures = "digital_signatures"
toCol TeamFeatureAppLock = "app_lock_status"

getFeatureStatus ::
  forall (a :: Public.TeamFeatureName) m.
  ( MonadClient m,
    Public.KnownTeamFeatureName a,
    Public.FeatureHasNoConfig a
  ) =>
  TeamId ->
  m (Maybe (TeamFeatureStatus a))
getFeatureStatus tid = do
  let q = query1 (select (Public.knownTeamFeatureName @a)) (params Quorum (Identity tid))
  mStatusValue <- (>>= runIdentity) <$> retry x1 q
  pure $ (Public.mkFeatureStatus <$> mStatusValue)
  where
    select :: TeamFeatureName -> PrepQuery R (Identity TeamId) (Identity (Maybe TeamFeatureStatusValue))
    select feature = fromString $ "select " <> toCol feature <> " from team_features where team_id = ?"

setFeatureStatus ::
  forall (a :: Public.TeamFeatureName) m.
  ( MonadClient m,
    Public.KnownTeamFeatureName a,
    Public.FeatureHasNoConfig a
  ) =>
  TeamId ->
  (TeamFeatureStatus a) ->
  m ()
setFeatureStatus tid status = do
  let flag = Public.teamFeatureStatusValue status
  retry x5 $ write (update (Public.knownTeamFeatureName @a)) (params Quorum (flag, tid))
  where
    update :: TeamFeatureName -> PrepQuery W (TeamFeatureStatusValue, TeamId) ()
    update feature = fromString $ "update team_features set " <> toCol feature <> " = ? where team_id = ?"

getApplockFeatureStatus ::
  (MonadClient m) =>
  TeamId ->
  m (Maybe (TeamFeatureStatus 'Public.TeamFeatureAppLock))
getApplockFeatureStatus tid = do
  let q = query1 (select) (params Quorum (Identity tid))
  mTuple <- retry x1 q
  pure $
    mTuple <&> \(statusValue, enforce, timeout) ->
      TeamFeatureStatus statusValue (Public.TeamFeatureAppLockConfig enforce timeout)
  where
    select :: PrepQuery R (Identity TeamId) (TeamFeatureStatusValue, Public.EnforceAppLock, Int32)
    select =
      fromString $
        "select " <> toCol Public.TeamFeatureAppLock <> ", app_lock_enforce, app_lock_inactivity_timeout_secs "
          <> "from team_features where team_id = ?"

setApplockFeatureStatus ::
  (MonadClient m) =>
  TeamId ->
  (TeamFeatureStatus 'Public.TeamFeatureAppLock) ->
  m ()
setApplockFeatureStatus tid status = do
  let statusValue = Public.teamFeatureStatusValue status
      enforce = Public.applockEnforceAppLock . Public.teamFeatureConfig $ status
      timeout = Public.applockInactivityTimeoutSecs . Public.teamFeatureConfig $ status
  retry x5 $ write update (params Quorum (statusValue, enforce, timeout, tid))
  where
    update :: PrepQuery W (TeamFeatureStatusValue, Public.EnforceAppLock, Int32, TeamId) ()
    update =
      fromString $
        "update team_features set "
          <> toCol Public.TeamFeatureAppLock
          <> " = ?, "
          <> "app_lock_enforce = ?, "
          <> "app_lock_inactivity_timeout_secs = ? "
          <> "where team_id = ?"
