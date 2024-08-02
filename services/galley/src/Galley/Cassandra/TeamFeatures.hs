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

module Galley.Cassandra.TeamFeatures
  ( interpretTeamFeatureStoreToCassandra,
    getFeatureConfigMulti,
    getAllFeatureConfigsForServer,
  )
where

import Cassandra
import Data.Id
import Galley.API.Teams.Features.Get
import Galley.Cassandra.GetAllTeamFeatureConfigs
import Galley.Cassandra.Instances ()
import Galley.Cassandra.MakeFeature
import Galley.Cassandra.Store
import Galley.Cassandra.Util
import Galley.Effects.TeamFeatureStore qualified as TFS
import Imports
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog
import UnliftIO.Async (pooledMapConcurrentlyN)
import Wire.API.Team.Feature

interpretTeamFeatureStoreToCassandra ::
  ( Member (Embed IO) r,
    Member (Input ClientState) r,
    Member TinyLog r
  ) =>
  Sem (TFS.TeamFeatureStore ': r) a ->
  Sem r a
interpretTeamFeatureStoreToCassandra = interpret $ \case
  TFS.GetFeatureConfig sing tid -> do
    logEffect "TeamFeatureStore.GetFeatureConfig"
    embedClient $ getFeatureConfig sing tid
  TFS.GetFeatureConfigMulti sing tids -> do
    logEffect "TeamFeatureStore.GetFeatureConfigMulti"
    embedClient $ getFeatureConfigMulti sing tids
  TFS.SetFeatureConfig sing tid wsnl -> do
    logEffect "TeamFeatureStore.SetFeatureConfig"
    embedClient $ setFeatureConfig sing tid wsnl
  TFS.GetFeatureLockStatus sing tid -> do
    logEffect "TeamFeatureStore.GetFeatureLockStatus"
    embedClient $ getFeatureLockStatus sing tid
  TFS.SetFeatureLockStatus sing tid ls -> do
    logEffect "TeamFeatureStore.SetFeatureLockStatus"
    embedClient $ setFeatureLockStatus sing tid ls
  TFS.GetAllFeatureConfigs tid -> do
    logEffect "TeamFeatureStore.GetAllFeatureConfigs"
    embedClient $ getAllFeatureConfigs tid

getFeatureConfig :: (MonadClient m) => FeatureSingleton cfg -> TeamId -> m (DbFeature cfg)
getFeatureConfig FeatureSingletonLegalholdConfig = getFeature
getFeatureConfig FeatureSingletonSSOConfig = getFeature
getFeatureConfig FeatureSingletonSearchVisibilityAvailableConfig = getFeature
getFeatureConfig FeatureSingletonValidateSAMLEmailsConfig = getFeature
getFeatureConfig FeatureSingletonClassifiedDomainsConfig = const (pure mempty)
getFeatureConfig FeatureSingletonDigitalSignaturesConfig = getFeature
getFeatureConfig FeatureSingletonAppLockConfig = getFeature
getFeatureConfig FeatureSingletonFileSharingConfig = getFeature
getFeatureConfig FeatureSingletonSelfDeletingMessagesConfig = getFeature
getFeatureConfig FeatureSingletonConferenceCallingConfig = getFeature
getFeatureConfig FeatureSingletonGuestLinksConfig = getFeature
getFeatureConfig FeatureSingletonSndFactorPasswordChallengeConfig = getFeature
getFeatureConfig FeatureSingletonSearchVisibilityInboundConfig = getFeature
getFeatureConfig FeatureSingletonMLSConfig = getFeature
getFeatureConfig FeatureSingletonMlsE2EIdConfig = getFeature
getFeatureConfig FeatureSingletonMlsMigration = getFeature
getFeatureConfig FeatureSingletonExposeInvitationURLsToTeamAdminConfig = getFeature
getFeatureConfig FeatureSingletonOutlookCalIntegrationConfig = getFeature
getFeatureConfig FeatureSingletonEnforceFileDownloadLocationConfig = getFeature
getFeatureConfig FeatureSingletonLimitedEventFanoutConfig = getFeature

setFeatureConfig :: (MonadClient m) => FeatureSingleton cfg -> TeamId -> Feature cfg -> m ()
setFeatureConfig FeatureSingletonLegalholdConfig = setFeature
setFeatureConfig FeatureSingletonSSOConfig = setFeature
setFeatureConfig FeatureSingletonSearchVisibilityAvailableConfig = setFeature
setFeatureConfig FeatureSingletonValidateSAMLEmailsConfig = setFeature
setFeatureConfig FeatureSingletonClassifiedDomainsConfig = \_ _ -> pure ()
setFeatureConfig FeatureSingletonDigitalSignaturesConfig = setFeature
setFeatureConfig FeatureSingletonAppLockConfig = setFeature
setFeatureConfig FeatureSingletonFileSharingConfig = setFeature
setFeatureConfig FeatureSingletonSelfDeletingMessagesConfig = setFeature
setFeatureConfig FeatureSingletonConferenceCallingConfig = setFeature
setFeatureConfig FeatureSingletonGuestLinksConfig = setFeature
setFeatureConfig FeatureSingletonSndFactorPasswordChallengeConfig = setFeature
setFeatureConfig FeatureSingletonSearchVisibilityInboundConfig = setFeature
setFeatureConfig FeatureSingletonMLSConfig = setFeature
setFeatureConfig FeatureSingletonMlsE2EIdConfig = setFeature
setFeatureConfig FeatureSingletonMlsMigration = setFeature
setFeatureConfig FeatureSingletonExposeInvitationURLsToTeamAdminConfig = setFeature
setFeatureConfig FeatureSingletonOutlookCalIntegrationConfig = setFeature
setFeatureConfig FeatureSingletonEnforceFileDownloadLocationConfig = setFeature
setFeatureConfig FeatureSingletonLimitedEventFanoutConfig = setFeature

getFeatureLockStatus :: (MonadClient m) => FeatureSingleton cfg -> TeamId -> m (Maybe LockStatus)
getFeatureLockStatus FeatureSingletonFileSharingConfig tid = getLockStatusC "file_sharing_lock_status" tid
getFeatureLockStatus FeatureSingletonSelfDeletingMessagesConfig tid = getLockStatusC "self_deleting_messages_lock_status" tid
getFeatureLockStatus FeatureSingletonGuestLinksConfig tid = getLockStatusC "guest_links_lock_status" tid
getFeatureLockStatus FeatureSingletonSndFactorPasswordChallengeConfig tid = getLockStatusC "snd_factor_password_challenge_lock_status" tid
getFeatureLockStatus FeatureSingletonMlsE2EIdConfig tid = getLockStatusC "mls_e2eid_lock_status" tid
getFeatureLockStatus FeatureSingletonMlsMigration tid = getLockStatusC "mls_migration_lock_status" tid
getFeatureLockStatus FeatureSingletonOutlookCalIntegrationConfig tid = getLockStatusC "outlook_cal_integration_lock_status" tid
getFeatureLockStatus FeatureSingletonMLSConfig tid = getLockStatusC "mls_lock_status" tid
getFeatureLockStatus FeatureSingletonEnforceFileDownloadLocationConfig tid = getLockStatusC "enforce_file_download_location_lock_status" tid
getFeatureLockStatus FeatureSingletonConferenceCallingConfig tid = getLockStatusC "conference_calling" tid
getFeatureLockStatus _ _ = pure Nothing

setFeatureLockStatus :: (MonadClient m) => FeatureSingleton cfg -> TeamId -> LockStatus -> m ()
setFeatureLockStatus FeatureSingletonFileSharingConfig tid feat = setLockStatusC "file_sharing_lock_status" tid feat
setFeatureLockStatus FeatureSingletonSelfDeletingMessagesConfig tid feat = setLockStatusC "self_deleting_messages_lock_status" tid feat
setFeatureLockStatus FeatureSingletonGuestLinksConfig tid feat = setLockStatusC "guest_links_lock_status" tid feat
setFeatureLockStatus FeatureSingletonSndFactorPasswordChallengeConfig tid feat = setLockStatusC "snd_factor_password_challenge_lock_status" tid feat
setFeatureLockStatus FeatureSingletonMlsE2EIdConfig tid feat = setLockStatusC "mls_e2eid_lock_status" tid feat
setFeatureLockStatus FeatureSingletonMlsMigration tid feat = setLockStatusC "mls_migration_lock_status" tid feat
setFeatureLockStatus FeatureSingletonOutlookCalIntegrationConfig tid feat = setLockStatusC "outlook_cal_integration_lock_status" tid feat
setFeatureLockStatus FeatureSingletonMLSConfig tid feat = setLockStatusC "mls_lock_status" tid feat
setFeatureLockStatus FeatureSingletonEnforceFileDownloadLocationConfig tid feat = setLockStatusC "enforce_file_download_location_lock_status" tid feat
setFeatureLockStatus FeatureSingletonConferenceCallingConfig tid feat = setLockStatusC "conference_calling" tid feat
setFeatureLockStatus _ _tid _status = pure ()

getLockStatusC ::
  forall m.
  (MonadClient m) =>
  String ->
  TeamId ->
  m (Maybe LockStatus)
getLockStatusC lockStatusCol tid = do
  let q = query1 select (params LocalQuorum (Identity tid))
  (>>= runIdentity) <$> retry x1 q
  where
    select :: PrepQuery R (Identity TeamId) (Identity (Maybe LockStatus))
    select =
      fromString $
        "select "
          <> lockStatusCol
          <> " from team_features where team_id = ?"

setLockStatusC ::
  (MonadClient m) =>
  String ->
  TeamId ->
  LockStatus ->
  m ()
setLockStatusC col tid status = do
  retry x5 $ write insert (params LocalQuorum (tid, status))
  where
    insert :: PrepQuery W (TeamId, LockStatus) ()
    insert =
      fromString $
        "insert into team_features (team_id, " <> col <> ") values (?, ?)"

getFeatureConfigMulti ::
  forall cfg m.
  (MonadClient m, MonadUnliftIO m) =>
  FeatureSingleton cfg ->
  [TeamId] ->
  m [(TeamId, DbFeature cfg)]
getFeatureConfigMulti proxy =
  pooledMapConcurrentlyN 8 (\tid -> getFeatureConfig proxy tid <&> (tid,))
