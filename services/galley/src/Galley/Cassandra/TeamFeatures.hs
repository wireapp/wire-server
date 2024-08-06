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
    fmap untag . embedClient $ getFeatureLockStatus sing tid
  TFS.SetFeatureLockStatus sing tid ls -> do
    logEffect "TeamFeatureStore.SetFeatureLockStatus"
    embedClient $ setFeatureLockStatus sing tid (Tagged ls)
  TFS.GetAllFeatureConfigs tid -> do
    logEffect "TeamFeatureStore.GetAllFeatureConfigs"
    embedClient $ getAllFeatureConfigs tid

getFeatureConfig :: (MonadClient m) => FeatureSingleton cfg -> TeamId -> m (DbFeature cfg)
getFeatureConfig FeatureSingletonLegalholdConfig = fetchFeature
getFeatureConfig FeatureSingletonSSOConfig = fetchFeature
getFeatureConfig FeatureSingletonSearchVisibilityAvailableConfig = fetchFeature
getFeatureConfig FeatureSingletonValidateSAMLEmailsConfig = fetchFeature
getFeatureConfig FeatureSingletonClassifiedDomainsConfig = fetchFeature
getFeatureConfig FeatureSingletonDigitalSignaturesConfig = fetchFeature
getFeatureConfig FeatureSingletonAppLockConfig = fetchFeature
getFeatureConfig FeatureSingletonFileSharingConfig = fetchFeature
getFeatureConfig FeatureSingletonSelfDeletingMessagesConfig = fetchFeature
getFeatureConfig FeatureSingletonConferenceCallingConfig = fetchFeature
getFeatureConfig FeatureSingletonGuestLinksConfig = fetchFeature
getFeatureConfig FeatureSingletonSndFactorPasswordChallengeConfig = fetchFeature
getFeatureConfig FeatureSingletonSearchVisibilityInboundConfig = fetchFeature
getFeatureConfig FeatureSingletonMLSConfig = fetchFeature
getFeatureConfig FeatureSingletonMlsE2EIdConfig = fetchFeature
getFeatureConfig FeatureSingletonMlsMigration = fetchFeature
getFeatureConfig FeatureSingletonExposeInvitationURLsToTeamAdminConfig = fetchFeature
getFeatureConfig FeatureSingletonOutlookCalIntegrationConfig = fetchFeature
getFeatureConfig FeatureSingletonEnforceFileDownloadLocationConfig = fetchFeature
getFeatureConfig FeatureSingletonLimitedEventFanoutConfig = fetchFeature

setFeatureConfig :: (MonadClient m) => FeatureSingleton cfg -> TeamId -> Feature cfg -> m ()
setFeatureConfig FeatureSingletonLegalholdConfig = storeFeature
setFeatureConfig FeatureSingletonSSOConfig = storeFeature
setFeatureConfig FeatureSingletonSearchVisibilityAvailableConfig = storeFeature
setFeatureConfig FeatureSingletonValidateSAMLEmailsConfig = storeFeature
setFeatureConfig FeatureSingletonClassifiedDomainsConfig = storeFeature
setFeatureConfig FeatureSingletonDigitalSignaturesConfig = storeFeature
setFeatureConfig FeatureSingletonAppLockConfig = storeFeature
setFeatureConfig FeatureSingletonFileSharingConfig = storeFeature
setFeatureConfig FeatureSingletonSelfDeletingMessagesConfig = storeFeature
setFeatureConfig FeatureSingletonConferenceCallingConfig = storeFeature
setFeatureConfig FeatureSingletonGuestLinksConfig = storeFeature
setFeatureConfig FeatureSingletonSndFactorPasswordChallengeConfig = storeFeature
setFeatureConfig FeatureSingletonSearchVisibilityInboundConfig = storeFeature
setFeatureConfig FeatureSingletonMLSConfig = storeFeature
setFeatureConfig FeatureSingletonMlsE2EIdConfig = storeFeature
setFeatureConfig FeatureSingletonMlsMigration = storeFeature
setFeatureConfig FeatureSingletonExposeInvitationURLsToTeamAdminConfig = storeFeature
setFeatureConfig FeatureSingletonOutlookCalIntegrationConfig = storeFeature
setFeatureConfig FeatureSingletonEnforceFileDownloadLocationConfig = storeFeature
setFeatureConfig FeatureSingletonLimitedEventFanoutConfig = storeFeature

getFeatureLockStatus ::
  (MonadClient m) =>
  FeatureSingleton cfg ->
  TeamId ->
  m (Tagged cfg (Maybe LockStatus))
getFeatureLockStatus FeatureSingletonLegalholdConfig = fetchFeatureLockStatus
getFeatureLockStatus FeatureSingletonSSOConfig = fetchFeatureLockStatus
getFeatureLockStatus FeatureSingletonSearchVisibilityAvailableConfig = fetchFeatureLockStatus
getFeatureLockStatus FeatureSingletonValidateSAMLEmailsConfig = fetchFeatureLockStatus
getFeatureLockStatus FeatureSingletonClassifiedDomainsConfig = fetchFeatureLockStatus
getFeatureLockStatus FeatureSingletonDigitalSignaturesConfig = fetchFeatureLockStatus
getFeatureLockStatus FeatureSingletonAppLockConfig = fetchFeatureLockStatus
getFeatureLockStatus FeatureSingletonFileSharingConfig = fetchFeatureLockStatus
getFeatureLockStatus FeatureSingletonSelfDeletingMessagesConfig = fetchFeatureLockStatus
getFeatureLockStatus FeatureSingletonConferenceCallingConfig = fetchFeatureLockStatus
getFeatureLockStatus FeatureSingletonGuestLinksConfig = fetchFeatureLockStatus
getFeatureLockStatus FeatureSingletonSndFactorPasswordChallengeConfig = fetchFeatureLockStatus
getFeatureLockStatus FeatureSingletonSearchVisibilityInboundConfig = fetchFeatureLockStatus
getFeatureLockStatus FeatureSingletonMLSConfig = fetchFeatureLockStatus
getFeatureLockStatus FeatureSingletonMlsE2EIdConfig = fetchFeatureLockStatus
getFeatureLockStatus FeatureSingletonMlsMigration = fetchFeatureLockStatus
getFeatureLockStatus FeatureSingletonExposeInvitationURLsToTeamAdminConfig = fetchFeatureLockStatus
getFeatureLockStatus FeatureSingletonOutlookCalIntegrationConfig = fetchFeatureLockStatus
getFeatureLockStatus FeatureSingletonEnforceFileDownloadLocationConfig = fetchFeatureLockStatus
getFeatureLockStatus FeatureSingletonLimitedEventFanoutConfig = fetchFeatureLockStatus

setFeatureLockStatus ::
  (MonadClient m) =>
  FeatureSingleton cfg ->
  TeamId ->
  Tagged cfg LockStatus ->
  m ()
setFeatureLockStatus FeatureSingletonLegalholdConfig = storeFeatureLockStatus
setFeatureLockStatus FeatureSingletonSSOConfig = storeFeatureLockStatus
setFeatureLockStatus FeatureSingletonSearchVisibilityAvailableConfig = storeFeatureLockStatus
setFeatureLockStatus FeatureSingletonValidateSAMLEmailsConfig = storeFeatureLockStatus
setFeatureLockStatus FeatureSingletonClassifiedDomainsConfig = storeFeatureLockStatus
setFeatureLockStatus FeatureSingletonDigitalSignaturesConfig = storeFeatureLockStatus
setFeatureLockStatus FeatureSingletonAppLockConfig = storeFeatureLockStatus
setFeatureLockStatus FeatureSingletonFileSharingConfig = storeFeatureLockStatus
setFeatureLockStatus FeatureSingletonSelfDeletingMessagesConfig = storeFeatureLockStatus
setFeatureLockStatus FeatureSingletonConferenceCallingConfig = storeFeatureLockStatus
setFeatureLockStatus FeatureSingletonGuestLinksConfig = storeFeatureLockStatus
setFeatureLockStatus FeatureSingletonSndFactorPasswordChallengeConfig = storeFeatureLockStatus
setFeatureLockStatus FeatureSingletonSearchVisibilityInboundConfig = storeFeatureLockStatus
setFeatureLockStatus FeatureSingletonMLSConfig = storeFeatureLockStatus
setFeatureLockStatus FeatureSingletonMlsE2EIdConfig = storeFeatureLockStatus
setFeatureLockStatus FeatureSingletonMlsMigration = storeFeatureLockStatus
setFeatureLockStatus FeatureSingletonExposeInvitationURLsToTeamAdminConfig = storeFeatureLockStatus
setFeatureLockStatus FeatureSingletonOutlookCalIntegrationConfig = storeFeatureLockStatus
setFeatureLockStatus FeatureSingletonEnforceFileDownloadLocationConfig = storeFeatureLockStatus
setFeatureLockStatus FeatureSingletonLimitedEventFanoutConfig = storeFeatureLockStatus

getFeatureConfigMulti ::
  forall cfg m.
  (MonadClient m, MonadUnliftIO m) =>
  FeatureSingleton cfg ->
  [TeamId] ->
  m [(TeamId, DbFeature cfg)]
getFeatureConfigMulti proxy =
  pooledMapConcurrentlyN 8 (\tid -> getFeatureConfig proxy tid <&> (tid,))
