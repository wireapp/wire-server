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
import Cassandra qualified as C
import Data.Id
import Data.Misc (HttpsUrl)
import Data.Time
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
import Wire.API.Conversation.Protocol (ProtocolTag)
import Wire.API.MLS.CipherSuite
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
getFeatureConfig FeatureSingletonLegalholdConfig tid = getFeature "legalhold_status" tid
getFeatureConfig FeatureSingletonSSOConfig tid = getFeature "sso_status" tid
getFeatureConfig FeatureSingletonSearchVisibilityAvailableConfig tid = getFeature "search_visibility_status" tid
getFeatureConfig FeatureSingletonValidateSAMLEmailsConfig tid = getFeature "validate_saml_emails" tid
getFeatureConfig FeatureSingletonClassifiedDomainsConfig _tid = pure mempty
getFeatureConfig FeatureSingletonDigitalSignaturesConfig tid = getFeature "digital_signatures" tid
getFeatureConfig FeatureSingletonAppLockConfig tid =
  getFeature
    "app_lock_status, app_lock_enforce, app_lock_inactivity_timeout_secs"
    tid
getFeatureConfig FeatureSingletonFileSharingConfig tid = getFeature "file_sharing" tid
getFeatureConfig FeatureSingletonSelfDeletingMessagesConfig tid =
  getFeature
    "self_deleting_messages_status, self_deleting_messages_ttl"
    tid
getFeatureConfig FeatureSingletonConferenceCallingConfig tid =
  getFeature
    "conference_calling_status, ttl(conference_calling_status), conference_calling_one_to_one"
    tid
getFeatureConfig FeatureSingletonGuestLinksConfig tid = getFeature "guest_links_status" tid
getFeatureConfig FeatureSingletonSndFactorPasswordChallengeConfig tid = getFeature "snd_factor_password_challenge_status" tid
getFeatureConfig FeatureSingletonSearchVisibilityInboundConfig tid = getFeature "search_visibility_status" tid
getFeatureConfig FeatureSingletonMLSConfig tid =
  getFeature
    "mls_status, mls_default_protocol, mls_protocol_toggle_users, \
    \mls_allowed_ciphersuites, mls_default_ciphersuite, mls_supported_protocols"
    tid
getFeatureConfig FeatureSingletonMlsE2EIdConfig tid =
  getFeature
    "mls_e2eid_status, mls_e2eid_grace_period, mls_e2eid_acme_discovery_url, \
    \mls_e2eid_crl_proxy, mls_e2eid_use_proxy_on_mobile"
    tid
getFeatureConfig FeatureSingletonMlsMigration tid =
  getFeature
    "mls_migration_status, mls_migration_start_time, \
    \mls_migration_finalise_regardless_after"
    tid
getFeatureConfig FeatureSingletonExposeInvitationURLsToTeamAdminConfig tid =
  getFeature "expose_invitation_urls_to_team_admin" tid
getFeatureConfig FeatureSingletonOutlookCalIntegrationConfig tid =
  getFeature "outlook_cal_integration_status" tid
getFeatureConfig FeatureSingletonEnforceFileDownloadLocationConfig tid =
  getFeature
    "enforce_file_download_location_status, enforce_file_download_location"
    tid
getFeatureConfig FeatureSingletonLimitedEventFanoutConfig tid =
  getFeature "limited_event_fanout_status" tid

setFeatureConfig :: (MonadClient m) => FeatureSingleton cfg -> TeamId -> Feature cfg -> m ()
setFeatureConfig FeatureSingletonLegalholdConfig tid feat = setFeatureStatusC "legalhold_status" tid feat.status
setFeatureConfig FeatureSingletonSSOConfig tid feat = setFeatureStatusC "sso_status" tid feat.status
setFeatureConfig FeatureSingletonSearchVisibilityAvailableConfig tid feat = setFeatureStatusC "search_visibility_status" tid feat.status
setFeatureConfig FeatureSingletonValidateSAMLEmailsConfig tid feat = setFeatureStatusC "validate_saml_emails" tid feat.status
setFeatureConfig FeatureSingletonClassifiedDomainsConfig _tid _feat = pure ()
setFeatureConfig FeatureSingletonDigitalSignaturesConfig tid feat = setFeatureStatusC "digital_signatures" tid feat.status
setFeatureConfig FeatureSingletonAppLockConfig tid feat = do
  let enforce = applockEnforceAppLock feat.config
      timeout = applockInactivityTimeoutSecs feat.config

  retry x5 $ write insert (params LocalQuorum (tid, feat.status, enforce, timeout))
  where
    insert :: PrepQuery W (TeamId, FeatureStatus, EnforceAppLock, Int32) ()
    insert =
      fromString $
        "insert into team_features (team_id, app_lock_status, app_lock_enforce,\
        \ app_lock_inactivity_timeout_secs) values (?, ?, ?, ?)"
setFeatureConfig FeatureSingletonFileSharingConfig tid feat = setFeatureStatusC "file_sharing" tid feat.status
setFeatureConfig FeatureSingletonSelfDeletingMessagesConfig tid feat = do
  let statusValue = feat.status
      timeout = sdmEnforcedTimeoutSeconds feat.config
  retry x5 $ write insert (params LocalQuorum (tid, statusValue, timeout))
  where
    insert :: PrepQuery W (TeamId, FeatureStatus, Int32) ()
    insert =
      "insert into team_features (team_id, self_deleting_messages_status,\
      \ self_deleting_messages_ttl) values (?, ?, ?)"
setFeatureConfig FeatureSingletonConferenceCallingConfig tid feat = do
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency LocalQuorum
    addPrepQuery insertStatus (tid, feat.status)
    addPrepQuery insertConfig (tid, feat.config.one2OneCalls)
  where
    insertStatus :: PrepQuery W (TeamId, FeatureStatus) ()
    insertStatus = "insert into team_features (team_id, conference_calling_status) values (?, ?)"
    insertConfig :: PrepQuery W (TeamId, One2OneCalls) ()
    insertConfig = "insert into team_features (team_id, conference_calling_one_to_one) values (?, ?)"
setFeatureConfig FeatureSingletonGuestLinksConfig tid feat = setFeatureStatusC "guest_links_status" tid feat.status
setFeatureConfig FeatureSingletonSndFactorPasswordChallengeConfig tid feat =
  setFeatureStatusC "snd_factor_password_challenge_status" tid feat.status
setFeatureConfig FeatureSingletonSearchVisibilityInboundConfig tid feat = setFeatureStatusC "search_visibility_status" tid feat.status
setFeatureConfig FeatureSingletonMLSConfig tid feat = do
  let status = feat.status
  let MLSConfig protocolToggleUsers defaultProtocol allowedCipherSuites defaultCipherSuite supportedProtocols = feat.config
  retry x5 $
    write
      insert
      ( params
          LocalQuorum
          ( tid,
            status,
            defaultProtocol,
            C.Set protocolToggleUsers,
            C.Set allowedCipherSuites,
            defaultCipherSuite,
            C.Set supportedProtocols
          )
      )
  where
    insert :: PrepQuery W (TeamId, FeatureStatus, ProtocolTag, C.Set UserId, C.Set CipherSuiteTag, CipherSuiteTag, C.Set ProtocolTag) ()
    insert =
      "insert into team_features (team_id, mls_status, mls_default_protocol, \
      \mls_protocol_toggle_users, mls_allowed_ciphersuites, mls_default_ciphersuite, mls_supported_protocols) values (?, ?, ?, ?, ?, ?, ?)"
setFeatureConfig FeatureSingletonMlsE2EIdConfig tid feat = do
  let statusValue = feat.status
      vex = verificationExpiration feat.config
      mUrl = acmeDiscoveryUrl feat.config
      mCrlProxy = crlProxy feat.config
      useProxy = useProxyOnMobile feat.config
  retry x5 $ write insert (params LocalQuorum (tid, statusValue, truncate vex, mUrl, mCrlProxy, useProxy))
  where
    insert :: PrepQuery W (TeamId, FeatureStatus, Int32, Maybe HttpsUrl, Maybe HttpsUrl, Bool) ()
    insert =
      "insert into team_features (team_id, mls_e2eid_status, mls_e2eid_grace_period, mls_e2eid_acme_discovery_url, mls_e2eid_crl_proxy, mls_e2eid_use_proxy_on_mobile) values (?, ?, ?, ?, ?, ?)"
setFeatureConfig FeatureSingletonMlsMigration tid feat = do
  retry x5 $ write insert (params LocalQuorum (tid, feat.status, feat.config.startTime, feat.config.finaliseRegardlessAfter))
  where
    insert :: PrepQuery W (TeamId, FeatureStatus, Maybe UTCTime, Maybe UTCTime) ()
    insert =
      "insert into team_features (team_id, mls_migration_status, mls_migration_start_time, mls_migration_finalise_regardless_after) values (?, ?, ?, ?)"
setFeatureConfig FeatureSingletonExposeInvitationURLsToTeamAdminConfig tid feat = setFeatureStatusC "expose_invitation_urls_to_team_admin" tid feat.status
setFeatureConfig FeatureSingletonOutlookCalIntegrationConfig tid feat = setFeatureStatusC "outlook_cal_integration_status" tid feat.status
setFeatureConfig FeatureSingletonEnforceFileDownloadLocationConfig tid feat = do
  retry x5 $ write insert (params LocalQuorum (tid, feat.status, feat.config.enforcedDownloadLocation))
  where
    insert :: PrepQuery W (TeamId, FeatureStatus, Maybe Text) ()
    insert =
      "insert into team_features (team_id, enforce_file_download_location_status, enforce_file_download_location) values (?, ?, ?)"
setFeatureConfig FeatureSingletonLimitedEventFanoutConfig tid feat =
  setFeatureStatusC "limited_event_fanout_status" tid feat.status

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

getFeature ::
  forall m cfg.
  (MonadClient m, MakeFeature cfg) =>
  String ->
  TeamId ->
  m (DbFeature cfg)
getFeature columns tid = do
  row <- retry x1 $ query1 select (params LocalQuorum (Identity tid))
  pure $ foldMap (mkFeature . toRowType) row
  where
    select :: PrepQuery R (Identity TeamId) (FeatureRow cfg)
    select =
      fromString $
        "select "
          <> columns
          <> " from team_features where team_id = ?"

setFeatureStatusC ::
  forall m.
  (MonadClient m) =>
  String ->
  TeamId ->
  FeatureStatus ->
  m ()
setFeatureStatusC statusCol tid status = do
  retry x5 $ write insert (params LocalQuorum (tid, status))
  where
    insert :: PrepQuery W (TeamId, FeatureStatus) ()
    insert =
      fromString $
        "insert into team_features (team_id, " <> statusCol <> ") values (?, ?)"

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
