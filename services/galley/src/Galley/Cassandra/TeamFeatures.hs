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
import Control.Monad.Trans.Maybe
import Data.Id
import Data.Misc (HttpsUrl)
import Data.Time
import Galley.API.Teams.Features.Get
import Galley.Cassandra.GetAllTeamFeatureConfigs
import Galley.Cassandra.Instances ()
import Galley.Cassandra.Store
import Galley.Cassandra.Util
import Galley.Effects (LegalHoldStore)
import Galley.Effects.LegalHoldStore qualified as LH
import Galley.Effects.TeamFeatureStore qualified as TFS
import Galley.Types.Teams (FeatureLegalHold)
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
    Member (Input AllFeatureConfigs) r,
    Member (Input (Maybe [TeamId], FeatureLegalHold)) r,
    Member LegalHoldStore r,
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
    serverConfigs <- input
    (allowListForExposeInvitationURLs, featureLH) <- input
    hasTeamImplicitLegalhold <- LH.isTeamLegalholdWhitelisted tid
    embedClient $
      getAllFeatureConfigs
        allowListForExposeInvitationURLs
        featureLH
        hasTeamImplicitLegalhold
        serverConfigs
        tid

getFeatureConfig :: MonadClient m => FeatureSingleton cfg -> TeamId -> m (Maybe (WithStatusNoLock cfg))
getFeatureConfig FeatureSingletonLegalholdConfig tid = getTrivialConfigC "legalhold_status" tid
getFeatureConfig FeatureSingletonSSOConfig tid = getTrivialConfigC "sso_status" tid
getFeatureConfig FeatureSingletonSearchVisibilityAvailableConfig tid = getTrivialConfigC "search_visibility_status" tid
getFeatureConfig FeatureSingletonValidateSAMLEmailsConfig tid = getTrivialConfigC "validate_saml_emails" tid
getFeatureConfig FeatureSingletonClassifiedDomainsConfig _tid = pure Nothing -- TODO(fisx): what's this about?
getFeatureConfig FeatureSingletonDigitalSignaturesConfig tid = getTrivialConfigC "digital_signatures" tid
getFeatureConfig FeatureSingletonAppLockConfig tid = runMaybeT $ do
  (mStatus, mEnforce, mTimeout) <-
    MaybeT . retry x1 $
      query1 select (params LocalQuorum (Identity tid))
  maybe mzero pure $
    WithStatusNoLock
      <$> mStatus
      <*> (AppLockConfig <$> mEnforce <*> mTimeout)
      -- FUTUREWORK: the above line is duplicated in
      -- "Galley.Cassandra.GetAllTeamFeatureConfigs"; make sure the two don't diverge!
      <*> Just FeatureTTLUnlimited
  where
    select :: PrepQuery R (Identity TeamId) (Maybe FeatureStatus, Maybe EnforceAppLock, Maybe Int32)
    select =
      "select app_lock_status, app_lock_enforce, app_lock_inactivity_timeout_secs \
      \ from team_features where team_id = ?"
getFeatureConfig FeatureSingletonFileSharingConfig tid = getTrivialConfigC "file_sharing" tid
getFeatureConfig FeatureSingletonSelfDeletingMessagesConfig tid = runMaybeT $ do
  (mEnabled, mTimeout) <-
    MaybeT . retry x1 $
      query1 select (params LocalQuorum (Identity tid))
  maybe mzero pure $
    WithStatusNoLock
      <$> mEnabled
      <*> fmap SelfDeletingMessagesConfig mTimeout
      -- FUTUREWORK: the above line is duplicated in
      -- "Galley.Cassandra.GetAllTeamFeatureConfigs"; make sure the two don't diverge!
      <*> Just FeatureTTLUnlimited
  where
    select :: PrepQuery R (Identity TeamId) (Maybe FeatureStatus, Maybe Int32)
    select =
      "select self_deleting_messages_status, self_deleting_messages_ttl\
      \ from team_features where team_id = ?"
getFeatureConfig FeatureSingletonConferenceCallingConfig tid = do
  let q = query1 select (params LocalQuorum (Identity tid))
  retry x1 q <&> \case
    Nothing -> Nothing
    Just (Nothing, _) -> Nothing
    Just (Just status, mTtl) ->
      Just
        . forgetLock
        . setStatus status
        . setWsTTL (fromMaybe FeatureTTLUnlimited mTtl)
        $ defFeatureStatus
  where
    select :: PrepQuery R (Identity TeamId) (Maybe FeatureStatus, Maybe FeatureTTL)
    select =
      fromString $
        "select conference_calling, ttl(conference_calling) from team_features where team_id = ?"
getFeatureConfig FeatureSingletonGuestLinksConfig tid = getTrivialConfigC "guest_links_status" tid
getFeatureConfig FeatureSingletonSndFactorPasswordChallengeConfig tid = getTrivialConfigC "snd_factor_password_challenge_status" tid
getFeatureConfig FeatureSingletonSearchVisibilityInboundConfig tid = getTrivialConfigC "search_visibility_status" tid
getFeatureConfig FeatureSingletonMLSConfig tid = do
  m <- retry x1 $ query1 select (params LocalQuorum (Identity tid))
  pure $ case m of
    Nothing -> Nothing
    Just (status, defaultProtocol, protocolToggleUsers, allowedCipherSuites, defaultCipherSuite, supportedProtocols) ->
      WithStatusNoLock
        <$> status
        <*> ( -- FUTUREWORK: this block is duplicated in
              -- "Galley.Cassandra.GetAllTeamFeatureConfigs"; make sure the two don't diverge!
              MLSConfig
                <$> maybe (Just []) (Just . C.fromSet) protocolToggleUsers
                <*> defaultProtocol
                <*> maybe (Just []) (Just . C.fromSet) allowedCipherSuites
                <*> defaultCipherSuite
                <*> maybe (Just []) (Just . C.fromSet) supportedProtocols
            )
        <*> Just FeatureTTLUnlimited
  where
    select :: PrepQuery R (Identity TeamId) (Maybe FeatureStatus, Maybe ProtocolTag, Maybe (C.Set UserId), Maybe (C.Set CipherSuiteTag), Maybe CipherSuiteTag, Maybe (C.Set ProtocolTag))
    select =
      "select mls_status, mls_default_protocol, mls_protocol_toggle_users, mls_allowed_ciphersuites, \
      \mls_default_ciphersuite, mls_supported_protocols from team_features where team_id = ?"
getFeatureConfig FeatureSingletonMlsE2EIdConfig tid = do
  let q = query1 select (params LocalQuorum (Identity tid))
  retry x1 q <&> \case
    Nothing -> Nothing
    Just (Nothing, _, _, _, _) -> Nothing
    Just (Just fs, mGracePeriod, mUrl, mCrlProxy, mUseProxyOnMobile) ->
      Just $
        WithStatusNoLock
          fs
          ( MlsE2EIdConfig (toGracePeriodOrDefault mGracePeriod) mUrl mCrlProxy (fromMaybe (useProxyOnMobile . wsConfig $ defFeatureStatus @MlsE2EIdConfig) mUseProxyOnMobile)
          )
          FeatureTTLUnlimited
  where
    toGracePeriodOrDefault :: Maybe Int32 -> NominalDiffTime
    toGracePeriodOrDefault = maybe (verificationExpiration $ wsConfig defFeatureStatus) fromIntegral

    select :: PrepQuery R (Identity TeamId) (Maybe FeatureStatus, Maybe Int32, Maybe HttpsUrl, Maybe HttpsUrl, Maybe Bool)
    select =
      fromString $
        "select mls_e2eid_status, mls_e2eid_grace_period, mls_e2eid_acme_discovery_url, mls_e2eid_crl_proxy, mls_e2eid_use_proxy_on_mobile from team_features where team_id = ?"
getFeatureConfig FeatureSingletonMlsMigration tid = do
  let q = query1 select (params LocalQuorum (Identity tid))
  retry x1 q <&> \case
    Nothing -> Nothing
    Just (Nothing, _, _) -> Nothing
    Just (Just fs, startTime, finaliseRegardlessAfter) ->
      Just $
        WithStatusNoLock
          fs
          -- FUTUREWORK: the following expression is duplicated in
          -- "Galley.Cassandra.GetAllTeamFeatureConfigs"; make sure the two don't diverge!
          MlsMigrationConfig
            { startTime = startTime,
              finaliseRegardlessAfter = finaliseRegardlessAfter
            }
          FeatureTTLUnlimited
  where
    select :: PrepQuery R (Identity TeamId) (Maybe FeatureStatus, Maybe UTCTime, Maybe UTCTime)
    select = "select mls_migration_status, mls_migration_start_time, mls_migration_finalise_regardless_after from team_features where team_id = ?"
getFeatureConfig FeatureSingletonExposeInvitationURLsToTeamAdminConfig tid = getTrivialConfigC "expose_invitation_urls_to_team_admin" tid
getFeatureConfig FeatureSingletonOutlookCalIntegrationConfig tid = getTrivialConfigC "outlook_cal_integration_status" tid
getFeatureConfig FeatureSingletonEnforceFileDownloadLocationConfig tid = do
  let q = query1 select (params LocalQuorum (Identity tid))
  retry x1 q <&> \case
    Nothing -> Nothing
    Just (Nothing, _) -> Nothing
    Just (Just fs, mbLocation) ->
      Just $ WithStatusNoLock fs (EnforceFileDownloadLocationConfig mbLocation) FeatureTTLUnlimited
  where
    select :: PrepQuery R (Identity TeamId) (Maybe FeatureStatus, Maybe Text)
    select = "select enforce_file_download_location_status, enforce_file_download_location from team_features where team_id = ?"
getFeatureConfig FeatureSingletonLimitedEventFanoutConfig tid =
  getTrivialConfigC "limited_event_fanout_status" tid

setFeatureConfig :: MonadClient m => FeatureSingleton cfg -> TeamId -> WithStatusNoLock cfg -> m ()
setFeatureConfig FeatureSingletonLegalholdConfig tid statusNoLock = setFeatureStatusC "legalhold_status" tid (wssStatus statusNoLock)
setFeatureConfig FeatureSingletonSSOConfig tid statusNoLock = setFeatureStatusC "sso_status" tid (wssStatus statusNoLock)
setFeatureConfig FeatureSingletonSearchVisibilityAvailableConfig tid statusNoLock = setFeatureStatusC "search_visibility_status" tid (wssStatus statusNoLock)
setFeatureConfig FeatureSingletonValidateSAMLEmailsConfig tid statusNoLock = setFeatureStatusC "validate_saml_emails" tid (wssStatus statusNoLock)
setFeatureConfig FeatureSingletonClassifiedDomainsConfig _tid _statusNoLock = pure ()
setFeatureConfig FeatureSingletonDigitalSignaturesConfig tid statusNoLock = setFeatureStatusC "digital_signatures" tid (wssStatus statusNoLock)
setFeatureConfig FeatureSingletonAppLockConfig tid status = do
  let enforce = applockEnforceAppLock (wssConfig status)
      timeout = applockInactivityTimeoutSecs (wssConfig status)

  retry x5 $ write insert (params LocalQuorum (tid, wssStatus status, enforce, timeout))
  where
    insert :: PrepQuery W (TeamId, FeatureStatus, EnforceAppLock, Int32) ()
    insert =
      fromString $
        "insert into team_features (team_id, app_lock_status, app_lock_enforce,\
        \ app_lock_inactivity_timeout_secs) values (?, ?, ?, ?)"
setFeatureConfig FeatureSingletonFileSharingConfig tid statusNoLock = setFeatureStatusC "file_sharing" tid (wssStatus statusNoLock)
setFeatureConfig FeatureSingletonSelfDeletingMessagesConfig tid status = do
  let statusValue = wssStatus status
      timeout = sdmEnforcedTimeoutSeconds . wssConfig $ status
  retry x5 $ write insert (params LocalQuorum (tid, statusValue, timeout))
  where
    insert :: PrepQuery W (TeamId, FeatureStatus, Int32) ()
    insert =
      "insert into team_features (team_id, self_deleting_messages_status,\
      \ self_deleting_messages_ttl) values (?, ?, ?)"
setFeatureConfig FeatureSingletonConferenceCallingConfig tid statusNoLock =
  retry x5 $ write insert (params LocalQuorum (tid, wssStatus statusNoLock))
  where
    renderFeatureTtl :: FeatureTTL -> String
    renderFeatureTtl = \case
      FeatureTTLSeconds d | d > 0 -> " using ttl " <> show d
      _ -> " using ttl 0" -- 0 or unlimited (delete a column's existing TTL by setting its value to zero)
    insert :: PrepQuery W (TeamId, FeatureStatus) ()
    insert =
      fromString $
        "insert into team_features (team_id,conference_calling) values (?, ?)"
          <> renderFeatureTtl (wssTTL statusNoLock)
setFeatureConfig FeatureSingletonGuestLinksConfig tid statusNoLock = setFeatureStatusC "guest_links_status" tid (wssStatus statusNoLock)
setFeatureConfig FeatureSingletonSndFactorPasswordChallengeConfig tid statusNoLock =
  setFeatureStatusC "snd_factor_password_challenge_status" tid (wssStatus statusNoLock)
setFeatureConfig FeatureSingletonSearchVisibilityInboundConfig tid statusNoLock = setFeatureStatusC "search_visibility_status" tid (wssStatus statusNoLock)
setFeatureConfig FeatureSingletonMLSConfig tid statusNoLock = do
  let status = wssStatus statusNoLock
  let MLSConfig protocolToggleUsers defaultProtocol allowedCipherSuites defaultCipherSuite supportedProtocols = wssConfig statusNoLock
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
setFeatureConfig FeatureSingletonMlsE2EIdConfig tid status = do
  let statusValue = wssStatus status
      vex = verificationExpiration . wssConfig $ status
      mUrl = acmeDiscoveryUrl . wssConfig $ status
      mCrlProxy = crlProxy . wssConfig $ status
      useProxy = useProxyOnMobile . wssConfig $ status
  retry x5 $ write insert (params LocalQuorum (tid, statusValue, truncate vex, mUrl, mCrlProxy, useProxy))
  where
    insert :: PrepQuery W (TeamId, FeatureStatus, Int32, Maybe HttpsUrl, Maybe HttpsUrl, Bool) ()
    insert =
      "insert into team_features (team_id, mls_e2eid_status, mls_e2eid_grace_period, mls_e2eid_acme_discovery_url, mls_e2eid_crl_proxy, mls_e2eid_use_proxy_on_mobile) values (?, ?, ?, ?, ?, ?)"
setFeatureConfig FeatureSingletonMlsMigration tid status = do
  let statusValue = wssStatus status
      config = wssConfig status

  retry x5 $ write insert (params LocalQuorum (tid, statusValue, config.startTime, config.finaliseRegardlessAfter))
  where
    insert :: PrepQuery W (TeamId, FeatureStatus, Maybe UTCTime, Maybe UTCTime) ()
    insert =
      "insert into team_features (team_id, mls_migration_status, mls_migration_start_time, mls_migration_finalise_regardless_after) values (?, ?, ?, ?)"
setFeatureConfig FeatureSingletonExposeInvitationURLsToTeamAdminConfig tid statusNoLock = setFeatureStatusC "expose_invitation_urls_to_team_admin" tid (wssStatus statusNoLock)
setFeatureConfig FeatureSingletonOutlookCalIntegrationConfig tid statusNoLock = setFeatureStatusC "outlook_cal_integration_status" tid (wssStatus statusNoLock)
setFeatureConfig FeatureSingletonEnforceFileDownloadLocationConfig tid status = do
  let statusValue = wssStatus status
      config = wssConfig status

  retry x5 $ write insert (params LocalQuorum (tid, statusValue, config.enforcedDownloadLocation))
  where
    insert :: PrepQuery W (TeamId, FeatureStatus, Maybe Text) ()
    insert =
      "insert into team_features (team_id, enforce_file_download_location_status, enforce_file_download_location) values (?, ?, ?)"
setFeatureConfig FeatureSingletonLimitedEventFanoutConfig tid statusNoLock =
  setFeatureStatusC "limited_event_fanout_status" tid (wssStatus statusNoLock)

getFeatureLockStatus :: MonadClient m => FeatureSingleton cfg -> TeamId -> m (Maybe LockStatus)
getFeatureLockStatus FeatureSingletonFileSharingConfig tid = getLockStatusC "file_sharing_lock_status" tid
getFeatureLockStatus FeatureSingletonSelfDeletingMessagesConfig tid = getLockStatusC "self_deleting_messages_lock_status" tid
getFeatureLockStatus FeatureSingletonGuestLinksConfig tid = getLockStatusC "guest_links_lock_status" tid
getFeatureLockStatus FeatureSingletonSndFactorPasswordChallengeConfig tid = getLockStatusC "snd_factor_password_challenge_lock_status" tid
getFeatureLockStatus FeatureSingletonMlsE2EIdConfig tid = getLockStatusC "mls_e2eid_lock_status" tid
getFeatureLockStatus FeatureSingletonMlsMigration tid = getLockStatusC "mls_migration_lock_status" tid
getFeatureLockStatus FeatureSingletonOutlookCalIntegrationConfig tid = getLockStatusC "outlook_cal_integration_lock_status" tid
getFeatureLockStatus FeatureSingletonMLSConfig tid = getLockStatusC "mls_lock_status" tid
getFeatureLockStatus FeatureSingletonEnforceFileDownloadLocationConfig tid = getLockStatusC "enforce_file_download_location_lock_status" tid
getFeatureLockStatus _ _ = pure Nothing

setFeatureLockStatus :: MonadClient m => FeatureSingleton cfg -> TeamId -> LockStatus -> m ()
setFeatureLockStatus FeatureSingletonFileSharingConfig tid status = setLockStatusC "file_sharing_lock_status" tid status
setFeatureLockStatus FeatureSingletonSelfDeletingMessagesConfig tid status = setLockStatusC "self_deleting_messages_lock_status" tid status
setFeatureLockStatus FeatureSingletonGuestLinksConfig tid status = setLockStatusC "guest_links_lock_status" tid status
setFeatureLockStatus FeatureSingletonSndFactorPasswordChallengeConfig tid status = setLockStatusC "snd_factor_password_challenge_lock_status" tid status
setFeatureLockStatus FeatureSingletonMlsE2EIdConfig tid status = setLockStatusC "mls_e2eid_lock_status" tid status
setFeatureLockStatus FeatureSingletonMlsMigration tid status = setLockStatusC "mls_migration_lock_status" tid status
setFeatureLockStatus FeatureSingletonOutlookCalIntegrationConfig tid status = setLockStatusC "outlook_cal_integration_lock_status" tid status
setFeatureLockStatus FeatureSingletonMLSConfig tid status = setLockStatusC "mls_lock_status" tid status
setFeatureLockStatus FeatureSingletonEnforceFileDownloadLocationConfig tid status = setLockStatusC "enforce_file_download_location_lock_status" tid status
setFeatureLockStatus _ _tid _status = pure ()

getTrivialConfigC ::
  forall m cfg.
  (MonadClient m, IsFeatureConfig cfg) =>
  String ->
  TeamId ->
  m (Maybe (WithStatusNoLock cfg))
getTrivialConfigC statusCol tid = do
  let q = query1 select (params LocalQuorum (Identity tid))
  mFeatureStatus <- (>>= runIdentity) <$> retry x1 q
  pure $ case mFeatureStatus of
    Nothing -> Nothing
    Just status -> Just . forgetLock $ setStatus status defFeatureStatus
  where
    select :: PrepQuery R (Identity TeamId) (Identity (Maybe FeatureStatus))
    select =
      fromString $
        "select "
          <> statusCol
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
  MonadClient m =>
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
  m [(TeamId, Maybe (WithStatusNoLock cfg))]
getFeatureConfigMulti proxy =
  pooledMapConcurrentlyN 8 (\tid -> getFeatureConfig proxy tid <&> (tid,))
