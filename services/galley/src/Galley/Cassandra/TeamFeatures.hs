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
    Cassandra,
    FeatureStatusCassandra (..),
    getFeatureConfigMulti,
  )
where

import Cassandra
import qualified Cassandra as C
import Control.Monad.Trans.Maybe
import Data.Id
import Data.Proxy
import Data.Time (UTCTime)
import Galley.Cassandra.Instances ()
import Galley.Cassandra.Store
import qualified Galley.Effects.TeamFeatureStore as TFS
import Imports
import Polysemy
import Polysemy.Input
import UnliftIO.Async (pooledMapConcurrentlyN)
import Wire.API.Conversation.Protocol (ProtocolTag)
import Wire.API.MLS.CipherSuite
import Wire.API.Team.Feature

data Cassandra

type instance TFS.FeaturePersistentConstraint Cassandra = FeatureStatusCassandra

interpretTeamFeatureStoreToCassandra ::
  ( Member (Embed IO) r,
    Member (Input ClientState) r
  ) =>
  Sem (TFS.TeamFeatureStore Cassandra ': r) a ->
  Sem r a
interpretTeamFeatureStoreToCassandra = interpret $ \case
  TFS.GetFeatureConfig proxy tid -> embedClient $ getFeatureConfig proxy tid
  TFS.GetFeatureConfigMulti proxy tids -> embedClient $ getFeatureConfigMulti proxy tids
  TFS.SetFeatureConfig proxy tid wsnl -> embedClient $ setFeatureConfig proxy tid wsnl
  TFS.GetFeatureLockStatus proxy tid -> embedClient $ getFeatureLockStatus proxy tid
  TFS.SetFeatureLockStatus proxy tid ls -> embedClient $ setFeatureLockStatus proxy tid ls

class FeatureStatusCassandra cfg where
  getFeatureConfig :: MonadClient m => Proxy cfg -> TeamId -> m (Maybe (WithStatusNoLock cfg))
  setFeatureConfig :: MonadClient m => Proxy cfg -> TeamId -> WithStatusNoLock cfg -> m ()

  -- default implementation: no lock status
  getFeatureLockStatus :: MonadClient m => Proxy cfg -> TeamId -> m (Maybe LockStatus)
  getFeatureLockStatus _ _tid = pure Nothing
  setFeatureLockStatus :: MonadClient m => Proxy cfg -> TeamId -> LockStatus -> m ()
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
  (FeatureStatusCassandra cfg, MonadClient m, MonadUnliftIO m) =>
  Proxy cfg ->
  [TeamId] ->
  m [(TeamId, Maybe (WithStatusNoLock cfg))]
getFeatureConfigMulti proxy =
  pooledMapConcurrentlyN 8 (\tid -> getFeatureConfig proxy tid <&> (tid,))

instance FeatureStatusCassandra LegalholdConfig where
  getFeatureConfig _ = getTrivialConfigC "legalhold_status"
  setFeatureConfig _ tid statusNoLock = setFeatureStatusC "legalhold_status" tid (wssStatus statusNoLock)

instance FeatureStatusCassandra SSOConfig where
  getFeatureConfig _ = getTrivialConfigC "sso_status"
  setFeatureConfig _ tid statusNoLock = setFeatureStatusC "sso_status" tid (wssStatus statusNoLock)

instance FeatureStatusCassandra SearchVisibilityAvailableConfig where
  getFeatureConfig _ = getTrivialConfigC "search_visibility_status"
  setFeatureConfig _ tid statusNoLock = setFeatureStatusC "search_visibility_status" tid (wssStatus statusNoLock)

instance FeatureStatusCassandra ValidateSAMLEmailsConfig where
  getFeatureConfig _ = getTrivialConfigC "validate_saml_emails"
  setFeatureConfig _ tid statusNoLock = setFeatureStatusC "validate_saml_emails" tid (wssStatus statusNoLock)

instance FeatureStatusCassandra ClassifiedDomainsConfig where
  getFeatureConfig _ _tid = pure Nothing -- TODO(fisx): what's this about?
  setFeatureConfig _ _tid _statusNoLock = pure ()

instance FeatureStatusCassandra DigitalSignaturesConfig where
  getFeatureConfig _ = getTrivialConfigC "digital_signatures"
  setFeatureConfig _ tid statusNoLock = setFeatureStatusC "digital_signatures" tid (wssStatus statusNoLock)

instance FeatureStatusCassandra AppLockConfig where
  getFeatureConfig _ tid = runMaybeT $ do
    (mStatus, mEnforce, mTimeout) <-
      MaybeT . retry x1 $
        query1 select (params LocalQuorum (Identity tid))
    maybe mzero pure $
      WithStatusNoLock
        <$> mStatus
        <*> (AppLockConfig <$> mEnforce <*> mTimeout)
        <*> Just FeatureTTLUnlimited
    where
      select :: PrepQuery R (Identity TeamId) (Maybe FeatureStatus, Maybe EnforceAppLock, Maybe Int32)
      select =
        "select app_lock_status, app_lock_enforce, app_lock_inactivity_timeout_secs \
        \ from team_features where team_id = ?"

  setFeatureConfig _ tid status = do
    let enforce = applockEnforceAppLock (wssConfig status)
        timeout = applockInactivityTimeoutSecs (wssConfig status)

    retry x5 $ write insert (params LocalQuorum (tid, wssStatus status, enforce, timeout))
    where
      insert :: PrepQuery W (TeamId, FeatureStatus, EnforceAppLock, Int32) ()
      insert =
        fromString $
          "insert into team_features (team_id, app_lock_status, app_lock_enforce,\
          \ app_lock_inactivity_timeout_secs) values (?, ?, ?, ?)"

instance FeatureStatusCassandra FileSharingConfig where
  getFeatureConfig _ = getTrivialConfigC "file_sharing"
  setFeatureConfig _ tid statusNoLock = setFeatureStatusC "file_sharing" tid (wssStatus statusNoLock)
  getFeatureLockStatus _ = getLockStatusC "file_sharing_lock_status"
  setFeatureLockStatus _ = setLockStatusC "file_sharing_lock_status"

instance FeatureStatusCassandra SelfDeletingMessagesConfig where
  getFeatureConfig _ tid = runMaybeT $ do
    (mEnabled, mTimeout) <-
      MaybeT . retry x1 $
        query1 select (params LocalQuorum (Identity tid))
    maybe mzero pure $
      WithStatusNoLock
        <$> mEnabled
        <*> fmap SelfDeletingMessagesConfig mTimeout
        <*> Just FeatureTTLUnlimited
    where
      select :: PrepQuery R (Identity TeamId) (Maybe FeatureStatus, Maybe Int32)
      select =
        "select self_deleting_messages_status, self_deleting_messages_ttl\
        \ from team_features where team_id = ?"

  setFeatureConfig _ tid status = do
    let statusValue = wssStatus status
        timeout = sdmEnforcedTimeoutSeconds . wssConfig $ status
    retry x5 $ write insert (params LocalQuorum (tid, statusValue, timeout))
    where
      insert :: PrepQuery W (TeamId, FeatureStatus, Int32) ()
      insert =
        "insert into team_features (team_id, self_deleting_messages_status,\
        \ self_deleting_messages_ttl) values (?, ?, ?)"

  getFeatureLockStatus _ = getLockStatusC "self_deleting_messages_lock_status"
  setFeatureLockStatus _ = setLockStatusC "self_deleting_messages_lock_status"

instance FeatureStatusCassandra ConferenceCallingConfig where
  getFeatureConfig _ tid = do
    let q = query1 select (params LocalQuorum (Identity tid))
    retry x1 q <&> \case
      Nothing -> Nothing
      Just (Nothing, _) -> Nothing
      Just (Just status, mTtl) -> Just . forgetLock . setStatus status . setWsTTL (fromMaybe FeatureTTLUnlimited mTtl) $ defFeatureStatus
    where
      select :: PrepQuery R (Identity TeamId) (Maybe FeatureStatus, Maybe FeatureTTL)
      select =
        fromString $
          "select conference_calling, ttl(conference_calling) from team_features where team_id = ?"

  setFeatureConfig _ tid statusNoLock =
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

instance FeatureStatusCassandra GuestLinksConfig where
  getFeatureConfig _ = getTrivialConfigC "guest_links_status"
  setFeatureConfig _ tid statusNoLock = setFeatureStatusC "guest_links_status" tid (wssStatus statusNoLock)

  getFeatureLockStatus _ = getLockStatusC "guest_links_lock_status"
  setFeatureLockStatus _ = setLockStatusC "guest_links_lock_status"

instance FeatureStatusCassandra SndFactorPasswordChallengeConfig where
  getFeatureConfig _ = getTrivialConfigC "snd_factor_password_challenge_status"
  setFeatureConfig _ tid statusNoLock =
    setFeatureStatusC "snd_factor_password_challenge_status" tid (wssStatus statusNoLock)

  getFeatureLockStatus _ = getLockStatusC "snd_factor_password_challenge_lock_status"
  setFeatureLockStatus _ = setLockStatusC "snd_factor_password_challenge_lock_status"

instance FeatureStatusCassandra SearchVisibilityInboundConfig where
  getFeatureConfig _ = getTrivialConfigC "search_visibility_status"
  setFeatureConfig _ tid statusNoLock = setFeatureStatusC "search_visibility_status" tid (wssStatus statusNoLock)

instance FeatureStatusCassandra MLSConfig where
  getFeatureConfig _ tid = do
    m <- retry x1 $ query1 select (params LocalQuorum (Identity tid))
    pure $ case m of
      Nothing -> Nothing
      Just (status, defaultProtocol, protocolToggleUsers, allowedCipherSuites, defaultCipherSuite) ->
        WithStatusNoLock
          <$> status
          <*> ( MLSConfig
                  <$> maybe (Just []) (Just . C.fromSet) protocolToggleUsers
                  <*> defaultProtocol
                  <*> maybe (Just []) (Just . C.fromSet) allowedCipherSuites
                  <*> defaultCipherSuite
              )
          <*> Just FeatureTTLUnlimited
    where
      select :: PrepQuery R (Identity TeamId) (Maybe FeatureStatus, Maybe ProtocolTag, Maybe (C.Set UserId), Maybe (C.Set CipherSuiteTag), Maybe CipherSuiteTag)
      select =
        "select mls_status, mls_default_protocol, mls_protocol_toggle_users, mls_allowed_ciphersuites, \
        \mls_default_ciphersuite from team_features where team_id = ?"

  setFeatureConfig _ tid statusNoLock = do
    let status = wssStatus statusNoLock
    let MLSConfig protocolToggleUsers defaultProtocol allowedCipherSuites defaultCipherSuite = wssConfig statusNoLock
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
              defaultCipherSuite
            )
        )
    where
      insert :: PrepQuery W (TeamId, FeatureStatus, ProtocolTag, C.Set UserId, C.Set CipherSuiteTag, CipherSuiteTag) ()
      insert =
        "insert into team_features (team_id, mls_status, mls_default_protocol, \
        \mls_protocol_toggle_users, mls_allowed_ciphersuites, mls_default_ciphersuite) values (?, ?, ?, ?, ?, ?)"

instance FeatureStatusCassandra MlsE2EIdConfig where
  getFeatureConfig _ tid = do
    let q = query1 select (params LocalQuorum (Identity tid))
    retry x1 q <&> \case
      Nothing -> Nothing
      Just (Nothing, _) -> Nothing
      Just (mStatus, mTimeout) ->
        WithStatusNoLock
          <$> mStatus
          <*> maybe (pure $ wsConfig defFeatureStatus) (pure . MlsE2EIdConfig) (Just mTimeout)
          <*> Just FeatureTTLUnlimited
    where
      select :: PrepQuery R (Identity TeamId) (Maybe FeatureStatus, Maybe UTCTime)
      select =
        fromString $
          "select mls_e2eid_status, mls_e2eid_ver_exp from team_features where team_id = ?"

  setFeatureConfig _ tid status = do
    let statusValue = wssStatus status
        timeout = verificationExpiration . wssConfig $ status
    retry x5 $ write insert (params LocalQuorum (tid, statusValue, timeout))
    where
      insert :: PrepQuery W (TeamId, FeatureStatus, Maybe UTCTime) ()
      insert =
        "insert into team_features (team_id, mls_e2eid_status, mls_e2eid_ver_exp) values (?, ?, ?)"

  getFeatureLockStatus _ = getLockStatusC "mls_e2eid_lock_status"
  setFeatureLockStatus _ = setLockStatusC "mls_e2eid_lock_status"

instance FeatureStatusCassandra ExposeInvitationURLsToTeamAdminConfig where
  getFeatureConfig _ = getTrivialConfigC "expose_invitation_urls_to_team_admin"
  setFeatureConfig _ tid statusNoLock = setFeatureStatusC "expose_invitation_urls_to_team_admin" tid (wssStatus statusNoLock)

instance FeatureStatusCassandra OutlookCalIntegrationConfig where
  getFeatureConfig _ = getTrivialConfigC "outlook_cal_integration_status"
  setFeatureConfig _ tid statusNoLock = setFeatureStatusC "outlook_cal_integration_status" tid (wssStatus statusNoLock)

  getFeatureLockStatus _ = getLockStatusC "outlook_cal_integration_lock_status"
  setFeatureLockStatus _ = setLockStatusC "outlook_cal_integration_lock_status"
