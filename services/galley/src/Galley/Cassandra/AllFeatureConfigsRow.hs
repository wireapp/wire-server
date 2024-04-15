{-# LANGUAGE TemplateHaskell #-}

module Galley.Cassandra.AllFeatureConfigsRow where

import Cassandra
import Cassandra qualified as C
import Data.Id
import Data.Misc (HttpsUrl)
import Data.Time
import Database.CQL.Protocol
import GHC.Records
import Galley.Cassandra.Instances ()
import Imports
import Wire.API.Conversation.Protocol (ProtocolTag)
import Wire.API.MLS.CipherSuite
import Wire.API.Team.Feature

data AllFeatureConfigsRow = AllFeatureConfigsRow
  { -- legalhold
    legalhold :: Maybe FeatureStatus,
    -- sso
    sso :: Maybe FeatureStatus,
    -- search visibility
    searchVisibility :: Maybe FeatureStatus,
    -- validate saml emails
    validateSamlEmails :: Maybe FeatureStatus,
    -- digital signatures
    digitalSignatures :: Maybe FeatureStatus,
    -- app lock
    appLock :: Maybe FeatureStatus,
    appLockEnforce :: Maybe EnforceAppLock,
    appLockInactivityTimeoutSecs :: Maybe Int32,
    -- file sharing
    fileSharing :: Maybe FeatureStatus,
    fileSharingLock :: Maybe LockStatus,
    -- self deleting messages
    selfDeletingMessages :: Maybe FeatureStatus,
    selfDeletingMessagesTtl :: Maybe Int32,
    selfDeletingMessagesLock :: Maybe LockStatus,
    -- conference calling
    conferenceCalling :: Maybe FeatureStatus,
    conferenceCallingTtl :: Maybe FeatureTTL,
    -- guest links
    guestLinks :: Maybe FeatureStatus,
    guestLinksLock :: Maybe LockStatus,
    -- snd factor
    sndFactor :: Maybe FeatureStatus,
    sndFactorLock :: Maybe LockStatus,
    -- mls
    mls :: Maybe FeatureStatus,
    mlsDefaultProtocol :: Maybe ProtocolTag,
    mlsToggleUsers :: Maybe (C.Set UserId),
    mlsAllowedCipherSuites :: Maybe (C.Set CipherSuiteTag),
    mlsDefaultCipherSuite :: Maybe CipherSuiteTag,
    mlsSupportedProtocols :: Maybe (C.Set ProtocolTag),
    mlsLock :: Maybe LockStatus,
    -- mls e2eid
    mlsE2eid :: Maybe FeatureStatus,
    mlsE2eidGracePeriod :: Maybe Int32,
    mlsE2eidAcmeDiscoverUrl :: Maybe HttpsUrl,
    mlsE2eidLock :: Maybe LockStatus,
    -- mls migration
    mlsMigration :: Maybe FeatureStatus,
    mlsMigrationStartTime :: Maybe UTCTime,
    mlsMigrationFinalizeRegardlessAfter :: Maybe UTCTime,
    mlsMigrationLock :: Maybe LockStatus,
    -- expose invitation urls
    exposeInvitationUrls :: Maybe FeatureStatus,
    -- outlook calendar integration
    outlookCalIntegration :: Maybe FeatureStatus,
    outlookCalIntegrationLock :: Maybe LockStatus,
    -- enforce download location
    enforceDownloadLocation :: Maybe FeatureStatus,
    enforceDownloadLocation_Location :: Maybe Text,
    enforceDownloadLocationLock :: Maybe LockStatus,
    -- limit event fanout
    limitEventFanout :: Maybe FeatureStatus
  }

recordInstance ''AllFeatureConfigsRow

allFeatureConfigsFromRow :: AllFeatureConfigs -> AllFeatureConfigsRow -> AllFeatureConfigs
allFeatureConfigsFromRow serverConfigs row =
  AllFeatureConfigs
    { afcLegalholdStatus = mkSimple @"legalhold" @"afcLegalholdStatus",
      afcSSOStatus = mkSimple @"sso" @"afcSSOStatus",
      afcTeamSearchVisibilityAvailable = mkSimple @"searchVisibility" @"afcTeamSearchVisibilityAvailable",
      afcSearchVisibilityInboundConfig = mkSimple @"searchVisibility" @"afcSearchVisibilityInboundConfig",
      afcValidateSAMLEmails = mkSimple @"validateSamlEmails" @"afcValidateSAMLEmails",
      afcDigitalSignatures = mkSimple @"digitalSignatures" @"afcDigitalSignatures",
      afcAppLock = mkWithConfig @"appLock" @"afcAppLock" appLockConfig,
      afcFileSharing = mkSimpleWithLock @"fileSharing" @"fileSharingLock" @"afcFileSharing",
      afcClassifiedDomains = computeFeatureConfigForTeamUser Nothing Nothing serverConfigs.afcClassifiedDomains,
      afcConferenceCalling = undefined,
      afcSelfDeletingMessages = undefined,
      afcGuestLink = undefined,
      afcSndFactorPasswordChallenge = undefined,
      afcMLS = undefined,
      afcExposeInvitationURLsToTeamAdmin = undefined,
      afcOutlookCalIntegration = undefined,
      afcMlsE2EId = undefined,
      afcMlsMigration = undefined,
      afcEnforceFileDownloadLocation = undefined,
      afcLimitedEventFanout = undefined
    }
  where
    mkWithStatusNoLockNoConfig :: forall cfg. IsFeatureConfig cfg => Maybe FeatureStatus -> Maybe (WithStatusNoLock cfg)
    mkWithStatusNoLockNoConfig = fmap (forgetLock . (flip setStatus defFeatureStatus))

    mkSimple ::
      forall rowName afcName cfg.
      ( IsFeatureConfig cfg,
        HasField rowName AllFeatureConfigsRow (Maybe FeatureStatus),
        HasField afcName AllFeatureConfigs (WithStatus cfg)
      ) =>
      WithStatus cfg
    mkSimple =
      computeFeatureConfigForTeamUser (mkWithStatusNoLockNoConfig (getField @rowName row)) Nothing (getField @afcName serverConfigs)

    mkSimpleWithLock ::
      forall rowName rowLockName afcName cfg.
      ( IsFeatureConfig cfg,
        HasField rowName AllFeatureConfigsRow (Maybe FeatureStatus),
        HasField rowLockName AllFeatureConfigsRow (Maybe LockStatus),
        HasField afcName AllFeatureConfigs (WithStatus cfg)
      ) =>
      WithStatus cfg
    mkSimpleWithLock =
      computeFeatureConfigForTeamUser (mkWithStatusNoLockNoConfig (getField @rowName row)) (getField @rowLockName row) (getField @afcName serverConfigs)

    mkWithConfig ::
      forall rowName afcName cfg.
      ( HasField rowName AllFeatureConfigsRow (Maybe FeatureStatus),
        HasField afcName AllFeatureConfigs (WithStatus cfg)
      ) =>
      Maybe cfg ->
      WithStatus cfg
    mkWithConfig mCfg =
      let withoutLock =
            WithStatusNoLock
              <$> (getField @rowName row)
              <*> mCfg
              <*> Just FeatureTTLUnlimited
       in computeFeatureConfigForTeamUser withoutLock Nothing (getField @afcName serverConfigs)

    appLockConfig = AppLockConfig <$> row.appLockEnforce <*> row.appLockInactivityTimeoutSecs

-- mkAppLock :: WithStatus AppLockConfig
-- mkAppLock =
--   let withoutLock = WithStatusNoLock <$> row.appLock <*> fmap SelfDeletingMessagesConfig row.appLockInactivityTimeoutSecs <*> Just FeatureTTLUnlimited
--   computeFeatureConfigForTeamUser withoutLock Nothing serverConfig.afcAppLock

getAllFeatureConfigs :: MonadClient m => AllFeatureConfigs -> TeamId -> m (Maybe AllFeatureConfigs)
getAllFeatureConfigs serverConfigs tid = do
  row <- retry x1 $ query1 select (params LocalQuorum (Identity tid))
  pure $ allFeatureConfigsFromRow serverConfigs . asRecord <$> row
  where
    select ::
      PrepQuery
        R
        (Identity TeamId)
        (TupleType AllFeatureConfigsRow)
    select =
      "select \
      \legalhold_status, \
      \sso_status, \
      \search_visibility_status, \
      \validate_saml_emails, \
      \digital_signatures, \
      \app_lock_status, app_lock_enforce, app_lock_inactivity_timeout_secs, \
      \file_sharing, file_sharing_lock_status, \
      \self_deleting_messages_status, self_deleting_messages_ttl, self_deleting_messages_lock_status, \
      \conference_calling, ttl(conference_calling), \
      \guest_links_status, guest_links_lock_status, \
      \snd_factor_password_challenge_status, snd_factor_password_challenge_lock_status, \
      \\
      \mls_status, mls_default_protocol, mls_protocol_toggle_users, mls_allowed_ciphersuites, \
      \mls_default_ciphersuite, mls_supported_protocols, mls_lock_status, \
      \\
      \mls_e2eid_status, mls_e2eid_grace_period, mls_e2eid_acme_discovery_url, mls_e2eid_lock_status, \
      \\
      \mls_migration_status, mls_migration_start_time, mls_migration_finalise_regardless_after, \
      \mls_migration_lock_status, \
      \\
      \expose_invitation_urls_to_team_admin, \
      \outlook_cal_integration_status, outlook_cal_integration_lock_status, \
      \enforce_file_download_location_status, enforce_file_download_location, enforce_file_download_location_lock_status, \
      \limited_event_fanout_status \
      \from team_features where team_id = ?"
