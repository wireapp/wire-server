{-# LANGUAGE TemplateHaskell #-}

module Galley.Cassandra.GetAllTeamFeatureConfigs where

import Cassandra
import Cassandra qualified as C
import Data.Id
import Data.Misc (HttpsUrl)
import Data.SOP
import Data.Time
import Database.CQL.Protocol
import Galley.Cassandra.Instances ()
import Galley.Cassandra.MakeFeature
import Imports
import Wire.API.Conversation.Protocol (ProtocolTag)
import Wire.API.MLS.CipherSuite
import Wire.API.Team.Feature

data AllTeamFeatureConfigsRow = AllTeamFeatureConfigsRow
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
    conferenceCallingOne2One :: Maybe One2OneCalls,
    conferenceCallingLock :: Maybe LockStatus,
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
    mlsE2eidMaybeCrlProxy :: Maybe HttpsUrl,
    mlsE2eidMaybeUseProxyOnMobile :: Maybe Bool,
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
  deriving (Generic, Show)

recordInstance ''AllTeamFeatureConfigsRow

emptyRow :: AllTeamFeatureConfigsRow
emptyRow =
  AllTeamFeatureConfigsRow
    { legalhold = Nothing,
      sso = Nothing,
      searchVisibility = Nothing,
      validateSamlEmails = Nothing,
      digitalSignatures = Nothing,
      appLock = Nothing,
      appLockEnforce = Nothing,
      appLockInactivityTimeoutSecs = Nothing,
      fileSharing = Nothing,
      fileSharingLock = Nothing,
      selfDeletingMessages = Nothing,
      selfDeletingMessagesTtl = Nothing,
      selfDeletingMessagesLock = Nothing,
      conferenceCalling = Nothing,
      conferenceCallingTtl = Nothing,
      conferenceCallingOne2One = Nothing,
      conferenceCallingLock = Nothing,
      guestLinks = Nothing,
      guestLinksLock = Nothing,
      sndFactor = Nothing,
      sndFactorLock = Nothing,
      mls = Nothing,
      mlsDefaultProtocol = Nothing,
      mlsToggleUsers = Nothing,
      mlsAllowedCipherSuites = Nothing,
      mlsDefaultCipherSuite = Nothing,
      mlsSupportedProtocols = Nothing,
      mlsLock = Nothing,
      mlsE2eid = Nothing,
      mlsE2eidGracePeriod = Nothing,
      mlsE2eidAcmeDiscoverUrl = Nothing,
      mlsE2eidMaybeCrlProxy = Nothing,
      mlsE2eidMaybeUseProxyOnMobile = Nothing,
      mlsE2eidLock = Nothing,
      mlsMigration = Nothing,
      mlsMigrationStartTime = Nothing,
      mlsMigrationFinalizeRegardlessAfter = Nothing,
      mlsMigrationLock = Nothing,
      exposeInvitationUrls = Nothing,
      outlookCalIntegration = Nothing,
      outlookCalIntegrationLock = Nothing,
      enforceDownloadLocation = Nothing,
      enforceDownloadLocation_Location = Nothing,
      enforceDownloadLocationLock = Nothing,
      limitEventFanout = Nothing
    }

allFeatureConfigsFromRow :: AllTeamFeatureConfigsRow -> AllFeatures DbFeatureWithLock
allFeatureConfigsFromRow row =
  mkFeatureWithLock Nothing (row.legalhold :* Nil)
    :* mkFeatureWithLock Nothing (row.sso :* Nil)
    :* mkFeatureWithLock Nothing (row.searchVisibility :* Nil)
    :* mkFeatureWithLock Nothing (row.searchVisibility :* Nil)
    :* mkFeatureWithLock Nothing (row.validateSamlEmails :* Nil)
    :* mkFeatureWithLock Nothing (row.digitalSignatures :* Nil)
    :* mkFeatureWithLock Nothing (row.appLock :* row.appLockEnforce :* row.appLockInactivityTimeoutSecs :* Nil)
    :* mkFeatureWithLock row.fileSharingLock (row.fileSharing :* Nil)
    :* mkFeatureWithLock Nothing Nil
    :* mkFeatureWithLock row.conferenceCallingLock (row.conferenceCalling :* row.conferenceCallingOne2One :* Nil)
    :* mkFeatureWithLock row.selfDeletingMessagesLock (row.selfDeletingMessages :* row.selfDeletingMessagesTtl :* Nil)
    :* mkFeatureWithLock row.guestLinksLock (row.guestLinks :* Nil)
    :* mkFeatureWithLock row.sndFactorLock (row.sndFactor :* Nil)
    :* mkFeatureWithLock row.mlsLock (row.mls :* row.mlsDefaultProtocol :* row.mlsToggleUsers :* row.mlsAllowedCipherSuites :* row.mlsDefaultCipherSuite :* row.mlsSupportedProtocols :* Nil)
    :* mkFeatureWithLock Nothing (row.exposeInvitationUrls :* Nil)
    :* mkFeatureWithLock row.outlookCalIntegrationLock (row.outlookCalIntegration :* Nil)
    :* mkFeatureWithLock row.mlsE2eidLock (row.mlsE2eid :* row.mlsE2eidGracePeriod :* row.mlsE2eidAcmeDiscoverUrl :* row.mlsE2eidMaybeCrlProxy :* row.mlsE2eidMaybeUseProxyOnMobile :* Nil)
    :* mkFeatureWithLock row.mlsMigrationLock (row.mlsMigration :* row.mlsMigrationStartTime :* row.mlsMigrationFinalizeRegardlessAfter :* Nil)
    :* mkFeatureWithLock row.enforceDownloadLocationLock (row.enforceDownloadLocation :* row.enforceDownloadLocation_Location :* Nil)
    :* mkFeatureWithLock Nothing (row.limitEventFanout :* Nil)
    :* Nil

getAllFeatureConfigs :: (MonadClient m) => TeamId -> m (AllFeatures DbFeatureWithLock)
getAllFeatureConfigs tid = do
  mRow <- retry x1 $ query1 select (params LocalQuorum (Identity tid))
  pure $ allFeatureConfigsFromRow $ maybe emptyRow asRecord mRow
  where
    select ::
      PrepQuery
        R
        (Identity TeamId)
        (TupleType AllTeamFeatureConfigsRow)
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
      \conference_calling_status, ttl(conference_calling_status), conference_calling_one_to_one, conference_calling, \
      \guest_links_status, guest_links_lock_status, \
      \snd_factor_password_challenge_status, snd_factor_password_challenge_lock_status, \
      \\
      \mls_status, mls_default_protocol, mls_protocol_toggle_users, mls_allowed_ciphersuites, \
      \mls_default_ciphersuite, mls_supported_protocols, mls_lock_status, \
      \\
      \mls_e2eid_status, mls_e2eid_grace_period, mls_e2eid_acme_discovery_url, mls_e2eid_crl_proxy, mls_e2eid_use_proxy_on_mobile, mls_e2eid_lock_status, \
      \\
      \mls_migration_status, mls_migration_start_time, mls_migration_finalise_regardless_after, \
      \mls_migration_lock_status, \
      \\
      \expose_invitation_urls_to_team_admin, \
      \outlook_cal_integration_status, outlook_cal_integration_lock_status, \
      \enforce_file_download_location_status, enforce_file_download_location, enforce_file_download_location_lock_status, \
      \limited_event_fanout_status \
      \from team_features where team_id = ?"
