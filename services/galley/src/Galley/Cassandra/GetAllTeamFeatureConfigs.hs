{-# LANGUAGE TemplateHaskell #-}

module Galley.Cassandra.GetAllTeamFeatureConfigs where

import Cassandra
import Cassandra qualified as C
import Data.Id
import Data.Misc (HttpsUrl)
import Data.Time
import Database.CQL.Protocol
import Galley.Cassandra.Instances ()
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
    conferenceCallingSftForOne2One :: Maybe Bool,
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
      conferenceCallingSftForOne2One = Nothing,
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
  AllFeatures
    { afcLegalholdStatus = mkFeatureWithLock Nothing row.legalhold,
      afcSSOStatus = mkFeatureWithLock Nothing row.sso,
      afcTeamSearchVisibilityAvailable = mkFeatureWithLock Nothing row.searchVisibility,
      afcSearchVisibilityInboundConfig = mkFeatureWithLock Nothing row.searchVisibility,
      afcValidateSAMLEmails = mkFeatureWithLock Nothing row.validateSamlEmails,
      afcDigitalSignatures = mkFeatureWithLock Nothing row.digitalSignatures,
      afcAppLock =
        mkFeatureWithLock
          Nothing
          (row.appLock, row.appLockEnforce, row.appLockInactivityTimeoutSecs),
      afcFileSharing = mkFeatureWithLock row.fileSharingLock row.fileSharing,
      afcClassifiedDomains = mkFeatureWithLock Nothing Nothing,
      afcConferenceCalling =
        mkFeatureWithLock
          row.conferenceCallingLock
          ( row.conferenceCalling,
            row.conferenceCallingTtl,
            row.conferenceCallingSftForOne2One
          ),
      afcSelfDeletingMessages =
        mkFeatureWithLock
          row.selfDeletingMessagesLock
          ( row.selfDeletingMessages,
            row.selfDeletingMessagesTtl
          ),
      afcGuestLink = mkFeatureWithLock row.guestLinksLock row.guestLinks,
      afcSndFactorPasswordChallenge = mkFeatureWithLock row.sndFactorLock row.sndFactor,
      afcMLS =
        mkFeatureWithLock
          row.mlsLock
          ( row.mls,
            row.mlsDefaultProtocol,
            row.mlsToggleUsers,
            row.mlsAllowedCipherSuites,
            row.mlsDefaultCipherSuite,
            row.mlsSupportedProtocols
          ),
      afcExposeInvitationURLsToTeamAdmin = mkFeatureWithLock Nothing row.exposeInvitationUrls,
      afcOutlookCalIntegration =
        mkFeatureWithLock
          row.outlookCalIntegrationLock
          row.outlookCalIntegration,
      afcMlsE2EId =
        mkFeatureWithLock
          row.mlsE2eidLock
          ( row.mlsE2eid,
            row.mlsE2eidGracePeriod,
            row.mlsE2eidAcmeDiscoverUrl,
            row.mlsE2eidMaybeCrlProxy,
            row.mlsE2eidMaybeUseProxyOnMobile
          ),
      afcMlsMigration =
        mkFeatureWithLock
          row.mlsMigrationLock
          ( row.mlsMigration,
            row.mlsMigrationStartTime,
            row.mlsMigrationFinalizeRegardlessAfter
          ),
      afcEnforceFileDownloadLocation =
        mkFeatureWithLock
          row.enforceDownloadLocationLock
          ( row.enforceDownloadLocation,
            row.enforceDownloadLocation_Location
          ),
      afcLimitedEventFanout = mkFeatureWithLock Nothing row.limitEventFanout
    }

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
      \conference_calling, ttl(conference_calling), conference_calling_sft_for_one_to_one, conference_calling_lock_status, \
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

class (Tuple (FeatureRow cfg), HasRowType (FeatureRow cfg)) => MakeFeature cfg where
  type FeatureRow cfg
  type FeatureRow cfg = Identity (Maybe FeatureStatus)

  mkFeature :: RowType (FeatureRow cfg) -> DbFeature cfg
  default mkFeature ::
    (FeatureRow cfg ~ Identity (Maybe FeatureStatus)) =>
    RowType (FeatureRow cfg) ->
    DbFeature cfg
  mkFeature = foldMap dbFeatureStatus

mkFeatureWithLock ::
  (MakeFeature cfg) =>
  Maybe LockStatus ->
  RowType (FeatureRow cfg) ->
  DbFeatureWithLock cfg
mkFeatureWithLock lockStatus row = DbFeatureWithLock lockStatus (mkFeature row)

-- | Used to remove the annoying Identity wrapper around single-element rows.
type family RowType a where
  RowType (Identity a) = a
  RowType tuple = tuple

class HasRowType a where
  fromRowType :: RowType a -> a
  default fromRowType :: (RowType a ~ a) => RowType a -> a
  fromRowType = id

  toRowType :: a -> RowType a
  default toRowType :: (RowType a ~ a) => a -> RowType a
  toRowType = id

instance HasRowType (a, b)

instance HasRowType (a, b, c)

instance HasRowType (a, b, c, d)

instance HasRowType (a, b, c, d, e)

instance HasRowType (a, b, c, d, e, f)

instance HasRowType (Identity a) where
  fromRowType = Identity
  toRowType = runIdentity

instance MakeFeature LegalholdConfig

instance MakeFeature SSOConfig

instance MakeFeature SearchVisibilityAvailableConfig

instance MakeFeature SearchVisibilityInboundConfig

instance MakeFeature ValidateSAMLEmailsConfig

instance MakeFeature DigitalSignaturesConfig

instance MakeFeature AppLockConfig where
  type FeatureRow AppLockConfig = (Maybe FeatureStatus, Maybe EnforceAppLock, Maybe Int32)

  mkFeature (status, enforce, timeout) =
    foldMap dbFeatureStatus status
      <> foldMap dbFeatureConfig (AppLockConfig <$> enforce <*> timeout)

instance MakeFeature FileSharingConfig

instance MakeFeature ClassifiedDomainsConfig

instance MakeFeature ConferenceCallingConfig where
  type FeatureRow ConferenceCallingConfig = (Maybe FeatureStatus, Maybe FeatureTTL, Maybe Bool)

  mkFeature (status, ttl, sftForOneToOne) =
    foldMap dbFeatureStatus status
      <> foldMap dbFeatureTTL ttl
      <> foldMap (dbFeatureConfig . ConferenceCallingConfig) sftForOneToOne

instance MakeFeature SelfDeletingMessagesConfig where
  type FeatureRow SelfDeletingMessagesConfig = (Maybe FeatureStatus, Maybe Int32)

  mkFeature (status, ttl) =
    foldMap dbFeatureStatus status
      <> foldMap (dbFeatureConfig . SelfDeletingMessagesConfig) ttl

instance MakeFeature GuestLinksConfig

instance MakeFeature SndFactorPasswordChallengeConfig

instance MakeFeature ExposeInvitationURLsToTeamAdminConfig

instance MakeFeature OutlookCalIntegrationConfig

instance MakeFeature MLSConfig where
  type
    FeatureRow MLSConfig =
      ( Maybe FeatureStatus,
        Maybe ProtocolTag,
        Maybe (C.Set UserId),
        Maybe (C.Set CipherSuiteTag),
        Maybe CipherSuiteTag,
        Maybe (C.Set ProtocolTag)
      )

  mkFeature (status, defProto, toggleUsers, ciphersuites, defCiphersuite, supportedProtos) =
    foldMap dbFeatureStatus status
      <> foldMap
        dbFeatureConfig
        ( MLSConfig
            <$> pure (foldMap C.fromSet toggleUsers)
            <*> defProto
            <*> pure (foldMap C.fromSet ciphersuites)
            <*> defCiphersuite
            <*> pure (foldMap C.fromSet supportedProtos)
        )

instance MakeFeature MlsE2EIdConfig where
  type
    FeatureRow MlsE2EIdConfig =
      ( Maybe FeatureStatus,
        Maybe Int32,
        Maybe HttpsUrl,
        Maybe HttpsUrl,
        Maybe Bool
      )

  mkFeature (status, gracePeriod, acmeDiscoveryUrl, crlProxy, useProxyOnMobile) =
    foldMap dbFeatureStatus status
      <> dbFeatureModConfig
        ( \defCfg ->
            defCfg
              { verificationExpiration =
                  maybe defCfg.verificationExpiration fromIntegral gracePeriod,
                acmeDiscoveryUrl = acmeDiscoveryUrl,
                crlProxy = crlProxy,
                useProxyOnMobile = fromMaybe defCfg.useProxyOnMobile useProxyOnMobile
              }
        )

instance MakeFeature MlsMigrationConfig where
  type
    FeatureRow MlsMigrationConfig =
      ( Maybe FeatureStatus,
        Maybe UTCTime,
        Maybe UTCTime
      )

  mkFeature (status, startTime, finalizeAfter) =
    foldMap dbFeatureStatus status
      <> dbFeatureConfig (MlsMigrationConfig startTime finalizeAfter)

instance MakeFeature EnforceFileDownloadLocationConfig where
  type FeatureRow EnforceFileDownloadLocationConfig = (Maybe FeatureStatus, Maybe Text)

  mkFeature (status, location) =
    foldMap dbFeatureStatus status
      <> dbFeatureConfig (EnforceFileDownloadLocationConfig location)

instance MakeFeature LimitedEventFanoutConfig
