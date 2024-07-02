{-# LANGUAGE TemplateHaskell #-}

module Galley.Cassandra.GetAllTeamFeatureConfigs where

import Cassandra
import Cassandra qualified as C
import Data.Id
import Data.Misc (HttpsUrl)
import Data.Time
import Database.CQL.Protocol
import Galley.Cassandra.Instances ()
import Galley.Types.Teams (FeatureLegalHold (..))
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

allFeatureConfigsFromRow ::
  -- id of team of which we want to see the feature
  TeamId ->
  -- team id list is from "settings.exposeInvitationURLsTeamAllowlist"
  Maybe [TeamId] ->
  FeatureLegalHold ->
  Bool ->
  AllFeatureConfigs ->
  AllTeamFeatureConfigsRow ->
  AllFeatureConfigs
allFeatureConfigsFromRow ourteam allowListForExposeInvitationURLs featureLH hasTeamImplicitLegalhold serverConfigs row =
  AllFeatureConfigs
    { afcLegalholdStatus = legalholdComputeFeatureStatus row.legalhold,
      afcSSOStatus =
        computeConfig
          row.sso
          Nothing
          FeatureTTLUnlimited
          (Just SSOConfig)
          serverConfigs.afcSSOStatus,
      afcTeamSearchVisibilityAvailable =
        computeConfig
          row.searchVisibility
          Nothing
          FeatureTTLUnlimited
          (Just SearchVisibilityAvailableConfig)
          serverConfigs.afcTeamSearchVisibilityAvailable,
      afcSearchVisibilityInboundConfig =
        computeConfig
          row.searchVisibility
          Nothing
          FeatureTTLUnlimited
          (Just SearchVisibilityInboundConfig)
          serverConfigs.afcSearchVisibilityInboundConfig,
      afcValidateSAMLEmails =
        computeConfig
          row.validateSamlEmails
          Nothing
          FeatureTTLUnlimited
          (Just ValidateSAMLEmailsConfig)
          serverConfigs.afcValidateSAMLEmails,
      afcDigitalSignatures =
        computeConfig
          row.digitalSignatures
          Nothing
          FeatureTTLUnlimited
          (Just DigitalSignaturesConfig)
          serverConfigs.afcDigitalSignatures,
      afcAppLock =
        computeConfig
          row.appLock
          Nothing
          FeatureTTLUnlimited
          appLockConfig
          serverConfigs.afcAppLock,
      afcFileSharing =
        computeConfig
          row.fileSharing
          row.fileSharingLock
          FeatureTTLUnlimited
          (Just FileSharingConfig)
          serverConfigs.afcFileSharing,
      afcClassifiedDomains =
        computeConfig Nothing Nothing FeatureTTLUnlimited Nothing serverConfigs.afcClassifiedDomains,
      afcConferenceCalling =
        computeConfig
          row.conferenceCalling
          Nothing
          (fromMaybe FeatureTTLUnlimited row.conferenceCallingTtl)
          (Just ConferenceCallingConfig)
          serverConfigs.afcConferenceCalling,
      afcSelfDeletingMessages =
        computeConfig
          row.selfDeletingMessages
          row.selfDeletingMessagesLock
          FeatureTTLUnlimited
          selfDeletingMessagesConfig
          serverConfigs.afcSelfDeletingMessages,
      afcGuestLink =
        computeConfig
          row.guestLinks
          row.guestLinksLock
          FeatureTTLUnlimited
          (Just GuestLinksConfig)
          serverConfigs.afcGuestLink,
      afcSndFactorPasswordChallenge =
        computeConfig
          row.sndFactor
          row.sndFactorLock
          FeatureTTLUnlimited
          (Just SndFactorPasswordChallengeConfig)
          serverConfigs.afcSndFactorPasswordChallenge,
      afcMLS =
        computeConfig
          row.mls
          row.mlsLock
          FeatureTTLUnlimited
          mlsConfig
          serverConfigs.afcMLS,
      afcExposeInvitationURLsToTeamAdmin = exposeInvitationURLsComputeFeatureStatus row.exposeInvitationUrls,
      afcOutlookCalIntegration =
        computeConfig
          row.outlookCalIntegration
          row.outlookCalIntegrationLock
          FeatureTTLUnlimited
          (Just OutlookCalIntegrationConfig)
          serverConfigs.afcOutlookCalIntegration,
      afcMlsE2EId =
        computeConfig
          row.mlsE2eid
          row.mlsE2eidLock
          FeatureTTLUnlimited
          mlsE2eidConfig
          serverConfigs.afcMlsE2EId,
      afcMlsMigration =
        computeConfig
          row.mlsMigration
          row.mlsMigrationLock
          FeatureTTLUnlimited
          mlsMigrationConfig
          serverConfigs.afcMlsMigration,
      afcEnforceFileDownloadLocation =
        computeConfig
          row.enforceDownloadLocation
          row.enforceDownloadLocationLock
          FeatureTTLUnlimited
          downloadLocationConfig
          serverConfigs.afcEnforceFileDownloadLocation,
      afcLimitedEventFanout =
        computeConfig
          row.limitEventFanout
          Nothing
          FeatureTTLUnlimited
          (Just LimitedEventFanoutConfig)
          serverConfigs.afcLimitedEventFanout
    }
  where
    computeConfig :: Maybe FeatureStatus -> Maybe LockStatus -> FeatureTTL -> Maybe cfg -> WithStatus cfg -> WithStatus cfg
    computeConfig mDbStatus mDbLock dbTtl mDbCfg serverCfg =
      let withStatusNoLock = case (mDbStatus, mDbCfg) of
            (Just dbStatus, Just dbCfg) ->
              Just $
                WithStatusNoLock
                  { wssTTL = dbTtl,
                    wssStatus = dbStatus,
                    wssConfig = dbCfg
                  }
            _ -> Nothing
       in computeFeatureConfigForTeamUser withStatusNoLock mDbLock serverCfg

    -- FUTUREWORK: the following lines are duplicated in
    -- "Galley.Cassandra.TeamFeatures"; make sure the pairs don't diverge!
    appLockConfig = AppLockConfig <$> row.appLockEnforce <*> row.appLockInactivityTimeoutSecs

    selfDeletingMessagesConfig = SelfDeletingMessagesConfig <$> row.selfDeletingMessagesTtl

    mlsConfig =
      MLSConfig
        <$> maybe (Just []) (Just . C.fromSet) row.mlsToggleUsers
        <*> row.mlsDefaultProtocol
        <*> maybe (Just []) (Just . C.fromSet) row.mlsAllowedCipherSuites
        <*> row.mlsDefaultCipherSuite
        <*> maybe (Just []) (Just . C.fromSet) row.mlsSupportedProtocols

    mlsE2eidConfig =
      Just $
        MlsE2EIdConfig
          (toGracePeriodOrDefault row.mlsE2eidGracePeriod)
          row.mlsE2eidAcmeDiscoverUrl
          row.mlsE2eidMaybeCrlProxy
          (fromMaybe (useProxyOnMobile . wsConfig $ defFeatureStatus) row.mlsE2eidMaybeUseProxyOnMobile)
      where
        toGracePeriodOrDefault :: Maybe Int32 -> NominalDiffTime
        toGracePeriodOrDefault = maybe (verificationExpiration $ wsConfig defFeatureStatus) fromIntegral

    mlsMigrationConfig =
      Just $
        MlsMigrationConfig
          { startTime = row.mlsMigrationStartTime,
            finaliseRegardlessAfter = row.mlsMigrationFinalizeRegardlessAfter
          }

    downloadLocationConfig = Just $ EnforceFileDownloadLocationConfig row.enforceDownloadLocation_Location

    -- FUTUREWORK: this duplicates logic hidden elsewhere for the other getters and setters.  do not change lightly!
    exposeInvitationURLsComputeFeatureStatus ::
      Maybe FeatureStatus ->
      WithStatus ExposeInvitationURLsToTeamAdminConfig
    exposeInvitationURLsComputeFeatureStatus mFeatureStatus =
      if ourteam `elem` fromMaybe [] allowListForExposeInvitationURLs
        then
          serverConfigs.afcExposeInvitationURLsToTeamAdmin
            & maybe id setStatus mFeatureStatus
            & setLockStatus LockStatusUnlocked
        else serverConfigs.afcExposeInvitationURLsToTeamAdmin

    -- FUTUREWORK: this duplicates logic hidden elsewhere for the other getters and setters.  do not change lightly!
    legalholdComputeFeatureStatus :: Maybe FeatureStatus -> WithStatus LegalholdConfig
    legalholdComputeFeatureStatus mStatusValue = setStatus status defFeatureStatus
      where
        status =
          if isLegalHoldEnabledForTeam
            then FeatureStatusEnabled
            else FeatureStatusDisabled
        isLegalHoldEnabledForTeam =
          case featureLH of
            FeatureLegalHoldDisabledPermanently -> False
            FeatureLegalHoldDisabledByDefault -> maybe False ((==) FeatureStatusEnabled) mStatusValue
            FeatureLegalHoldWhitelistTeamsAndImplicitConsent -> hasTeamImplicitLegalhold

getAllFeatureConfigs :: (MonadClient m) => Maybe [TeamId] -> FeatureLegalHold -> Bool -> AllFeatureConfigs -> TeamId -> m AllFeatureConfigs
getAllFeatureConfigs allowListForExposeInvitationURLs featureLH hasTeamImplicitLegalhold serverConfigs tid = do
  mRow <- retry x1 $ query1 select (params LocalQuorum (Identity tid))
  pure
    $ allFeatureConfigsFromRow
      tid
      allowListForExposeInvitationURLs
      featureLH
      hasTeamImplicitLegalhold
      serverConfigs
    $ maybe emptyRow asRecord mRow
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
      \conference_calling, ttl(conference_calling), \
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
