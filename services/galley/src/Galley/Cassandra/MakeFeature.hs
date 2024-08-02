-- | Abstraction to fetch and store feature values from and to the database.
module Galley.Cassandra.MakeFeature where

import Cassandra
import Cassandra qualified as C
import Data.Functor
import Data.Functor.Identity
import Data.Id
import Data.Misc (HttpsUrl)
import Data.Time
import Galley.Cassandra.Instances ()
import Imports
import Wire.API.Conversation.Protocol (ProtocolTag)
import Wire.API.MLS.CipherSuite
import Wire.API.Team.Feature

class StoredFeature cfg where
  featureColumns :: String

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

instance StoredFeature LegalholdConfig where
  featureColumns = "legalhold_status"

instance MakeFeature SSOConfig

instance StoredFeature SSOConfig where
  featureColumns = "sso_status"

instance MakeFeature SearchVisibilityAvailableConfig

instance StoredFeature SearchVisibilityAvailableConfig where
  featureColumns = "search_visibility_status"

instance MakeFeature SearchVisibilityInboundConfig

instance StoredFeature SearchVisibilityInboundConfig where
  featureColumns = "search_visibility_status"

instance MakeFeature ValidateSAMLEmailsConfig

instance StoredFeature ValidateSAMLEmailsConfig where
  featureColumns = "validate_saml_emails"

instance MakeFeature DigitalSignaturesConfig

instance StoredFeature DigitalSignaturesConfig where
  featureColumns = "digital_signatures"

instance MakeFeature AppLockConfig where
  type FeatureRow AppLockConfig = (Maybe FeatureStatus, Maybe EnforceAppLock, Maybe Int32)

  mkFeature (status, enforce, timeout) =
    foldMap dbFeatureStatus status
      <> foldMap dbFeatureConfig (AppLockConfig <$> enforce <*> timeout)

instance StoredFeature AppLockConfig where
  featureColumns = "app_lock_status, app_lock_enforce, app_lock_inactivity_timeout_secs"

instance MakeFeature ClassifiedDomainsConfig

instance MakeFeature FileSharingConfig

instance StoredFeature FileSharingConfig where
  featureColumns = "file_sharing"

instance MakeFeature ConferenceCallingConfig where
  type FeatureRow ConferenceCallingConfig = (Maybe FeatureStatus, Maybe FeatureTTL, Maybe One2OneCalls)

  mkFeature (status, _, sftForOneToOne) =
    foldMap dbFeatureStatus status
      <> foldMap (dbFeatureConfig . ConferenceCallingConfig) sftForOneToOne

instance StoredFeature ConferenceCallingConfig where
  featureColumns = "conference_calling_status, ttl(conference_calling_status), conference_calling_one_to_one"

instance MakeFeature SelfDeletingMessagesConfig where
  type FeatureRow SelfDeletingMessagesConfig = (Maybe FeatureStatus, Maybe Int32)

  mkFeature (status, ttl) =
    foldMap dbFeatureStatus status
      <> foldMap (dbFeatureConfig . SelfDeletingMessagesConfig) ttl

instance StoredFeature SelfDeletingMessagesConfig where
  featureColumns = "self_deleting_messages_status, self_deleting_messages_ttl"

instance MakeFeature GuestLinksConfig

instance StoredFeature GuestLinksConfig where
  featureColumns = "guest_links_status"

instance MakeFeature SndFactorPasswordChallengeConfig

instance StoredFeature SndFactorPasswordChallengeConfig where
  featureColumns = "snd_factor_password_challenge_status"

instance MakeFeature ExposeInvitationURLsToTeamAdminConfig

instance StoredFeature ExposeInvitationURLsToTeamAdminConfig where
  featureColumns = "expose_invitation_urls_to_team_admin"

instance MakeFeature OutlookCalIntegrationConfig

instance StoredFeature OutlookCalIntegrationConfig where
  featureColumns = "outlook_cal_integration_status"

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
        ( MLSConfig (foldMap C.fromSet toggleUsers)
            <$> defProto
            <*> pure (foldMap C.fromSet ciphersuites)
            <*> defCiphersuite
            <*> pure (foldMap C.fromSet supportedProtos)
        )

instance StoredFeature MLSConfig where
  featureColumns =
    "mls_status, mls_default_protocol, mls_protocol_toggle_users, \
    \mls_allowed_ciphersuites, mls_default_ciphersuite, mls_supported_protocols"

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

instance StoredFeature MlsE2EIdConfig where
  featureColumns =
    "mls_e2eid_status, mls_e2eid_grace_period, mls_e2eid_acme_discovery_url, \
    \mls_e2eid_crl_proxy, mls_e2eid_use_proxy_on_mobile"

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

instance StoredFeature MlsMigrationConfig where
  featureColumns =
    "mls_migration_status, mls_migration_start_time, \
    \mls_migration_finalise_regardless_after"

instance MakeFeature EnforceFileDownloadLocationConfig where
  type FeatureRow EnforceFileDownloadLocationConfig = (Maybe FeatureStatus, Maybe Text)

  mkFeature (status, location) =
    foldMap dbFeatureStatus status
      <> dbFeatureConfig (EnforceFileDownloadLocationConfig location)

instance StoredFeature EnforceFileDownloadLocationConfig where
  featureColumns = "enforce_file_download_location_status, enforce_file_download_location"

instance MakeFeature LimitedEventFanoutConfig

instance StoredFeature LimitedEventFanoutConfig where
  featureColumns = "limited_event_fanout_status"

getFeature ::
  forall cfg m.
  (MonadClient m, MakeFeature cfg, StoredFeature cfg) =>
  TeamId ->
  m (DbFeature cfg)
getFeature tid = do
  row <- retry x1 $ query1 select (params LocalQuorum (Identity tid))
  pure $ foldMap (mkFeature . toRowType) row
  where
    select :: PrepQuery R (Identity TeamId) (FeatureRow cfg)
    select =
      fromString $
        "select "
          <> featureColumns @cfg
          <> " from team_features where team_id = ?"
