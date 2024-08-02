{-# LANGUAGE CPP #-}

-- | Abstraction to fetch and store feature values from and to the database.
module Galley.Cassandra.MakeFeature where

import Cassandra
import Cassandra qualified as C
import Data.Functor
import Data.Functor.Identity
import Data.Id
import Data.Kind
import Data.List.Singletons
import Data.Misc (HttpsUrl)
import Data.Singletons
import Data.Time
import GHC.TypeNats
import Galley.Cassandra.Instances ()
import Generics.SOP
import Imports hiding (Generic)
import Wire.API.Conversation.Protocol (ProtocolTag)
import Wire.API.MLS.CipherSuite
import Wire.API.Team.Feature

class StoredFeature cfg where
  featureColumns :: String

class MakeFeature cfg where
  type FeatureReadRow cfg :: [Type]
  type FeatureReadRow cfg = '[Maybe FeatureStatus]
  type FeatureWriteRow cfg :: [Type]
  type FeatureWriteRow cfg = '[FeatureStatus]

  mkFeature :: NP I (FeatureReadRow cfg) -> DbFeature cfg
  default mkFeature ::
    (FeatureReadRow cfg ~ '[Maybe FeatureStatus]) =>
    NP I (FeatureReadRow cfg) ->
    DbFeature cfg
  mkFeature = foldMap dbFeatureStatus . unI . hd

  unmkFeature :: Feature cfg -> NP I (FeatureWriteRow cfg)
  default unmkFeature ::
    (FeatureWriteRow cfg ~ '[FeatureStatus]) =>
    Feature cfg ->
    NP I (FeatureWriteRow cfg)
  unmkFeature feat = I feat.status :* Nil

mkFeatureWithLock ::
  (MakeFeature cfg) =>
  Maybe LockStatus ->
  NP I (FeatureReadRow cfg) ->
  DbFeatureWithLock cfg
mkFeatureWithLock lockStatus row = DbFeatureWithLock lockStatus (mkFeature row)

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
  type FeatureReadRow AppLockConfig = '[Maybe FeatureStatus, Maybe EnforceAppLock, Maybe Int32]
  type FeatureWriteRow AppLockConfig = '[FeatureStatus, EnforceAppLock, Int32]

  mkFeature (I status :* I enforce :* I timeout :* Nil) =
    foldMap dbFeatureStatus status
      <> foldMap dbFeatureConfig (AppLockConfig <$> enforce <*> timeout)

  unmkFeature feat =
    I feat.status
      :* I feat.config.applockEnforceAppLock
      :* I feat.config.applockInactivityTimeoutSecs
      :* Nil

instance StoredFeature AppLockConfig where
  featureColumns = "app_lock_status, app_lock_enforce, app_lock_inactivity_timeout_secs"

instance MakeFeature ClassifiedDomainsConfig

instance MakeFeature FileSharingConfig

instance StoredFeature FileSharingConfig where
  featureColumns = "file_sharing"

instance MakeFeature ConferenceCallingConfig where
  type FeatureReadRow ConferenceCallingConfig = '[Maybe FeatureStatus, Maybe One2OneCalls]
  type FeatureWriteRow ConferenceCallingConfig = '[FeatureStatus, One2OneCalls]

  mkFeature (I status :* I calls :* Nil) =
    foldMap dbFeatureStatus status
      <> foldMap (dbFeatureConfig . ConferenceCallingConfig) calls

  unmkFeature feat =
    I feat.status
      :* I feat.config.one2OneCalls
      :* Nil

instance StoredFeature ConferenceCallingConfig where
  featureColumns = "conference_calling_status, conference_calling_one_to_one"

instance MakeFeature SelfDeletingMessagesConfig where
  type FeatureReadRow SelfDeletingMessagesConfig = '[Maybe FeatureStatus, Maybe Int32]
  type FeatureWriteRow SelfDeletingMessagesConfig = '[FeatureStatus, Int32]

  mkFeature (I status :* I ttl :* Nil) =
    foldMap dbFeatureStatus status
      <> foldMap (dbFeatureConfig . SelfDeletingMessagesConfig) ttl

  unmkFeature feat =
    I feat.status
      :* I feat.config.sdmEnforcedTimeoutSeconds
      :* Nil

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
    FeatureReadRow MLSConfig =
      '[ Maybe FeatureStatus,
         Maybe ProtocolTag,
         Maybe (C.Set UserId),
         Maybe (C.Set CipherSuiteTag),
         Maybe CipherSuiteTag,
         Maybe (C.Set ProtocolTag)
       ]

  type
    FeatureWriteRow MLSConfig =
      '[ FeatureStatus,
         ProtocolTag,
         (C.Set UserId),
         (C.Set CipherSuiteTag),
         CipherSuiteTag,
         (C.Set ProtocolTag)
       ]

  mkFeature (I status :* I defProto :* I toggleUsers :* I ciphersuites :* I defCiphersuite :* I supportedProtos :* Nil) =
    foldMap dbFeatureStatus status
      <> foldMap
        dbFeatureConfig
        ( MLSConfig (foldMap C.fromSet toggleUsers)
            <$> defProto
            <*> pure (foldMap C.fromSet ciphersuites)
            <*> defCiphersuite
            <*> pure (foldMap C.fromSet supportedProtos)
        )

  unmkFeature feat =
    I feat.status
      :* I feat.config.mlsDefaultProtocol
      :* I (C.Set feat.config.mlsProtocolToggleUsers)
      :* I (C.Set feat.config.mlsAllowedCipherSuites)
      :* I feat.config.mlsDefaultCipherSuite
      :* I (C.Set feat.config.mlsSupportedProtocols)
      :* Nil

instance StoredFeature MLSConfig where
  featureColumns =
    "mls_status, mls_default_protocol, mls_protocol_toggle_users, mls_allowed_ciphersuites, mls_default_ciphersuite, mls_supported_protocols"

instance MakeFeature MlsE2EIdConfig where
  type
    FeatureReadRow MlsE2EIdConfig =
      '[ Maybe FeatureStatus,
         Maybe Int32,
         Maybe HttpsUrl,
         Maybe HttpsUrl,
         Maybe Bool
       ]

  type
    FeatureWriteRow MlsE2EIdConfig =
      '[ FeatureStatus,
         Int32,
         Maybe HttpsUrl,
         Maybe HttpsUrl,
         Bool
       ]

  mkFeature (I status :* I gracePeriod :* I acmeDiscoveryUrl :* I crlProxy :* I useProxyOnMobile :* Nil) =
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

  unmkFeature feat =
    I feat.status
      :* I (truncate feat.config.verificationExpiration)
      :* I feat.config.acmeDiscoveryUrl
      :* I feat.config.crlProxy
      :* I feat.config.useProxyOnMobile
      :* Nil

instance StoredFeature MlsE2EIdConfig where
  featureColumns =
    "mls_e2eid_status, mls_e2eid_grace_period, mls_e2eid_acme_discovery_url, mls_e2eid_crl_proxy, mls_e2eid_use_proxy_on_mobile"

instance MakeFeature MlsMigrationConfig where
  type
    FeatureReadRow MlsMigrationConfig =
      '[ Maybe FeatureStatus,
         Maybe UTCTime,
         Maybe UTCTime
       ]

  type
    FeatureWriteRow MlsMigrationConfig =
      '[FeatureStatus, Maybe UTCTime, Maybe UTCTime]

  mkFeature (I status :* I startTime :* I finalizeAfter :* Nil) =
    foldMap dbFeatureStatus status
      <> dbFeatureConfig (MlsMigrationConfig startTime finalizeAfter)

  unmkFeature feat =
    I feat.status
      :* I feat.config.startTime
      :* I feat.config.finaliseRegardlessAfter
      :* Nil

instance StoredFeature MlsMigrationConfig where
  featureColumns =
    "mls_migration_status, mls_migration_start_time, mls_migration_finalise_regardless_after"

instance MakeFeature EnforceFileDownloadLocationConfig where
  type FeatureReadRow EnforceFileDownloadLocationConfig = '[Maybe FeatureStatus, Maybe Text]
  type FeatureWriteRow EnforceFileDownloadLocationConfig = '[FeatureStatus, Maybe Text]

  mkFeature (I status :* I location :* Nil) =
    foldMap dbFeatureStatus status
      <> dbFeatureConfig (EnforceFileDownloadLocationConfig location)
  unmkFeature feat = I feat.status :* I feat.config.enforcedDownloadLocation :* Nil

instance StoredFeature EnforceFileDownloadLocationConfig where
  featureColumns = "enforce_file_download_location_status, enforce_file_download_location"

instance MakeFeature LimitedEventFanoutConfig

instance StoredFeature LimitedEventFanoutConfig where
  featureColumns = "limited_event_fanout_status"

getFeature ::
  forall cfg m.
  ( MonadClient m,
    MakeFeature cfg,
    StoredFeature cfg,
    AsTuple (FeatureReadRow cfg),
    Tuple (TupleP (FeatureReadRow cfg))
  ) =>
  TeamId ->
  m (DbFeature cfg)
getFeature tid = do
  row <- retry x1 $ query1 select (params LocalQuorum (Identity tid))
  pure $ foldMap (mkFeature . fromTuple) row
  where
    selectQ =
      "select "
        <> featureColumns @cfg
        <> " from team_features where team_id = ?"
    select :: PrepQuery R (Identity TeamId) (TupleP (FeatureReadRow cfg))
    select = fromString selectQ

setFeature ::
  forall cfg m.
  ( MonadClient m,
    MakeFeature cfg,
    StoredFeature cfg,
    AsTuple (TeamId ': FeatureWriteRow cfg),
    Tuple (TupleP (TeamId ': FeatureWriteRow cfg)),
    KnownNat (Length (TeamId ': FeatureWriteRow cfg))
  ) =>
  TeamId ->
  Feature cfg ->
  m ()
setFeature tid feat = do
  retry x5 $ write insert (params LocalQuorum (toTuple (I tid :* unmkFeature feat)))
  where
    n :: Int
    n = fromIntegral (demote @(Length (TeamId ': FeatureWriteRow cfg)))

    insert :: PrepQuery W (TupleP (TeamId ': FeatureWriteRow cfg)) ()
    insert =
      fromString $
        "insert into team_features (team_id, "
          <> featureColumns @cfg
          <> ") values ("
          <> intercalate "," (replicate n "?")
          <> ")"

type AsTuple xs = (Code (TupleP xs) ~ '[xs], Generic (TupleP xs))

toTuple :: (AsTuple xs) => NP I xs -> TupleP xs
toTuple = to . SOP . Z

fromTuple :: (AsTuple xs) => TupleP xs -> NP I xs
fromTuple = unZ . unSOP . from

-- | This could be replaced in principle by a type class as follows:
-- @@
-- class TupleP xs t
-- instance (Code t ~ '[xs], Generic t) => TupleP xs t
-- @@
-- but then we wouldn't have the functional dependency xs -> t, which is needed
-- to keep inference sane.
type family TupleP (xs :: [Type]) where
  TupleP '[a] = Identity a
  TupleP [a, b] = (a, b)
  TupleP [a, b, c] = (a, b, c)
  TupleP [a, b, c, d] = (a, b, c, d)
  TupleP [a, b, c, d, e] = (a, b, c, d, e)
  TupleP [a, b, c, d, e, f] = (a, b, c, d, e, f)
  TupleP [a, b, c, d, e, f, g] = (a, b, c, d, e, f, g)
