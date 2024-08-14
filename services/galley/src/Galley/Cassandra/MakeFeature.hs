{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Abstraction to fetch and store feature values from and to the database.
module Galley.Cassandra.MakeFeature where

import Cassandra
import Cassandra qualified as C
import Data.Functor
import Data.Functor.Identity
import Data.Id
import Data.Kind
import Data.List.Singletons (Length)
import Data.Misc (HttpsUrl)
import Data.Singletons (demote)
import Data.Time
import GHC.TypeNats
import Galley.Cassandra.FeatureTH
import Galley.Cassandra.Instances ()
import Generics.SOP
import Imports hiding (Generic, Map)
import Wire.API.Conversation.Protocol (ProtocolTag)
import Wire.API.MLS.CipherSuite
import Wire.API.Team.Feature

-- | This is necessary in order to convert an @NP f xs@ type to something that
-- CQL can understand.
--
-- The generated code looks like:
-- @@
-- instance TupleP xs where
--   TupleP '[] = ()
--   TupleP '[a] = Identity a
--   TupleP '[a, b] = (a, b)
--   ...
-- @@
$generateTupleP

class MakeFeature cfg where
  type FeatureRow cfg :: [Type]
  type FeatureRow cfg = '[FeatureStatus]

  featureColumns :: NP (K String) (FeatureRow cfg)

  rowToFeature :: NP Maybe (FeatureRow cfg) -> DbFeature cfg
  default rowToFeature ::
    (FeatureRow cfg ~ '[FeatureStatus]) =>
    NP Maybe (FeatureRow cfg) ->
    DbFeature cfg
  rowToFeature = foldMap dbFeatureStatus . hd

  featureToRow :: LockableFeature cfg -> NP Maybe (FeatureRow cfg)
  default featureToRow ::
    (FeatureRow cfg ~ '[FeatureStatus]) =>
    LockableFeature cfg ->
    NP Maybe (FeatureRow cfg)
  featureToRow feat = Just feat.status :* Nil

instance MakeFeature LegalholdConfig where
  featureColumns = K "legalhold_status" :* Nil

instance MakeFeature SSOConfig where
  featureColumns = K "sso_status" :* Nil

instance MakeFeature SearchVisibilityAvailableConfig where
  featureColumns = K "search_visibility_status" :* Nil

-- | This feature shares its status column with
-- 'SearchVisibilityAvailableConfig'. This means that when fetching all
-- features, this column is repeated in the query, i.e. the query looks like:
-- @@
-- select ..., search_visibility_status, search_visibility_status, ... from team_features ...
-- @@
instance MakeFeature SearchVisibilityInboundConfig where
  featureColumns = K "search_visibility_status" :* Nil

instance MakeFeature ValidateSAMLEmailsConfig where
  featureColumns = K "validate_saml_emails" :* Nil

instance MakeFeature DigitalSignaturesConfig where
  featureColumns = K "digital_signatures" :* Nil

instance MakeFeature AppLockConfig where
  type FeatureRow AppLockConfig = '[FeatureStatus, EnforceAppLock, Int32]
  featureColumns =
    K "app_lock_status"
      :* K "app_lock_enforce"
      :* K "app_lock_inactivity_timeout_secs"
      :* Nil

  rowToFeature (status :* enforce :* timeout :* Nil) =
    foldMap dbFeatureStatus status
      <> foldMap dbFeatureConfig (AppLockConfig <$> enforce <*> timeout)

  featureToRow feat =
    Just feat.status
      :* Just feat.config.applockEnforceAppLock
      :* Just feat.config.applockInactivityTimeoutSecs
      :* Nil

instance MakeFeature ClassifiedDomainsConfig where
  type FeatureRow ClassifiedDomainsConfig = '[]
  featureColumns = Nil

  rowToFeature Nil = mempty
  featureToRow _ = Nil

instance MakeFeature FileSharingConfig where
  type FeatureRow FileSharingConfig = '[LockStatus, FeatureStatus]
  featureColumns = K "file_sharing_lock_status" :* K "file_sharing" :* Nil

  rowToFeature (lockStatus :* status :* Nil) =
    foldMap dbFeatureLockStatus lockStatus
      <> foldMap dbFeatureStatus status

  featureToRow feat = Just feat.lockStatus :* Just feat.status :* Nil

instance MakeFeature ConferenceCallingConfig where
  type FeatureRow ConferenceCallingConfig = '[LockStatus, FeatureStatus, One2OneCalls]
  featureColumns =
    K "conference_calling"
      :* K "conference_calling_status"
      :* K "conference_calling_one_to_one"
      :* Nil

  rowToFeature (lockStatus :* status :* calls :* Nil) =
    foldMap dbFeatureLockStatus lockStatus
      <> foldMap dbFeatureStatus status
      <> foldMap (dbFeatureConfig . ConferenceCallingConfig) calls

  featureToRow feat =
    Just feat.lockStatus
      :* Just feat.status
      :* Just feat.config.one2OneCalls
      :* Nil

instance MakeFeature SelfDeletingMessagesConfig where
  type FeatureRow SelfDeletingMessagesConfig = '[LockStatus, FeatureStatus, Int32]
  featureColumns =
    K "self_deleting_messages_lock_status"
      :* K "self_deleting_messages_status"
      :* K "self_deleting_messages_ttl"
      :* Nil

  rowToFeature (lockStatus :* status :* ttl :* Nil) =
    foldMap dbFeatureLockStatus lockStatus
      <> foldMap dbFeatureStatus status
      <> foldMap (dbFeatureConfig . SelfDeletingMessagesConfig) ttl

  featureToRow feat =
    Just feat.lockStatus
      :* Just feat.status
      :* Just feat.config.sdmEnforcedTimeoutSeconds
      :* Nil

instance MakeFeature GuestLinksConfig where
  type FeatureRow GuestLinksConfig = '[LockStatus, FeatureStatus]
  featureColumns = K "guest_links_lock_status" :* K "guest_links_status" :* Nil

  rowToFeature (lockStatus :* status :* Nil) =
    foldMap dbFeatureLockStatus lockStatus
      <> foldMap dbFeatureStatus status

  featureToRow feat = Just feat.lockStatus :* Just feat.status :* Nil

instance MakeFeature SndFactorPasswordChallengeConfig where
  type FeatureRow SndFactorPasswordChallengeConfig = '[LockStatus, FeatureStatus]
  featureColumns =
    K "snd_factor_password_challenge_lock_status"
      :* K "snd_factor_password_challenge_status"
      :* Nil

  rowToFeature (lockStatus :* status :* Nil) =
    foldMap dbFeatureLockStatus lockStatus
      <> foldMap dbFeatureStatus status

  featureToRow feat = Just feat.lockStatus :* Just feat.status :* Nil

instance MakeFeature ExposeInvitationURLsToTeamAdminConfig where
  featureColumns = K "expose_invitation_urls_to_team_admin" :* Nil

instance MakeFeature OutlookCalIntegrationConfig where
  type FeatureRow OutlookCalIntegrationConfig = '[LockStatus, FeatureStatus]

  featureColumns =
    K "outlook_cal_integration_lock_status"
      :* K "outlook_cal_integration_status"
      :* Nil

  rowToFeature (lockStatus :* status :* Nil) =
    foldMap dbFeatureLockStatus lockStatus
      <> foldMap dbFeatureStatus status

  featureToRow feat = Just feat.lockStatus :* Just feat.status :* Nil

instance MakeFeature MLSConfig where
  type
    FeatureRow MLSConfig =
      '[ LockStatus,
         FeatureStatus,
         ProtocolTag,
         (C.Set UserId),
         (C.Set CipherSuiteTag),
         CipherSuiteTag,
         (C.Set ProtocolTag)
       ]
  featureColumns =
    K "mls_lock_status"
      :* K "mls_status"
      :* K "mls_default_protocol"
      :* K "mls_protocol_toggle_users"
      :* K "mls_allowed_ciphersuites"
      :* K "mls_default_ciphersuite"
      :* K "mls_supported_protocols"
      :* Nil

  rowToFeature
    ( lockStatus
        :* status
        :* defProto
        :* toggleUsers
        :* ciphersuites
        :* defCiphersuite
        :* supportedProtos
        :* Nil
      ) =
      foldMap dbFeatureLockStatus lockStatus
        <> foldMap dbFeatureStatus status
        <> foldMap
          dbFeatureConfig
          ( MLSConfig (foldMap C.fromSet toggleUsers)
              <$> defProto
              <*> pure (foldMap C.fromSet ciphersuites)
              <*> defCiphersuite
              <*> pure (foldMap C.fromSet supportedProtos)
          )

  featureToRow feat =
    Just feat.lockStatus
      :* Just feat.status
      :* Just feat.config.mlsDefaultProtocol
      :* Just (C.Set feat.config.mlsProtocolToggleUsers)
      :* Just (C.Set feat.config.mlsAllowedCipherSuites)
      :* Just feat.config.mlsDefaultCipherSuite
      :* Just (C.Set feat.config.mlsSupportedProtocols)
      :* Nil

instance MakeFeature MlsE2EIdConfig where
  type
    FeatureRow MlsE2EIdConfig =
      '[ LockStatus,
         FeatureStatus,
         Int32,
         HttpsUrl,
         HttpsUrl,
         Bool
       ]
  featureColumns =
    K "mls_e2eid_lock_status"
      :* K "mls_e2eid_status"
      :* K "mls_e2eid_grace_period"
      :* K "mls_e2eid_acme_discovery_url"
      :* K "mls_e2eid_crl_proxy"
      :* K "mls_e2eid_use_proxy_on_mobile"
      :* Nil

  rowToFeature
    ( lockStatus
        :* status
        :* gracePeriod
        :* acmeDiscoveryUrl
        :* crlProxy
        :* useProxyOnMobile
        :* Nil
      ) =
      foldMap dbFeatureLockStatus lockStatus
        <> foldMap dbFeatureStatus status
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

  featureToRow feat =
    Just feat.lockStatus
      :* Just feat.status
      :* Just (truncate feat.config.verificationExpiration)
      :* feat.config.acmeDiscoveryUrl
      :* feat.config.crlProxy
      :* Just feat.config.useProxyOnMobile
      :* Nil

instance MakeFeature MlsMigrationConfig where
  type
    FeatureRow MlsMigrationConfig =
      '[LockStatus, FeatureStatus, UTCTime, UTCTime]

  featureColumns =
    K "mls_migration_lock_status"
      :* K "mls_migration_status"
      :* K "mls_migration_start_time"
      :* K "mls_migration_finalise_regardless_after"
      :* Nil

  rowToFeature (lockStatus :* status :* startTime :* finalizeAfter :* Nil) =
    foldMap dbFeatureLockStatus lockStatus
      <> foldMap dbFeatureStatus status
      <> dbFeatureConfig (MlsMigrationConfig startTime finalizeAfter)

  featureToRow feat =
    Just feat.lockStatus
      :* Just feat.status
      :* feat.config.startTime
      :* feat.config.finaliseRegardlessAfter
      :* Nil

instance MakeFeature EnforceFileDownloadLocationConfig where
  type FeatureRow EnforceFileDownloadLocationConfig = '[LockStatus, FeatureStatus, Text]

  featureColumns =
    K "enforce_file_download_location_lock_status"
      :* K "enforce_file_download_location_status"
      :* K "enforce_file_download_location"
      :* Nil

  rowToFeature (lockStatus :* status :* location :* Nil) =
    foldMap dbFeatureLockStatus lockStatus
      <> foldMap dbFeatureStatus status
      <> dbFeatureConfig (EnforceFileDownloadLocationConfig location)
  featureToRow feat =
    Just feat.lockStatus
      :* Just feat.status
      :* feat.config.enforcedDownloadLocation
      :* Nil

instance MakeFeature LimitedEventFanoutConfig where
  featureColumns = K "limited_event_fanout_status" :* Nil

instance MakeFeature DummyConfig where
  type FeatureRow DummyConfig = '[LockStatus, FeatureStatus, Int32]
  featureColumns = K "dummy_lock_status" :* K "dummy_status" :* K "dummy_level" :* Nil

  rowToFeature (lockStatus :* status :* level :* Nil) =
    foldMap dbFeatureLockStatus lockStatus
      <> foldMap dbFeatureStatus status
      <> foldMap (dbFeatureConfig . DummyConfig . fromIntegral) level

  featureToRow feat =
    Just feat.lockStatus
      :* Just feat.status
      :* Just (fromIntegral feat.config.dummyLevel)
      :* Nil

fetchFeature ::
  forall cfg m row mrow.
  ( MonadClient m,
    row ~ FeatureRow cfg,
    MakeFeature cfg,
    IsProductType (TupleP mrow) mrow,
    AllZip (IsF Maybe) row mrow,
    Tuple (TupleP mrow)
  ) =>
  TeamId ->
  m (DbFeature cfg)
fetchFeature tid = do
  case featureColumns @cfg of
    Nil -> pure (rowToFeature Nil)
    cols -> do
      mRow <- fetchFeatureRow @row @mrow tid cols
      pure $ foldMap rowToFeature mRow

fetchFeatureRow ::
  forall row mrow m.
  ( MonadClient m,
    IsProductType (TupleP mrow) mrow,
    AllZip (IsF Maybe) row mrow,
    Tuple (TupleP mrow)
  ) =>
  TeamId ->
  NP (K String) row ->
  m (Maybe (NP Maybe row))
fetchFeatureRow tid cols = do
  let select :: PrepQuery R (Identity TeamId) (TupleP mrow)
      select =
        fromString $
          "select "
            <> intercalate ", " (hcollapse cols)
            <> " from team_features where team_id = ?"
  row <- retry x1 $ query1 select (params LocalQuorum (Identity tid))
  pure $ fmap (unfactorI . productTypeFrom) row

storeFeature ::
  forall cfg m row mrow.
  ( MonadClient m,
    row ~ FeatureRow cfg,
    MakeFeature cfg,
    IsProductType (TupleP (TeamId : mrow)) (TeamId : mrow),
    AllZip (IsF Maybe) row mrow,
    Tuple (TupleP (TeamId : mrow)),
    KnownNat (Length row)
  ) =>
  TeamId ->
  LockableFeature cfg ->
  m ()
storeFeature tid feat = do
  if n == 0
    then pure ()
    else
      retry x5 $
        write
          insert
          ( params LocalQuorum (productTypeTo (I tid :* factorI (featureToRow feat)))
          )
  where
    n :: Int
    n = fromIntegral (demote @(Length row))

    insert :: PrepQuery W (TupleP (TeamId ': mrow)) ()
    insert =
      fromString $
        "insert into team_features (team_id, "
          <> intercalate ", " (hcollapse (featureColumns @cfg))
          <> ") values ("
          <> intercalate "," (replicate (succ n) "?")
          <> ")"

class (FeatureRow cfg ~ row) => StoreFeatureLockStatus (row :: [Type]) cfg where
  storeFeatureLockStatus' :: (MonadClient m) => TeamId -> Tagged cfg LockStatus -> m ()

instance
  {-# OVERLAPPING #-}
  ( FeatureRow cfg ~ (LockStatus ': row),
    MakeFeature cfg
  ) =>
  StoreFeatureLockStatus (LockStatus ': row) cfg
  where
  storeFeatureLockStatus' tid lock = do
    let col = unK (hd (featureColumns @cfg))
        insert :: PrepQuery W (TeamId, LockStatus) ()
        insert =
          fromString $
            "insert into team_features (team_id, " <> col <> ") values (?, ?)"
    retry x5 $ write insert (params LocalQuorum (tid, (untag lock)))

instance (FeatureRow cfg ~ row) => StoreFeatureLockStatus row cfg where
  storeFeatureLockStatus' _ _ = pure ()

storeFeatureLockStatus ::
  forall cfg m.
  (MonadClient m, StoreFeatureLockStatus (FeatureRow cfg) cfg) =>
  TeamId ->
  Tagged cfg LockStatus ->
  m ()
storeFeatureLockStatus = storeFeatureLockStatus' @(FeatureRow cfg)

-- | Convert @NP f [x1, ..., xn]@ to @NP I [f x1, ..., f xn]@.
--
-- This works because @I . f = f@.
factorI :: forall f xs ys. (AllZip (IsF f) xs ys) => NP f xs -> NP I ys
factorI Nil = Nil
factorI (x :* xs) = I x :* factorI xs

-- | Convert @NP I [f x1, ..., f xn]@ to @NP f [x1, ..., xn]@.
--
-- See 'factorI'.
unfactorI :: forall f xs ys. (AllZip (IsF f) xs ys) => NP I ys -> NP f xs
unfactorI Nil = Nil
unfactorI (I x :* xs) = x :* unfactorI xs

-- | This is to emulate a constraint-level lambda.
class (f x ~ y) => IsF f x y | y -> x

instance (f x ~ y) => IsF f x y
