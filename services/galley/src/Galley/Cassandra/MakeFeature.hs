{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wwarn #-}

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
import GHC.TypeError
import GHC.TypeNats
import Galley.Cassandra.Instances ()
import Generics.SOP
import Imports hiding (Generic, Map)
import Wire.API.Conversation.Protocol (ProtocolTag)
import Wire.API.MLS.CipherSuite
import Wire.API.Team.Feature

class MakeFeature cfg where
  type FeatureRow cfg :: [Type]
  type FeatureRow cfg = '[FeatureStatus]

  featureColumns :: NP (K String) (FeatureRow cfg)

  mkFeature :: NP Maybe (FeatureRow cfg) -> DbFeature cfg
  default mkFeature ::
    (FeatureRow cfg ~ '[FeatureStatus]) =>
    NP Maybe (FeatureRow cfg) ->
    DbFeature cfg
  mkFeature = foldMap dbFeatureStatus . hd

  unmkFeature :: LockableFeature cfg -> NP Maybe (FeatureRow cfg)
  default unmkFeature ::
    (FeatureRow cfg ~ '[FeatureStatus]) =>
    LockableFeature cfg ->
    NP Maybe (FeatureRow cfg)
  unmkFeature feat = Just feat.status :* Nil

instance MakeFeature LegalholdConfig where
  featureColumns = K "legalhold_status" :* Nil

instance MakeFeature SSOConfig where
  featureColumns = K "sso_status" :* Nil

instance MakeFeature SearchVisibilityAvailableConfig where
  featureColumns = K "search_visibility_status" :* Nil

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

  mkFeature (status :* enforce :* timeout :* Nil) =
    foldMap dbFeatureStatus status
      <> foldMap dbFeatureConfig (AppLockConfig <$> enforce <*> timeout)

  unmkFeature feat =
    Just feat.status
      :* Just feat.config.applockEnforceAppLock
      :* Just feat.config.applockInactivityTimeoutSecs
      :* Nil

instance MakeFeature ClassifiedDomainsConfig where
  type FeatureRow ClassifiedDomainsConfig = '[]
  featureColumns = Nil

  mkFeature Nil = mempty
  unmkFeature _ = Nil

instance MakeFeature FileSharingConfig where
  type FeatureRow FileSharingConfig = '[LockStatus, FeatureStatus]
  featureColumns = K "file_sharing_lock_status" :* K "file_sharing" :* Nil

  mkFeature (lockStatus :* status :* Nil) =
    foldMap dbFeatureLockStatus lockStatus
      <> foldMap dbFeatureStatus status

  unmkFeature feat = Just feat.lockStatus :* Just feat.status :* Nil

instance MakeFeature ConferenceCallingConfig where
  type FeatureRow ConferenceCallingConfig = '[LockStatus, FeatureStatus, One2OneCalls]
  featureColumns =
    K "conference_calling"
      :* K "conference_calling_status"
      :* K "conference_calling_one_to_one"
      :* Nil

  mkFeature (lockStatus :* status :* calls :* Nil) =
    foldMap dbFeatureLockStatus lockStatus
      <> foldMap dbFeatureStatus status
      <> foldMap (dbFeatureConfig . ConferenceCallingConfig) calls

  unmkFeature feat =
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

  mkFeature (lockStatus :* status :* ttl :* Nil) =
    foldMap dbFeatureLockStatus lockStatus
      <> foldMap dbFeatureStatus status
      <> foldMap (dbFeatureConfig . SelfDeletingMessagesConfig) ttl

  unmkFeature feat =
    Just feat.lockStatus
      :* Just feat.status
      :* Just feat.config.sdmEnforcedTimeoutSeconds
      :* Nil

instance MakeFeature GuestLinksConfig where
  type FeatureRow GuestLinksConfig = '[LockStatus, FeatureStatus]
  featureColumns = K "guest_links_lock_status" :* K "guest_links_status" :* Nil

  mkFeature (lockStatus :* status :* Nil) =
    foldMap dbFeatureLockStatus lockStatus
      <> foldMap dbFeatureStatus status

  unmkFeature feat = Just feat.lockStatus :* Just feat.status :* Nil

instance MakeFeature SndFactorPasswordChallengeConfig where
  type FeatureRow SndFactorPasswordChallengeConfig = '[LockStatus, FeatureStatus]
  featureColumns =
    K "snd_factor_password_challenge_lock_status"
      :* K "snd_factor_password_challenge_status"
      :* Nil

  mkFeature (lockStatus :* status :* Nil) =
    foldMap dbFeatureLockStatus lockStatus
      <> foldMap dbFeatureStatus status

  unmkFeature feat = Just feat.lockStatus :* Just feat.status :* Nil

instance MakeFeature ExposeInvitationURLsToTeamAdminConfig where
  featureColumns = K "expose_invitation_urls_to_team_admin" :* Nil

instance MakeFeature OutlookCalIntegrationConfig where
  type FeatureRow OutlookCalIntegrationConfig = '[LockStatus, FeatureStatus]

  featureColumns =
    K "outlook_cal_integration_lock_status"
      :* K "outlook_cal_integration_status"
      :* Nil

  mkFeature (lockStatus :* status :* Nil) =
    foldMap dbFeatureLockStatus lockStatus
      <> foldMap dbFeatureStatus status

  unmkFeature feat = Just feat.lockStatus :* Just feat.status :* Nil

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

  mkFeature
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

  unmkFeature feat =
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

  mkFeature
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

  unmkFeature feat =
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

  mkFeature (lockStatus :* status :* startTime :* finalizeAfter :* Nil) =
    foldMap dbFeatureLockStatus lockStatus
      <> foldMap dbFeatureStatus status
      <> dbFeatureConfig (MlsMigrationConfig startTime finalizeAfter)

  unmkFeature feat =
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

  mkFeature (lockStatus :* status :* location :* Nil) =
    foldMap dbFeatureLockStatus lockStatus
      <> foldMap dbFeatureStatus status
      <> dbFeatureConfig (EnforceFileDownloadLocationConfig location)
  unmkFeature feat =
    Just feat.lockStatus
      :* Just feat.status
      :* feat.config.enforcedDownloadLocation
      :* Nil

instance MakeFeature LimitedEventFanoutConfig where
  featureColumns = K "limited_event_fanout_status" :* Nil

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
    Nil -> pure (mkFeature Nil)
    cols -> do
      let select :: PrepQuery R (Identity TeamId) (TupleP mrow)
          select =
            fromString $
              "select "
                <> intercalate ", " (hcollapse cols)
                <> " from team_features where team_id = ?"
      row <- retry x1 $ query1 select (params LocalQuorum (Identity tid))
      pure $ foldMap (mkFeature . unfactorI . productTypeFrom) row

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
          ( params LocalQuorum (productTypeTo (I tid :* factorI (unmkFeature feat)))
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

class (FeatureRow cfg ~ row) => StoreFeatureLockStatus (row :: [Type]) cfg | cfg -> row where
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

-- | This is necessary in order to convert an @NP f xs@ type to something that
-- CQL can understand.
type family TupleP (xs :: [Type]) where
  TupleP '[] = ()
  TupleP '[a] = Identity a
  TupleP [a, b] = (a, b)
  TupleP [a, b, c] = (a, b, c)
  TupleP [a, b, c, d] = (a, b, c, d)
  TupleP [a, b, c, d, e] = (a, b, c, d, e)
  TupleP [a, b, c, d, e, f] = (a, b, c, d, e, f)
  TupleP [a, b, c, d, e, f, g] = (a, b, c, d, e, f, g)
  TupleP [a, b, c, d, e, f, g, h] = (a, b, c, d, e, f, g, h)
  TupleP _ = TypeError ('Text "TupleP: tuple too long")

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
