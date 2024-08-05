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

  featureColumns :: String

  mkFeature :: NP Maybe (FeatureRow cfg) -> DbFeature cfg
  default mkFeature ::
    (FeatureRow cfg ~ '[FeatureStatus]) =>
    NP Maybe (FeatureRow cfg) ->
    DbFeature cfg
  mkFeature = foldMap dbFeatureStatus . hd

  unmkFeature :: Feature cfg -> NP Maybe (FeatureRow cfg)
  default unmkFeature ::
    (FeatureRow cfg ~ '[FeatureStatus]) =>
    Feature cfg ->
    NP Maybe (FeatureRow cfg)
  unmkFeature feat = Just feat.status :* Nil

mkFeatureWithLock ::
  (MakeFeature cfg) =>
  Maybe LockStatus ->
  NP Maybe (FeatureRow cfg) ->
  DbFeatureWithLock cfg
mkFeatureWithLock lockStatus row = DbFeatureWithLock lockStatus (mkFeature row)

instance MakeFeature LegalholdConfig where
  featureColumns = "legalhold_status"

instance MakeFeature SSOConfig where
  featureColumns = "sso_status"

instance MakeFeature SearchVisibilityAvailableConfig where
  featureColumns = "search_visibility_status"

instance MakeFeature SearchVisibilityInboundConfig where
  featureColumns = "search_visibility_status"

instance MakeFeature ValidateSAMLEmailsConfig where
  featureColumns = "validate_saml_emails"

instance MakeFeature DigitalSignaturesConfig where
  featureColumns = "digital_signatures"

instance MakeFeature AppLockConfig where
  type FeatureRow AppLockConfig = '[FeatureStatus, EnforceAppLock, Int32]
  featureColumns = "app_lock_status, app_lock_enforce, app_lock_inactivity_timeout_secs"

  mkFeature (status :* enforce :* timeout :* Nil) =
    foldMap dbFeatureStatus status
      <> foldMap dbFeatureConfig (AppLockConfig <$> enforce <*> timeout)

  unmkFeature feat =
    Just feat.status
      :* Just feat.config.applockEnforceAppLock
      :* Just feat.config.applockInactivityTimeoutSecs
      :* Nil

instance MakeFeature ClassifiedDomainsConfig where
  featureColumns = ""

instance MakeFeature FileSharingConfig where
  featureColumns = "file_sharing"

instance MakeFeature ConferenceCallingConfig where
  type FeatureRow ConferenceCallingConfig = '[FeatureStatus, One2OneCalls]
  featureColumns = "conference_calling_status, conference_calling_one_to_one"

  mkFeature (status :* calls :* Nil) =
    foldMap dbFeatureStatus status
      <> foldMap (dbFeatureConfig . ConferenceCallingConfig) calls

  unmkFeature feat =
    Just feat.status
      :* Just feat.config.one2OneCalls
      :* Nil

instance MakeFeature SelfDeletingMessagesConfig where
  type FeatureRow SelfDeletingMessagesConfig = '[FeatureStatus, Int32]
  featureColumns = "self_deleting_messages_status, self_deleting_messages_ttl"

  mkFeature (status :* ttl :* Nil) =
    foldMap dbFeatureStatus status
      <> foldMap (dbFeatureConfig . SelfDeletingMessagesConfig) ttl

  unmkFeature feat =
    Just feat.status
      :* Just feat.config.sdmEnforcedTimeoutSeconds
      :* Nil

instance MakeFeature GuestLinksConfig where
  featureColumns = "guest_links_status"

instance MakeFeature SndFactorPasswordChallengeConfig where
  featureColumns = "snd_factor_password_challenge_status"

instance MakeFeature ExposeInvitationURLsToTeamAdminConfig where
  featureColumns = "expose_invitation_urls_to_team_admin"

instance MakeFeature OutlookCalIntegrationConfig where
  featureColumns = "outlook_cal_integration_status"

instance MakeFeature MLSConfig where
  type
    FeatureRow MLSConfig =
      '[ FeatureStatus,
         ProtocolTag,
         (C.Set UserId),
         (C.Set CipherSuiteTag),
         CipherSuiteTag,
         (C.Set ProtocolTag)
       ]
  featureColumns =
    "mls_status, mls_default_protocol, mls_protocol_toggle_users, mls_allowed_ciphersuites, mls_default_ciphersuite, mls_supported_protocols"

  mkFeature
    ( status
        :* defProto
        :* toggleUsers
        :* ciphersuites
        :* defCiphersuite
        :* supportedProtos
        :* Nil
      ) =
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
    Just feat.status
      :* Just feat.config.mlsDefaultProtocol
      :* Just (C.Set feat.config.mlsProtocolToggleUsers)
      :* Just (C.Set feat.config.mlsAllowedCipherSuites)
      :* Just feat.config.mlsDefaultCipherSuite
      :* Just (C.Set feat.config.mlsSupportedProtocols)
      :* Nil

instance MakeFeature MlsE2EIdConfig where
  type
    FeatureRow MlsE2EIdConfig =
      '[ FeatureStatus,
         Int32,
         HttpsUrl,
         HttpsUrl,
         Bool
       ]
  featureColumns =
    "mls_e2eid_status, mls_e2eid_grace_period, mls_e2eid_acme_discovery_url, mls_e2eid_crl_proxy, mls_e2eid_use_proxy_on_mobile"

  mkFeature (status :* gracePeriod :* acmeDiscoveryUrl :* crlProxy :* useProxyOnMobile :* Nil) =
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
    Just feat.status
      :* Just (truncate feat.config.verificationExpiration)
      :* feat.config.acmeDiscoveryUrl
      :* feat.config.crlProxy
      :* Just feat.config.useProxyOnMobile
      :* Nil

instance MakeFeature MlsMigrationConfig where
  type
    FeatureRow MlsMigrationConfig =
      '[FeatureStatus, UTCTime, UTCTime]

  featureColumns =
    "mls_migration_status, mls_migration_start_time, mls_migration_finalise_regardless_after"

  mkFeature (status :* startTime :* finalizeAfter :* Nil) =
    foldMap dbFeatureStatus status
      <> dbFeatureConfig (MlsMigrationConfig startTime finalizeAfter)

  unmkFeature feat =
    Just feat.status
      :* feat.config.startTime
      :* feat.config.finaliseRegardlessAfter
      :* Nil

instance MakeFeature EnforceFileDownloadLocationConfig where
  type FeatureRow EnforceFileDownloadLocationConfig = '[FeatureStatus, Text]

  featureColumns = "enforce_file_download_location_status, enforce_file_download_location"

  mkFeature (status :* location :* Nil) =
    foldMap dbFeatureStatus status
      <> dbFeatureConfig (EnforceFileDownloadLocationConfig location)
  unmkFeature feat = Just feat.status :* feat.config.enforcedDownloadLocation :* Nil

instance MakeFeature LimitedEventFanoutConfig where
  featureColumns = "limited_event_fanout_status"

getFeature ::
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
getFeature tid = do
  row <- retry x1 $ query1 select (params LocalQuorum (Identity tid))
  pure $ foldMap (mkFeature . unfactorI . productTypeFrom) row
  where
    selectQ =
      "select "
        <> featureColumns @cfg
        <> " from team_features where team_id = ?"
    select :: PrepQuery R (Identity TeamId) (TupleP mrow)
    select = fromString selectQ

setFeature ::
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
  Feature cfg ->
  m ()
setFeature tid feat = do
  retry x5 $ write insert (params LocalQuorum (productTypeTo (I tid :* factorI (unmkFeature feat))))
  where
    n :: Int
    n = fromIntegral (demote @(Length row))

    insert :: PrepQuery W (TupleP (TeamId ': mrow)) ()
    insert =
      fromString $
        "insert into team_features (team_id, "
          <> featureColumns @cfg
          <> ") values ("
          <> intercalate "," (replicate (succ n) "?")
          <> ")"

-- | This is necessary in order to convert an @NP f xs@ type to something that
-- CQL can understand.
type family TupleP (xs :: [Type]) where
  TupleP '[a] = Identity a
  TupleP [a, b] = (a, b)
  TupleP [a, b, c] = (a, b, c)
  TupleP [a, b, c, d] = (a, b, c, d)
  TupleP [a, b, c, d, e] = (a, b, c, d, e)
  TupleP [a, b, c, d, e, f] = (a, b, c, d, e, f)
  TupleP [a, b, c, d, e, f, g] = (a, b, c, d, e, f, g)

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
