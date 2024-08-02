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
  type FeatureRow ConferenceCallingConfig = (Maybe FeatureStatus, Maybe FeatureTTL, Maybe One2OneCalls)

  mkFeature (status, _, sftForOneToOne) =
    foldMap dbFeatureStatus status
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
        ( MLSConfig (foldMap C.fromSet toggleUsers)
            <$> defProto
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
