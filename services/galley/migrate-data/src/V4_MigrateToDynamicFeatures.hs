{-# OPTIONS -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module V4_MigrateToDynamicFeatures where

import Barbies.Bare
import Cassandra
import Cassandra qualified as C
import Conduit
import Data.Conduit.List qualified as C
import Data.Default
import Data.Id
import Data.Misc
import Data.Schema
import Data.Time.Clock
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Galley.DataMigration.Types
import Imports
import Wire.API.Conversation.Protocol
import Wire.API.MLS.CipherSuite
import Wire.API.Team.Feature

migration :: Migration
migration =
  Migration
    { version = MigrationVersion 4,
      text = "Migrate to dynamic features",
      action = runConduit $ do
        getFeatures @FeatureRow1 .| C.concat .| C.mapM_ writeFeatures
        getFeatures @FeatureRow2 .| C.concat .| C.mapM_ writeFeatures
    }

pageSize :: Int32
pageSize = 1000

----------------------------------------------------------------------------
-- Queries

-- Note: the row type is split into two arbitrary parts, because the whole tuple is too large for cql.

type FeatureRow1 =
  ( TeamId, -- team_id
    Maybe EnforceAppLock, -- app_lock_enforce
    Maybe Int32, -- app_lock_inactivity_timeout_secs
    Maybe FeatureStatus, -- app_lock_status
    Maybe LockStatus, -- conference_calling
    Maybe One2OneCalls, -- conference_calling_one_to_one
    Maybe FeatureStatus, -- conference_calling_status
    Maybe FeatureStatus, -- digital_signatures
    Maybe LockStatus, -- domain_registration_lock_status
    Maybe FeatureStatus, -- domain_registration_status
    Maybe Text, -- enforce_file_download_location
    Maybe LockStatus, -- enforce_file_download_location_lock_status
    Maybe FeatureStatus, -- enforce_file_download_location_status
    Maybe FeatureStatus, -- expose_invitation_urls_to_team_admin
    Maybe FeatureStatus, -- file_sharing
    Maybe LockStatus, -- file_sharing_lock_status
    Maybe LockStatus, -- guest_links_lock_status
    Maybe FeatureStatus, -- guest_links_status
    Maybe FeatureStatus, -- legalhold_status
    Maybe FeatureStatus -- limited_event_fanout_status
  )

type FeatureRow2 =
  ( TeamId,
    Maybe (Cassandra.Set CipherSuiteTag), -- mls_allowed_ciphersuites
    Maybe CipherSuiteTag, -- mls_default_ciphersuite
    Maybe ProtocolTag, -- mls_default_protocol
    Maybe HttpsUrl, -- mls_e2eid_acme_discovery_url
    Maybe HttpsUrl, -- mls_e2eid_crl_proxy
    Maybe Int32, -- mls_e2eid_grace_period
    Maybe LockStatus, -- mls_e2eid_lock_status
    Maybe FeatureStatus, -- mls_e2eid_status
    Maybe Bool, -- mls_e2eid_use_proxy_on_mobile
    Maybe LockStatus, -- mls_lock_status
    Maybe OptionalUTCTime, -- mls_migration_finalise_regardless_after
    Maybe LockStatus, -- mls_migration_lock_status
    Maybe OptionalUTCTime, -- mls_migration_start_time
    Maybe FeatureStatus, -- mls_migration_status
    Maybe (Cassandra.Set UserId), -- mls_protocol_toggle_users
    Maybe FeatureStatus, -- mls_status
    Maybe (Cassandra.Set ProtocolTag), -- mls_supported_protocols
    Maybe LockStatus, -- outlook_cal_integration_lock_status
    Maybe FeatureStatus, -- outlook_cal_integration_status
    Maybe FeatureStatus, -- search_visibility_inbound_status
    Maybe FeatureStatus, -- search_visibility_status
    Maybe LockStatus, -- self_deleting_messages_lock_status
    Maybe FeatureStatus, -- self_deleting_messages_status
    Maybe Int32, -- self_deleting_messages_ttl
    Maybe LockStatus, -- snd_factor_password_challenge_lock_status
    Maybe FeatureStatus, -- snd_factor_password_challenge_status
    Maybe FeatureStatus, -- sso_status
    Maybe FeatureStatus -- validate_saml_emails
  )

class (Tuple row) => IsFeatureRow row where
  rowQuery :: PrepQuery R () row
  writeFeatures :: (MonadClient m) => row -> m ()

instance IsFeatureRow FeatureRow1 where
  rowQuery =
    "select\
    \ team_id,\
    \ app_lock_enforc,\
    \ app_lock_inactivity_timeout_secs,\
    \ app_lock_status,\
    \ conference_calling,\
    \ conference_calling_one_to_one,\
    \ conference_calling_status,\
    \ digital_signatures,\
    \ domain_registration_lock_status,\
    \ domain_registration_status,\
    \ enforce_file_download_location,\
    \ enforce_file_download_location_lock_status,\
    \ enforce_file_download_location_status,\
    \ expose_invitation_urls_to_team_admin,\
    \ file_sharing,\
    \ file_sharing_lock_status,\
    \ guest_links_lock_status,\
    \ guest_links_status,\
    \ legalhold_status,\
    \ limited_event_fanout_status\
    \ from team_features"
  writeFeatures
    ( tid,
      app_lock_enforce,
      app_lock_inactivity_timeout_secs,
      app_lock_status,
      conference_calling,
      conference_calling_one_to_one,
      conference_calling_status,
      digital_signatures,
      domain_registration_lock_status,
      domain_registration_status,
      enforce_file_download_location,
      enforce_file_download_location_lock_status,
      enforce_file_download_location_status,
      expose_invitation_urls_to_team_admin,
      file_sharing,
      file_sharing_lock_status,
      guest_links_lock_status,
      guest_links_status,
      legalhold_status,
      limited_event_fanout_status
      ) = do
      writeFeature tid $
        Tagged @AppLockConfig $
          (def :: DbFeature)
            { status = app_lock_status,
              config =
                Just . DbConfig $
                  schemaToJSON
                    (AppLockConfig @Covered app_lock_enforce app_lock_inactivity_timeout_secs)
            }
      writeFeature tid $
        Tagged @ConferenceCallingConfig $
          DbFeature
            { status = conference_calling_status,
              lockStatus = conference_calling,
              config =
                DbConfig . schemaToJSON . ConferenceCallingConfig <$> conference_calling_one_to_one
            }
      writeFeature tid $
        Tagged @DigitalSignaturesConfig $
          (def :: DbFeature) {status = digital_signatures}
      writeFeature tid $
        Tagged @DomainRegistrationConfig $
          (def :: DbFeature)
            { status = domain_registration_status,
              lockStatus = domain_registration_lock_status
            }
      writeFeature tid $
        Tagged @EnforceFileDownloadLocationConfig $
          DbFeature
            { status = enforce_file_download_location_status,
              lockStatus = enforce_file_download_location_lock_status,
              config =
                Just . DbConfig . schemaToJSON . EnforceFileDownloadLocationConfig $
                  enforce_file_download_location
            }

      writeFeature tid $
        Tagged @ExposeInvitationURLsToTeamAdminConfig $
          (def :: DbFeature) {status = expose_invitation_urls_to_team_admin}

      writeFeature tid $
        Tagged @FileSharingConfig $
          (def :: DbFeature)
            { status = file_sharing,
              lockStatus = file_sharing_lock_status
            }
      writeFeature tid $
        Tagged @GuestLinksConfig $
          (def :: DbFeature)
            { status = guest_links_status,
              lockStatus = guest_links_lock_status
            }

      writeFeature tid $
        Tagged @LegalholdConfig $
          (def :: DbFeature) {status = legalhold_status}

      writeFeature tid $
        Tagged @LimitedEventFanoutConfig $
          (def :: DbFeature) {status = limited_event_fanout_status}

instance IsFeatureRow FeatureRow2 where
  rowQuery =
    "select \
    \ mls_allowed_ciphersuites, \
    \ mls_default_ciphersuite, \
    \ mls_default_protocol, \
    \ mls_e2eid_acme_discovery_url, \
    \ mls_e2eid_crl_proxy, \
    \ mls_e2eid_grace_period, \
    \ mls_e2eid_lock_status, \
    \ mls_e2eid_status, \
    \ mls_e2eid_use_proxy_on_mobile, \
    \ mls_lock_status, \
    \ mls_migration_finalise_regardless_after, \
    \ mls_migration_lock_status, \
    \ mls_migration_start_time, \
    \ mls_migration_status, \
    \ mls_protocol_toggle_users, \
    \ mls_status, \
    \ mls_supported_protocols, \
    \ outlook_cal_integration_lock_status, \
    \ outlook_cal_integration_status, \
    \ search_visibility_inbound_status, \
    \ search_visibility_status, \
    \ self_deleting_messages_lock_status, \
    \ self_deleting_messages_status, \
    \ self_deleting_messages_ttl, \
    \ snd_factor_password_challenge_lock_status, \
    \ snd_factor_password_challenge_status, \
    \ sso_status, \
    \ validate_saml_emails \
    \ from team_features"
  writeFeatures
    ( team_id,
      mls_allowed_ciphersuites,
      mls_default_ciphersuite,
      mls_default_protocol,
      mls_e2eid_acme_discovery_url,
      mls_e2eid_crl_proxy,
      mls_e2eid_grace_period,
      mls_e2eid_lock_status,
      mls_e2eid_status,
      mls_e2eid_use_proxy_on_mobile,
      mls_lock_status,
      mls_migration_finalise_regardless_after,
      mls_migration_lock_status,
      mls_migration_start_time,
      mls_migration_status,
      mls_protocol_toggle_users,
      mls_status,
      mls_supported_protocols,
      outlook_cal_integration_lock_status,
      outlook_cal_integration_status,
      search_visibility_inbound_status,
      search_visibility_status,
      self_deleting_messages_lock_status,
      self_deleting_messages_status,
      self_deleting_messages_ttl,
      snd_factor_password_challenge_lock_status,
      snd_factor_password_challenge_status,
      sso_status,
      validate_saml_emails
      ) = do
      writeFeature team_id $
        Tagged @MLSConfig $
          DbFeature
            { status = mls_status,
              lockStatus = mls_lock_status,
              config =
                Just . DbConfig $
                  schemaToJSON
                    ( MLSConfig @Covered
                        (fmap C.fromSet mls_protocol_toggle_users)
                        mls_default_protocol
                        (fmap C.fromSet mls_allowed_ciphersuites)
                        mls_default_ciphersuite
                        (fmap C.fromSet mls_supported_protocols)
                    )
            }

      writeFeature team_id $
        Tagged @MlsE2EIdConfig $
          DbFeature
            { status = mls_e2eid_status,
              lockStatus = mls_e2eid_lock_status,
              config =
                Just . DbConfig $
                  schemaToJSON
                    ( MlsE2EIdConfig @Covered
                        (fmap fromIntegral mls_e2eid_grace_period)
                        (Alt mls_e2eid_acme_discovery_url)
                        (Alt mls_e2eid_crl_proxy)
                        (maybe def UseProxyOnMobile mls_e2eid_use_proxy_on_mobile)
                    )
            }

      writeFeature team_id $
        Tagged @MlsMigrationConfig $
          DbFeature
            { status = mls_migration_status,
              lockStatus = mls_migration_lock_status,
              config =
                Just . DbConfig $
                  schemaToJSON
                    ( MlsMigrationConfig
                        (maybeToNullable (fmap unOptionalUTCTime mls_migration_start_time))
                        (maybeToNullable (fmap unOptionalUTCTime mls_migration_finalise_regardless_after))
                    )
            }

      writeFeature team_id $
        Tagged @OutlookCalIntegrationConfig $
          (def :: DbFeature)
            { status = outlook_cal_integration_status,
              lockStatus = outlook_cal_integration_lock_status
            }

      writeFeature team_id $
        Tagged @SearchVisibilityInboundConfig $
          (def :: DbFeature)
            { status = search_visibility_inbound_status
            }

      writeFeature team_id $
        Tagged @SearchVisibilityAvailableConfig $
          (def :: DbFeature)
            { status = search_visibility_status
            }

      writeFeature team_id $
        Tagged @SelfDeletingMessagesConfig $
          DbFeature
            { status = self_deleting_messages_status,
              lockStatus = self_deleting_messages_lock_status,
              config =
                DbConfig
                  . schemaToJSON
                  . SelfDeletingMessagesConfig
                  <$> self_deleting_messages_ttl
            }

      writeFeature team_id $
        Tagged @SndFactorPasswordChallengeConfig $
          (def :: DbFeature)
            { status = snd_factor_password_challenge_status,
              lockStatus = snd_factor_password_challenge_lock_status
            }

      writeFeature team_id $
        Tagged @SSOConfig $
          (def :: DbFeature) {status = sso_status}

      writeFeature team_id $
        Tagged @ValidateSAMLEmailsConfig
          (def :: DbFeature) {status = validate_saml_emails}

getFeatures :: (IsFeatureRow row, MonadClient m) => ConduitM () [row] m ()
getFeatures = paginateC rowQuery (paramsP LocalQuorum () pageSize) x5

-- instances copied from Galley.Cassandra.Instances

instance Cql EnforceAppLock where
  ctype = Tagged IntColumn
  toCql (EnforceAppLock False) = CqlInt 0
  toCql (EnforceAppLock True) = CqlInt 1
  fromCql (CqlInt n) = case n of
    0 -> pure (EnforceAppLock False)
    1 -> pure (EnforceAppLock True)
    _ -> Left "fromCql EnforceAppLock: int out of range"
  fromCql _ = Left "fromCql EnforceAppLock: int expected"

instance Cql ProtocolTag where
  ctype = Tagged IntColumn

  toCql = CqlInt . fromIntegral . fromEnum

  fromCql (CqlInt i) = do
    let i' = fromIntegral i
    if i' < fromEnum @ProtocolTag minBound
      || i' > fromEnum @ProtocolTag maxBound
      then Left $ "unexpected protocol: " ++ show i
      else Right $ toEnum i'
  fromCql _ = Left "protocol: int expected"

-- Optional time stamp. A 'Nothing' value is represented as 0.
newtype OptionalUTCTime = OptionalUTCTime {unOptionalUTCTime :: Maybe UTCTime}

instance Cql OptionalUTCTime where
  ctype = Tagged (untag (ctype @UTCTime))

  toCql = toCql . fromMaybe (posixSecondsToUTCTime 0) . unOptionalUTCTime

  fromCql x = do
    t <- fromCql x
    pure . OptionalUTCTime $ guard (utcTimeToPOSIXSeconds t /= 0) $> t

writeFeature ::
  forall cfg m.
  (IsFeatureConfig cfg, MonadClient m) =>
  TeamId ->
  Tagged cfg DbFeature ->
  m ()
writeFeature tid (Tagged feat) = do
  let q :: PrepQuery W (Maybe FeatureStatus, Maybe LockStatus, Maybe DbConfig, TeamId, Text) ()
      q = "update team_features_dyn set status = ?, lock_status = ?, config = ? where team = ? and feature = ?"
  retry x5 $
    write
      q
      ( params
          LocalQuorum
          ( feat.status,
            feat.lockStatus,
            feat.config,
            tid,
            featureName @cfg
          )
      )
