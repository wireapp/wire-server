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

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

module Wire.API.Team.Feature.Defaults where

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types qualified as A
import Data.ByteString (toStrict)
import Data.ByteString.UTF8 qualified as UTF8
import Data.Default
import Data.Kind
import Data.SOP
import Imports
import Wire.API.Team.Feature

-- | Used to extract the feature config type out of 'FeatureDefaults' or
-- related types.
type family ConfigOf a

-- | Open data family of server-wide default values for features.
-- Instances live in service-specific packages (e.g., galley-types).
data family FeatureDefaults cfg

-- | Link a 'FeatureDefaults cfg' to its config type.
type instance ConfigOf (FeatureDefaults cfg) = cfg

-- Convert a feature default value to an actual 'LockableFeature'.
type GetFeatureDefaults :: Type -> Constraint
class GetFeatureDefaults a where
  featureDefaults1 :: a -> LockableFeature (ConfigOf a)

type instance ConfigOf (Feature cfg) = cfg

instance (IsFeatureConfig cfg) => GetFeatureDefaults (Feature cfg) where
  featureDefaults1 = withLockStatus (def @(LockableFeature cfg)).lockStatus

newtype FixedDefaults cfg = FixedDefaults (FeatureDefaults cfg)

type instance ConfigOf (FixedDefaults cfg) = cfg

instance (IsFeatureConfig cfg) => GetFeatureDefaults (FixedDefaults cfg) where
  featureDefaults1 _ = def

type instance ConfigOf (LockableFeature cfg) = cfg

instance GetFeatureDefaults (LockableFeature cfg) where
  featureDefaults1 = id

-- Parsing utilities for defaults from JSON objects
class ParseFeatureDefaults a where
  parseFeatureDefaults :: A.Object -> A.Parser a

newtype RequiredField cfg = RequiredField (FeatureDefaults cfg)

instance
  (IsFeatureConfig cfg, FromJSON (FeatureDefaults cfg)) =>
  ParseFeatureDefaults (RequiredField cfg)
  where
  parseFeatureDefaults obj = RequiredField <$> obj .: featureKey @cfg

newtype OptionalField cfg = OptionalField (FeatureDefaults cfg)

instance
  ( IsFeatureConfig cfg,
    Default (FeatureDefaults cfg),
    FromJSON (FeatureDefaults cfg)
  ) =>
  ParseFeatureDefaults (OptionalField cfg)
  where
  parseFeatureDefaults obj = OptionalField <$> obj .:? featureKey @cfg .!= def

newtype Defaults a = Defaults {_unDefaults :: a}

instance (FromJSON a) => FromJSON (Defaults a) where
  parseJSON = withObject "default object" $ \ob ->
    Defaults <$> (ob .: "defaults")

-- Produce the JSON key for a feature config based on its symbol name
featureKey :: forall cfg. (IsFeatureConfig cfg) => Key.Key
featureKey = Key.fromText $ featureName @cfg

-- Data family instances and JSON parsing for all features

data instance FeatureDefaults LegalholdConfig
  = FeatureLegalHoldDisabledPermanently
  | FeatureLegalHoldDisabledByDefault
  | FeatureLegalHoldWhitelistTeamsAndImplicitConsent
  deriving stock (Eq, Ord, Show)
  deriving (ParseFeatureDefaults) via RequiredField LegalholdConfig
  deriving (GetFeatureDefaults) via FixedDefaults LegalholdConfig

instance FromJSON (FeatureDefaults LegalholdConfig) where
  parseJSON (String "disabled-permanently") = pure $ FeatureLegalHoldDisabledPermanently
  parseJSON (String "disabled-by-default") = pure $ FeatureLegalHoldDisabledByDefault
  parseJSON (String "whitelist-teams-and-implicit-consent") = pure FeatureLegalHoldWhitelistTeamsAndImplicitConsent
  parseJSON bad = fail $ "FeatureLegalHold: " <> (UTF8.toString . toStrict . encode $ bad)

data instance FeatureDefaults SSOConfig
  = FeatureSSOEnabledByDefault
  | FeatureSSODisabledByDefault
  deriving stock (Eq, Ord, Show)
  deriving (ParseFeatureDefaults) via RequiredField SSOConfig

instance FromJSON (FeatureDefaults SSOConfig) where
  parseJSON (String "enabled-by-default") = pure FeatureSSOEnabledByDefault
  parseJSON (String "disabled-by-default") = pure FeatureSSODisabledByDefault
  parseJSON bad = fail $ "FeatureSSO: " <> (UTF8.toString . toStrict . encode $ bad)

instance GetFeatureDefaults (FeatureDefaults SSOConfig) where
  featureDefaults1 flag =
    def
      { status = case flag of
          FeatureSSOEnabledByDefault -> FeatureStatusEnabled
          FeatureSSODisabledByDefault -> FeatureStatusDisabled
      }

data instance FeatureDefaults SearchVisibilityAvailableConfig
  = FeatureTeamSearchVisibilityAvailableByDefault
  | FeatureTeamSearchVisibilityUnavailableByDefault
  deriving stock (Eq, Ord, Show)

instance ParseFeatureDefaults (FeatureDefaults SearchVisibilityAvailableConfig) where
  parseFeatureDefaults obj = obj .: "teamSearchVisibility"

instance FromJSON (FeatureDefaults SearchVisibilityAvailableConfig) where
  parseJSON (String "enabled-by-default") = pure FeatureTeamSearchVisibilityAvailableByDefault
  parseJSON (String "disabled-by-default") = pure FeatureTeamSearchVisibilityUnavailableByDefault
  parseJSON bad = fail $ "FeatureSearchVisibility: " <> (UTF8.toString . toStrict . encode $ bad)

instance GetFeatureDefaults (FeatureDefaults SearchVisibilityAvailableConfig) where
  featureDefaults1 flag =
    def
      { status = case flag of
          FeatureTeamSearchVisibilityAvailableByDefault -> FeatureStatusEnabled
          FeatureTeamSearchVisibilityUnavailableByDefault -> FeatureStatusDisabled
      }

newtype instance FeatureDefaults SearchVisibilityInboundConfig
  = SearchVisibilityInboundDefaults (Feature SearchVisibilityInboundConfig)
  deriving stock (Eq, Show)
  deriving newtype (Default, GetFeatureDefaults)
  deriving (FromJSON) via Defaults (Feature SearchVisibilityInboundConfig)
  deriving (ParseFeatureDefaults) via OptionalField SearchVisibilityInboundConfig

newtype instance FeatureDefaults ValidateSAMLEmailsConfig
  = ValidateSAMLEmailsDefaults (Feature ValidateSAMLEmailsConfig)
  deriving stock (Eq, Show)
  deriving newtype (Default, GetFeatureDefaults)
  deriving (FromJSON) via Defaults (Feature ValidateSAMLEmailsConfig)
  deriving (ParseFeatureDefaults) via OptionalField ValidateSAMLEmailsConfig

data instance FeatureDefaults DigitalSignaturesConfig = DigitalSignaturesDefaults
  deriving stock (Eq, Show)
  deriving (GetFeatureDefaults) via FixedDefaults DigitalSignaturesConfig

instance ParseFeatureDefaults (FeatureDefaults DigitalSignaturesConfig) where
  parseFeatureDefaults _ = pure DigitalSignaturesDefaults

newtype instance FeatureDefaults AppLockConfig
  = AppLockDefaults (Feature AppLockConfig)
  deriving stock (Eq, Show)
  deriving newtype (Default, GetFeatureDefaults)
  deriving (FromJSON) via Defaults (Feature AppLockConfig)
  deriving (ParseFeatureDefaults) via OptionalField AppLockConfig

newtype instance FeatureDefaults FileSharingConfig
  = FileSharingDefaults (LockableFeature FileSharingConfig)
  deriving stock (Eq, Show)
  deriving newtype (Default, GetFeatureDefaults)
  deriving (FromJSON) via Defaults (LockableFeature FileSharingConfig)
  deriving (ParseFeatureDefaults) via OptionalField FileSharingConfig

newtype instance FeatureDefaults ClassifiedDomainsConfig
  = ClassifiedDomainsDefaults (Feature ClassifiedDomainsConfig)
  deriving stock (Eq, Show)
  deriving newtype (Default, FromJSON)
  deriving (ParseFeatureDefaults) via OptionalField ClassifiedDomainsConfig
  deriving (GetFeatureDefaults) via Feature ClassifiedDomainsConfig

newtype instance FeatureDefaults ConferenceCallingConfig
  = ConferenceCallingDefaults (LockableFeature ConferenceCallingConfig)
  deriving stock (Eq, Show)
  deriving newtype (Default, GetFeatureDefaults)
  deriving (FromJSON) via Defaults (LockableFeature ConferenceCallingConfig)
  deriving (ParseFeatureDefaults) via OptionalField ConferenceCallingConfig

newtype instance FeatureDefaults SelfDeletingMessagesConfig
  = SelfDeletingMessagesDefaults (LockableFeature SelfDeletingMessagesConfig)
  deriving stock (Eq, Show)
  deriving newtype (Default, GetFeatureDefaults)
  deriving (FromJSON) via Defaults (LockableFeature SelfDeletingMessagesConfig)
  deriving (ParseFeatureDefaults) via OptionalField SelfDeletingMessagesConfig

newtype instance FeatureDefaults GuestLinksConfig
  = GuestLinksDefaults (LockableFeature GuestLinksConfig)
  deriving stock (Eq, Show)
  deriving newtype (Default, GetFeatureDefaults)
  deriving (FromJSON) via Defaults (LockableFeature GuestLinksConfig)
  deriving (ParseFeatureDefaults) via OptionalField GuestLinksConfig

newtype instance FeatureDefaults SndFactorPasswordChallengeConfig
  = SndFactorPasswordChallengeDefaults (LockableFeature SndFactorPasswordChallengeConfig)
  deriving stock (Eq, Show)
  deriving newtype (Default, GetFeatureDefaults)
  deriving (FromJSON) via Defaults (LockableFeature SndFactorPasswordChallengeConfig)
  deriving (ParseFeatureDefaults) via OptionalField SndFactorPasswordChallengeConfig

newtype instance FeatureDefaults MLSConfig
  = MLSDefaults (LockableFeature MLSConfig)
  deriving stock (Eq, Show)
  deriving newtype (Default, GetFeatureDefaults)
  deriving (FromJSON) via Defaults (LockableFeature MLSConfig)
  deriving (ParseFeatureDefaults) via OptionalField MLSConfig

newtype instance FeatureDefaults ChannelsConfig
  = ChannelsDefaults (LockableFeature ChannelsConfig)
  deriving stock (Eq, Show)
  deriving newtype (Default, GetFeatureDefaults)
  deriving (FromJSON) via Defaults (LockableFeature ChannelsConfig)
  deriving (ParseFeatureDefaults) via OptionalField ChannelsConfig

data instance FeatureDefaults ExposeInvitationURLsToTeamAdminConfig
  = ExposeInvitationURLsToTeamAdminDefaults
  deriving stock (Eq, Show)
  deriving (GetFeatureDefaults) via FixedDefaults ExposeInvitationURLsToTeamAdminConfig

instance ParseFeatureDefaults (FeatureDefaults ExposeInvitationURLsToTeamAdminConfig) where
  parseFeatureDefaults _ = pure ExposeInvitationURLsToTeamAdminDefaults

newtype instance FeatureDefaults OutlookCalIntegrationConfig
  = OutlookCalIntegrationDefaults (LockableFeature OutlookCalIntegrationConfig)
  deriving stock (Eq, Show)
  deriving newtype (Default, GetFeatureDefaults)
  deriving (FromJSON) via Defaults (LockableFeature OutlookCalIntegrationConfig)
  deriving (ParseFeatureDefaults) via OptionalField OutlookCalIntegrationConfig

newtype instance FeatureDefaults MlsE2EIdConfig
  = MlsE2EIdDefaults (LockableFeature MlsE2EIdConfig)
  deriving stock (Eq, Show)
  deriving newtype (Default, GetFeatureDefaults)
  deriving (FromJSON) via Defaults (LockableFeature MlsE2EIdConfig)
  deriving (ParseFeatureDefaults) via OptionalField MlsE2EIdConfig

newtype instance FeatureDefaults MlsMigrationConfig
  = MlsMigrationDefaults (LockableFeature MlsMigrationConfig)
  deriving stock (Eq, Show)
  deriving newtype (Default, GetFeatureDefaults)
  deriving (FromJSON) via Defaults (LockableFeature MlsMigrationConfig)
  deriving (ParseFeatureDefaults) via OptionalField MlsMigrationConfig

newtype instance FeatureDefaults EnforceFileDownloadLocationConfig
  = EnforceFileDownloadLocationDefaults (LockableFeature EnforceFileDownloadLocationConfig)
  deriving stock (Eq, Show)
  deriving newtype (Default, GetFeatureDefaults)
  deriving (FromJSON) via Defaults (LockableFeature EnforceFileDownloadLocationConfig)
  deriving (ParseFeatureDefaults) via OptionalField EnforceFileDownloadLocationConfig

newtype instance FeatureDefaults LimitedEventFanoutConfig
  = LimitedEventFanoutDefaults (Feature LimitedEventFanoutConfig)
  deriving stock (Eq, Show)
  deriving newtype (Default, GetFeatureDefaults)
  deriving (FromJSON) via Defaults (Feature LimitedEventFanoutConfig)
  deriving (ParseFeatureDefaults) via OptionalField LimitedEventFanoutConfig

newtype instance FeatureDefaults DomainRegistrationConfig
  = DomainRegistrationConfigDefaults (LockableFeature DomainRegistrationConfig)
  deriving stock (Eq, Show)
  deriving newtype (Default, GetFeatureDefaults)
  deriving (FromJSON) via Defaults (LockableFeature DomainRegistrationConfig)
  deriving (ParseFeatureDefaults) via OptionalField DomainRegistrationConfig

newtype instance FeatureDefaults CellsConfig
  = CellsConfigDefaults (LockableFeature CellsConfig)
  deriving stock (Eq, Show)
  deriving newtype (Default, GetFeatureDefaults)
  deriving (FromJSON) via Defaults (LockableFeature CellsConfig)
  deriving (ParseFeatureDefaults) via OptionalField CellsConfig

newtype instance FeatureDefaults AllowedGlobalOperationsConfig
  = AllowedGlobalOperationsConfigDefaults (Feature AllowedGlobalOperationsConfig)
  deriving stock (Eq, Show)
  deriving newtype (Default, FromJSON)
  deriving (ParseFeatureDefaults) via OptionalField AllowedGlobalOperationsConfig
  deriving (GetFeatureDefaults) via Feature AllowedGlobalOperationsConfig

newtype instance FeatureDefaults AssetAuditLogConfig
  = AssetAuditLogDefaults (Feature AssetAuditLogConfig)
  deriving stock (Eq, Show)
  deriving newtype (Default, FromJSON)
  deriving (ParseFeatureDefaults) via OptionalField AssetAuditLogConfig
  deriving (GetFeatureDefaults) via Feature AssetAuditLogConfig

newtype instance FeatureDefaults ConsumableNotificationsConfig
  = ConsumableNotificationsDefaults (LockableFeature ConsumableNotificationsConfig)
  deriving stock (Eq, Show)
  deriving newtype (Default, GetFeatureDefaults)
  deriving (FromJSON) via Defaults (LockableFeature ConsumableNotificationsConfig)
  deriving (ParseFeatureDefaults) via OptionalField ConsumableNotificationsConfig

newtype instance FeatureDefaults ChatBubblesConfig
  = ChatBubblesDefaults (LockableFeature ChatBubblesConfig)
  deriving stock (Eq, Show)
  deriving newtype (Default, GetFeatureDefaults)
  deriving (FromJSON) via Defaults (LockableFeature ChatBubblesConfig)
  deriving (ParseFeatureDefaults) via OptionalField ChatBubblesConfig

newtype instance FeatureDefaults AppsConfig
  = AppsDefaults (LockableFeature AppsConfig)
  deriving stock (Eq, Show)
  deriving newtype (Default, GetFeatureDefaults)
  deriving (FromJSON) via Defaults (LockableFeature AppsConfig)
  deriving (ParseFeatureDefaults) via OptionalField AppsConfig

newtype instance FeatureDefaults SimplifiedUserConnectionRequestQRCodeConfig
  = SimplifiedUserConnectionRequestQRCodeDefaults (LockableFeature SimplifiedUserConnectionRequestQRCodeConfig)
  deriving stock (Eq, Show)
  deriving newtype (Default, GetFeatureDefaults)
  deriving (FromJSON) via Defaults (LockableFeature SimplifiedUserConnectionRequestQRCodeConfig)
  deriving (ParseFeatureDefaults) via OptionalField SimplifiedUserConnectionRequestQRCodeConfig

newtype instance FeatureDefaults StealthUsersConfig
  = StealthUsersDefaults (LockableFeature StealthUsersConfig)
  deriving stock (Eq, Show)
  deriving newtype (Default, GetFeatureDefaults)
  deriving (FromJSON) via Defaults (LockableFeature StealthUsersConfig)
  deriving (ParseFeatureDefaults) via OptionalField StealthUsersConfig

-- Collections of all features
type FeatureFlags = AllFeatures FeatureDefaults

-- Project a single feature's defaults out of the full set
featureDefaults ::
  forall cfg.
  ( GetFeatureDefaults (FeatureDefaults cfg),
    NpProject cfg Features
  ) =>
  FeatureFlags ->
  LockableFeature cfg
featureDefaults = featureDefaults1 . npProject

-- Parse a full FeatureFlags object from JSON using per-feature parsers
class FeatureFlagsFromObject f cfgs where
  featureFlagsFromObject :: Object -> A.Parser (NP f cfgs)

instance FeatureFlagsFromObject f '[] where
  featureFlagsFromObject _ = pure Nil

instance
  ( ParseFeatureDefaults (f cfg),
    FeatureFlagsFromObject f cfgs
  ) =>
  FeatureFlagsFromObject f (cfg : cfgs)
  where
  featureFlagsFromObject obj =
    (:*)
      <$> parseFeatureDefaults obj
      <*> featureFlagsFromObject obj

instance
  (FeatureFlagsFromObject FeatureDefaults Features) =>
  FromJSON FeatureFlags
  where
  parseJSON = withObject "FeatureFlags" featureFlagsFromObject
