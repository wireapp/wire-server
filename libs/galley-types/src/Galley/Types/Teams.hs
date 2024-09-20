{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Galley.Types.Teams
  ( TeamCreationTime (..),
    tcTime,
    GetFeatureDefaults (..),
    FeatureDefaults (..),
    FeatureFlags,
    DefaultsInitial (..),
    initialFeature,
    featureDefaults,
    notTeamMember,
    findTeamMember,
    isTeamMember,
    isTeamOwner,
    canSeePermsOf,
  )
where

import Control.Lens (makeLenses, view)
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types qualified as A
import Data.ByteString (toStrict)
import Data.ByteString.UTF8 qualified as UTF8
import Data.Default
import Data.Id (UserId)
import Data.SOP
import Data.Schema qualified as S
import Data.Set qualified as Set
import Imports
import Wire.API.Team.Feature
import Wire.API.Team.Member
import Wire.API.Team.Permission

-- This is the cassandra timestamp of writetime(binding)
newtype TeamCreationTime = TeamCreationTime
  { _tcTime :: Int64
  }

-- | Used to extract the feature config type out of 'FeatureDefaults' or
-- related types.
type family ConfigOf a

type instance ConfigOf (FeatureDefaults cfg) = cfg

-- | Convert a feature default value to an actual 'LockableFeature'.
class GetFeatureDefaults a where
  featureDefaults1 :: a -> LockableFeature (ConfigOf a)

type instance ConfigOf (Feature cfg) = cfg

instance (IsFeatureConfig cfg) => GetFeatureDefaults (Feature cfg) where
  featureDefaults1 = withLockStatus (def @(LockableFeature cfg)).lockStatus

-- | Some features do not have a configured default value, so this takes it
-- wholly from the 'Default' instance.
newtype FixedDefaults cfg = FixedDefaults (FeatureDefaults cfg)

type instance ConfigOf (FixedDefaults cfg) = cfg

instance (IsFeatureConfig cfg) => GetFeatureDefaults (FixedDefaults cfg) where
  featureDefaults1 _ = def

type instance ConfigOf (LockableFeature cfg) = cfg

instance GetFeatureDefaults (LockableFeature cfg) where
  featureDefaults1 = id

data family FeatureDefaults cfg

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

-- | Default value for all teams that have not enabled or disabled this feature explicitly.
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

newtype instance FeatureDefaults MLSConfig = MLSDefaults (DefaultsInitial MLSConfig)
  deriving stock (Eq, Show)
  deriving newtype (Default, GetFeatureDefaults)
  deriving (FromJSON) via DefaultsInitial MLSConfig
  deriving (ParseFeatureDefaults) via OptionalField MLSConfig

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

featureKey :: forall cfg. (IsFeatureConfig cfg) => Key.Key
featureKey = Key.fromText $ featureName @cfg

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

type FeatureFlags = AllFeatures FeatureDefaults

featureDefaults ::
  forall cfg.
  ( GetFeatureDefaults (FeatureDefaults cfg),
    NpProject cfg Features
  ) =>
  FeatureFlags ->
  LockableFeature cfg
featureDefaults = featureDefaults1 . npProject

class FeatureFlagsFromObject f cfgs where
  featureFlagsFromObject :: A.Object -> A.Parser (NP f cfgs)

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

newtype Defaults a = Defaults {_unDefaults :: a}

instance (FromJSON a) => FromJSON (Defaults a) where
  parseJSON = withObject "default object" $ \ob ->
    Defaults <$> (ob .: "defaults")

data DefaultsInitial cfg = DefaultsInitial
  { defFeature :: LockableFeature cfg,
    initial :: cfg
  }
  deriving (Eq, Show)

instance (IsFeatureConfig cfg) => Default (DefaultsInitial cfg) where
  def = DefaultsInitial def def

type instance ConfigOf (DefaultsInitial cfg) = cfg

instance GetFeatureDefaults (DefaultsInitial cfg) where
  featureDefaults1 = defFeature

instance (IsFeatureConfig cfg) => FromJSON (DefaultsInitial cfg) where
  parseJSON = withObject "default with initial" $ \ob -> do
    feat <- ob .:? "defaults" .!= def
    mc <-
      fromMaybe feat.config
        <$> A.explicitParseFieldMaybe S.schemaParseJSON ob "initialConfig"
    pure $ DefaultsInitial feat mc

initialFeature :: DefaultsInitial cfg -> LockableFeature cfg
initialFeature d = d.defFeature {config = d.initial}

makeLenses ''TeamCreationTime

notTeamMember :: [UserId] -> [TeamMember] -> [UserId]
notTeamMember uids tmms =
  Set.toList $
    Set.fromList uids `Set.difference` Set.fromList (map (view userId) tmms)

isTeamMember :: (Foldable m) => UserId -> m TeamMember -> Bool
isTeamMember u = isJust . findTeamMember u

findTeamMember :: (Foldable m) => UserId -> m TeamMember -> Maybe TeamMember
findTeamMember u = find ((u ==) . view userId)

isTeamOwner :: TeamMemberOptPerms -> Bool
isTeamOwner tm = optionalPermissions tm == Just fullPermissions

canSeePermsOf :: TeamMember -> TeamMember -> Bool
canSeePermsOf seeer seeee =
  seeer `hasPermission` GetMemberPermissions || seeer == seeee
