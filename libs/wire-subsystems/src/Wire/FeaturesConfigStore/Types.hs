-- import Data.SOP
-- import Wire.API.Team.Feature
--
-- type AllTeamFeatures = NP LockableFeature Features
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Wire.FeaturesConfigStore.Types where

import Data.Id (TeamId, UserId)
import Data.SOP.Sing (SListI)
import Galley.Types.Teams (FeatureDefaults, GetFeatureDefaults)
import Polysemy
import Wire.API.Team.Feature
import Wire.FeaturesConfigCompute

-- | Don't export methods of this typeclass
class
  ( IsFeatureConfig cfg,
    GetFeatureDefaults (FeatureDefaults cfg),
    SListI Features
  ) =>
  GetFeatureConfig cfg
  where
  getFeatureForUser ::
    (Member FeaturesConfigCompute r) =>
    UserId ->
    Sem r (LockableFeature cfg)
  default getFeatureForUser ::
    (Member FeaturesConfigCompute r) =>
    UserId ->
    Sem r (LockableFeature cfg)
  getFeatureForUser _uid = resolveServerFeature
  computeFeature ::
    (Member FeaturesConfigCompute r) =>
    TeamId ->
    LockableFeature cfg ->
    DbFeature cfg ->
    Sem r (LockableFeature cfg)
  default computeFeature ::
    (Member FeaturesConfigCompute r) =>
    TeamId ->
    LockableFeature cfg ->
    DbFeature cfg ->
    Sem r (LockableFeature cfg)
  computeFeature tid defFeature dbFeature =
    resolveGenericDbFeature tid defFeature dbFeature

class (GetFeatureConfig cfg, Member FeaturesConfigCompute r) => GetAllFeaturesForServerConstraints r cfg

instance (GetFeatureConfig cfg, Member FeaturesConfigCompute r) => GetAllFeaturesForServerConstraints r cfg

class (GetFeatureConfig cfg, Member FeaturesConfigCompute r) => GetAllTeamFeaturesForUserConstraints r cfg

instance (GetFeatureConfig cfg, Member FeaturesConfigCompute r) => GetAllTeamFeaturesForUserConstraints r cfg

instance GetFeatureConfig SSOConfig

instance GetFeatureConfig SearchVisibilityAvailableConfig

instance GetFeatureConfig ValidateSAMLEmailsConfig

instance GetFeatureConfig DigitalSignaturesConfig

instance GetFeatureConfig LegalholdConfig where
  computeFeature tid defFeature dbFeature =
    resolveLegalhold tid defFeature dbFeature

instance GetFeatureConfig FileSharingConfig

instance GetFeatureConfig AppLockConfig

instance GetFeatureConfig ClassifiedDomainsConfig

instance GetFeatureConfig ConferenceCallingConfig where
  getFeatureForUser uid =
    resolveConferenceCallingUser uid

  computeFeature tid defFeature dbFeature =
    resolveConferenceCalling tid defFeature dbFeature

instance GetFeatureConfig SelfDeletingMessagesConfig

instance GetFeatureConfig GuestLinksConfig

instance GetFeatureConfig SndFactorPasswordChallengeConfig

instance GetFeatureConfig SearchVisibilityInboundConfig

instance GetFeatureConfig MLSConfig

instance GetFeatureConfig ChannelsConfig

instance GetFeatureConfig ExposeInvitationURLsToTeamAdminConfig where
  computeFeature tid defFeature dbFeature =
    resolveExposeInvitationURLsToTeamAdmin tid defFeature dbFeature

instance GetFeatureConfig OutlookCalIntegrationConfig

instance GetFeatureConfig MlsE2EIdConfig

instance GetFeatureConfig MlsMigrationConfig

instance GetFeatureConfig EnforceFileDownloadLocationConfig

instance GetFeatureConfig LimitedEventFanoutConfig

instance GetFeatureConfig DomainRegistrationConfig

instance GetFeatureConfig CellsConfig

instance GetFeatureConfig CellsInternalConfig

instance GetFeatureConfig AllowedGlobalOperationsConfig

instance GetFeatureConfig AssetAuditLogConfig

instance GetFeatureConfig ConsumableNotificationsConfig

instance GetFeatureConfig ChatBubblesConfig

instance GetFeatureConfig AppsConfig

instance GetFeatureConfig SimplifiedUserConnectionRequestQRCodeConfig

instance GetFeatureConfig StealthUsersConfig

instance GetFeatureConfig MeetingsConfig

instance GetFeatureConfig MeetingsPremiumConfig
