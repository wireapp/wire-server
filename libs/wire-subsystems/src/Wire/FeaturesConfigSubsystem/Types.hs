{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Wire.FeaturesConfigSubsystem.Types where

import Data.Default
import Data.Id (TeamId, UserId)
import Data.SOP.Sing (SListI)
import Galley.Types.Teams
import Imports
import Polysemy
import Polysemy.Input
import Wire.API.Team.Feature
import Wire.BrigAPIAccess (BrigAPIAccess, getAccountConferenceCallingConfigClient)
import Wire.FeaturesConfigSubsystem.Utils (resolveServerFeature)
import Wire.LegalHold
import Wire.LegalHoldStore

type GetFeatureConfigEffects r =
  ( Member (Input FeatureFlags) r,
    Member (Input ExposeInvitationURLsAllowlist) r,
    Member LegalHoldStore r,
    Member (Input (FeatureDefaults LegalholdConfig)) r,
    Member BrigAPIAccess r
  )

newtype ExposeInvitationURLsAllowlist
  = ExposeInvitationURLsAllowlist [TeamId]

-- | Don't export methods of this typeclass
class
  ( IsFeatureConfig cfg,
    GetFeatureDefaults (FeatureDefaults cfg),
    SListI Features
  ) =>
  GetFeatureConfig cfg
  where
  getFeatureForUser ::
    (GetFeatureConfigEffects r) =>
    UserId ->
    Sem r (LockableFeature cfg)
  default getFeatureForUser ::
    (GetFeatureConfigEffects r) =>
    UserId ->
    Sem r (LockableFeature cfg)
  getFeatureForUser _uid = resolveServerFeature
  computeFeature ::
    (GetFeatureConfigEffects r) =>
    TeamId ->
    LockableFeature cfg ->
    DbFeature cfg ->
    Sem r (LockableFeature cfg)
  default computeFeature ::
    TeamId ->
    LockableFeature cfg ->
    DbFeature cfg ->
    Sem r (LockableFeature cfg)
  computeFeature _tid defFeature dbFeature =
    pure $ resolveDbFeature defFeature dbFeature

class (GetFeatureConfig cfg, GetFeatureConfigEffects r) => GetAllFeaturesForServerConstraints r cfg

instance (GetFeatureConfig cfg, GetFeatureConfigEffects r) => GetAllFeaturesForServerConstraints r cfg

class (GetFeatureConfig cfg, GetFeatureConfigEffects r) => GetAllTeamFeaturesForUserConstraints r cfg

instance (GetFeatureConfig cfg, GetFeatureConfigEffects r) => GetAllTeamFeaturesForUserConstraints r cfg

instance GetFeatureConfig SSOConfig

instance GetFeatureConfig SearchVisibilityAvailableConfig

instance GetFeatureConfig ValidateSAMLEmailsConfig

instance GetFeatureConfig DigitalSignaturesConfig

instance GetFeatureConfig LegalholdConfig where
  computeFeature tid defFeature dbFeature =
    setLockableFeatureStatus defFeature <$> computeLegalHoldFeatureStatus tid dbFeature

instance GetFeatureConfig FileSharingConfig

instance GetFeatureConfig AppLockConfig

instance GetFeatureConfig ClassifiedDomainsConfig

instance GetFeatureConfig ConferenceCallingConfig where
  getFeatureForUser uid = do
    feat <- getAccountConferenceCallingConfigClient uid
    pure $ withLockStatus (def @(LockableFeature ConferenceCallingConfig)).lockStatus feat

  computeFeature _tid defFeature dbFeature =
    pure $
      let feat = applyDbFeature dbFeature $ setLockableFeatureStatus defFeature FeatureStatusEnabled
       in case feat.lockStatus of
            LockStatusLocked -> setLockableFeatureLockStatus defFeature LockStatusLocked
            LockStatusUnlocked -> feat

instance GetFeatureConfig SelfDeletingMessagesConfig

instance GetFeatureConfig GuestLinksConfig

instance GetFeatureConfig SndFactorPasswordChallengeConfig

instance GetFeatureConfig SearchVisibilityInboundConfig

instance GetFeatureConfig MLSConfig

instance GetFeatureConfig ChannelsConfig

instance GetFeatureConfig ExposeInvitationURLsToTeamAdminConfig where
  computeFeature tid defFeature dbFeature = do
    (ExposeInvitationURLsAllowlist allowList) <- inputs id
    let teamAllowed = tid `elem` allowList
        lockStatus = if teamAllowed then LockStatusUnlocked else LockStatusLocked
    pure $ resolveDbFeature defFeature (dbFeatureLockStatus lockStatus <> dbFeature)

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
