{-# LANGUAGE RecordWildCards #-}
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

module Galley.API.Teams.Features.Get
  ( getFeatureStatus,
    getFeatureStatusMulti,
    getAllFeatureConfigsForServer,
    getAllFeatureConfigsForTeam,
    getAllFeatureConfigsForUser,
    getSingleFeatureConfigForUser,
    GetFeatureConfig (..),
    getConfigForTeam,
    guardSecondFactorDisabled,
    DoAuth (..),
    featureEnabledForTeam,
    toTeamStatus,
  )
where

import Control.Error (hush)
import Control.Lens
import Data.Bifunctor (second)
import Data.Default
import Data.Id
import Data.Kind
import Data.Qualified (Local, tUnqualified)
import Data.Tagged
import Galley.API.LegalHold.Team
import Galley.API.Util
import Galley.Effects
import Galley.Effects.BrigAccess (getAccountConferenceCallingConfigClient)
import Galley.Effects.ConversationStore as ConversationStore
import Galley.Effects.TeamFeatureStore qualified as TeamFeatures
import Galley.Effects.TeamStore (getOneUserTeam, getTeamMember)
import Galley.Options
import Galley.Types.Teams
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Wire.API.Conversation (cnvmTeam)
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Routes.Internal.Galley.TeamFeatureNoConfigMulti qualified as Multi
import Wire.API.Team.Feature

data DoAuth = DoAuth UserId | DontDoAuth

type DefaultGetConfigForUserConstraints cfg r =
  ( Member (Input Opts) r,
    Member TeamFeatureStore r,
    ComputeFeatureConstraints cfg r
  )

-- | Don't export methods of this typeclass
class (IsFeatureConfig cfg) => GetFeatureConfig cfg where
  type GetConfigForUserConstraints cfg (r :: EffectRow) :: Constraint
  type
    GetConfigForUserConstraints cfg (r :: EffectRow) =
      DefaultGetConfigForUserConstraints cfg r

  type ComputeFeatureConstraints cfg (r :: EffectRow) :: Constraint
  type ComputeFeatureConstraints cfg r = ()

  getConfigForServer ::
    (Member (Input Opts) r) =>
    Sem r (LockableFeature cfg)
  -- only override if there is additional business logic for getting the feature config
  -- and/or if the feature flag is configured for the backend in 'FeatureFlags' for galley in 'Galley.Types.Teams'
  -- otherwise this will return the default config from wire-api
  default getConfigForServer :: Sem r (LockableFeature cfg)
  getConfigForServer = pure def

  getConfigForUser ::
    (GetConfigForUserConstraints cfg r) =>
    UserId ->
    Sem r (LockableFeature cfg)
  default getConfigForUser ::
    (DefaultGetConfigForUserConstraints cfg r) =>
    UserId ->
    Sem r (LockableFeature cfg)
  getConfigForUser _ = getConfigForServer

  computeFeature ::
    (ComputeFeatureConstraints cfg r) =>
    TeamId ->
    LockableFeature cfg ->
    Maybe LockStatus ->
    DbFeature cfg ->
    Sem r (LockableFeature cfg)
  default computeFeature ::
    TeamId ->
    LockableFeature cfg ->
    Maybe LockStatus ->
    DbFeature cfg ->
    Sem r (LockableFeature cfg)
  computeFeature _tid defFeature lockStatus dbFeature =
    pure $
      genericComputeFeature @cfg defFeature lockStatus dbFeature

getFeatureStatus ::
  forall cfg r.
  ( GetFeatureConfig cfg,
    ComputeFeatureConstraints cfg r,
    Member (Input Opts) r,
    Member TeamFeatureStore r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS 'TeamNotFound) r,
    Member TeamStore r
  ) =>
  DoAuth ->
  TeamId ->
  Sem r (LockableFeature cfg)
getFeatureStatus doauth tid = do
  case doauth of
    DoAuth uid ->
      getTeamMember tid uid >>= maybe (throwS @'NotATeamMember) (const $ pure ())
    DontDoAuth ->
      assertTeamExists tid
  getConfigForTeam @cfg tid

getFeatureStatusMulti ::
  forall cfg r.
  ( GetFeatureConfig cfg,
    ComputeFeatureConstraints cfg r,
    Member (Input Opts) r,
    Member TeamFeatureStore r
  ) =>
  Multi.TeamFeatureNoConfigMultiRequest ->
  Sem r (Multi.TeamFeatureNoConfigMultiResponse cfg)
getFeatureStatusMulti (Multi.TeamFeatureNoConfigMultiRequest tids) = do
  cfgs <- getConfigForMultiTeam @cfg tids
  let xs = uncurry toTeamStatus . second forgetLock <$> cfgs
  pure $ Multi.TeamFeatureNoConfigMultiResponse xs

toTeamStatus :: TeamId -> Feature cfg -> Multi.TeamStatus cfg
toTeamStatus tid feat = Multi.TeamStatus tid feat.status

getTeamAndCheckMembership ::
  ( Member TeamStore r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS 'TeamNotFound) r
  ) =>
  UserId ->
  Sem r (Maybe TeamId)
getTeamAndCheckMembership uid = do
  mTid <- getOneUserTeam uid
  for_ mTid $ \tid -> do
    zusrMembership <- getTeamMember tid uid
    void $ maybe (throwS @'NotATeamMember) pure zusrMembership
    assertTeamExists tid
  pure mTid

getAllFeatureConfigsForTeam ::
  forall r.
  ( Member (Input Opts) r,
    Member (ErrorS 'NotATeamMember) r,
    Member LegalHoldStore r,
    Member TeamFeatureStore r,
    Member TeamStore r
  ) =>
  Local UserId ->
  TeamId ->
  Sem r AllFeatureConfigs
getAllFeatureConfigsForTeam luid tid = do
  void $ getTeamMember tid (tUnqualified luid) >>= noteS @'NotATeamMember
  getAllFeatureConfigs tid

getAllFeatureConfigs ::
  ( Member (Input Opts) r,
    Member LegalHoldStore r,
    Member TeamFeatureStore r,
    Member TeamStore r
  ) =>
  TeamId ->
  Sem r AllFeatureConfigs
getAllFeatureConfigs tid = do
  features <- TeamFeatures.getAllFeatureConfigs tid
  defFeatures <- getAllFeatureConfigsForServer
  biTraverseAllFeatures (computeFeatureWithLock tid) defFeatures features

computeFeatureWithLock ::
  forall cfg r.
  (GetFeatureConfig cfg, ComputeFeatureConstraints cfg r) =>
  TeamId ->
  LockableFeature cfg ->
  DbFeatureWithLock cfg ->
  Sem r (LockableFeature cfg)
computeFeatureWithLock tid defFeature feat =
  computeFeature @cfg tid defFeature feat.lockStatus feat.feature

-- | One of a number of possible combinators. This is the only one we happen to need.
biTraverseAllFeatures ::
  ( Member (Input Opts) r,
    Member TeamStore r,
    Member LegalHoldStore r
  ) =>
  ( forall cfg.
    (GetFeatureConfig cfg, ComputeFeatureConstraints cfg r) =>
    f cfg ->
    g cfg ->
    Sem r (h cfg)
  ) ->
  (AllFeatures f -> AllFeatures g -> Sem r (AllFeatures h))
biTraverseAllFeatures phi features1 features2 = do
  afcLegalholdStatus <- phi (afcLegalholdStatus features1) (afcLegalholdStatus features2)
  afcSSOStatus <- phi (afcSSOStatus features1) (afcSSOStatus features2)
  afcTeamSearchVisibilityAvailable <- phi (afcTeamSearchVisibilityAvailable features1) (afcTeamSearchVisibilityAvailable features2)
  afcSearchVisibilityInboundConfig <- phi (afcSearchVisibilityInboundConfig features1) (afcSearchVisibilityInboundConfig features2)
  afcValidateSAMLEmails <- phi (afcValidateSAMLEmails features1) (afcValidateSAMLEmails features2)
  afcDigitalSignatures <- phi (afcDigitalSignatures features1) (afcDigitalSignatures features2)
  afcAppLock <- phi (afcAppLock features1) (afcAppLock features2)
  afcFileSharing <- phi (afcFileSharing features1) (afcFileSharing features2)
  afcClassifiedDomains <- phi (afcClassifiedDomains features1) (afcClassifiedDomains features2)
  afcConferenceCalling <- phi (afcConferenceCalling features1) (afcConferenceCalling features2)
  afcSelfDeletingMessages <- phi (afcSelfDeletingMessages features1) (afcSelfDeletingMessages features2)
  afcGuestLink <- phi (afcGuestLink features1) (afcGuestLink features2)
  afcSndFactorPasswordChallenge <- phi (afcSndFactorPasswordChallenge features1) (afcSndFactorPasswordChallenge features2)
  afcMLS <- phi (afcMLS features1) (afcMLS features2)
  afcExposeInvitationURLsToTeamAdmin <- phi (afcExposeInvitationURLsToTeamAdmin features1) (afcExposeInvitationURLsToTeamAdmin features2)
  afcOutlookCalIntegration <- phi (afcOutlookCalIntegration features1) (afcOutlookCalIntegration features2)
  afcMlsE2EId <- phi (afcMlsE2EId features1) (afcMlsE2EId features2)
  afcMlsMigration <- phi (afcMlsMigration features1) (afcMlsMigration features2)
  afcEnforceFileDownloadLocation <- phi (afcEnforceFileDownloadLocation features1) (afcEnforceFileDownloadLocation features2)
  afcLimitedEventFanout <- phi (afcLimitedEventFanout features1) (afcLimitedEventFanout features2)
  pure AllFeatures {..}

getAllFeatureConfigsForServer ::
  forall r.
  (Member (Input Opts) r) =>
  Sem r AllFeatureConfigs
getAllFeatureConfigsForServer =
  AllFeatures
    <$> getConfigForServer @LegalholdConfig
    <*> getConfigForServer @SSOConfig
    <*> getConfigForServer @SearchVisibilityAvailableConfig
    <*> getConfigForServer @SearchVisibilityInboundConfig
    <*> getConfigForServer @ValidateSAMLEmailsConfig
    <*> getConfigForServer @DigitalSignaturesConfig
    <*> getConfigForServer @AppLockConfig
    <*> getConfigForServer @FileSharingConfig
    <*> getConfigForServer @ClassifiedDomainsConfig
    <*> getConfigForServer @ConferenceCallingConfig
    <*> getConfigForServer @SelfDeletingMessagesConfig
    <*> getConfigForServer @GuestLinksConfig
    <*> getConfigForServer @SndFactorPasswordChallengeConfig
    <*> getConfigForServer @MLSConfig
    <*> getConfigForServer @ExposeInvitationURLsToTeamAdminConfig
    <*> getConfigForServer @OutlookCalIntegrationConfig
    <*> getConfigForServer @MlsE2EIdConfig
    <*> getConfigForServer @MlsMigrationConfig
    <*> getConfigForServer @EnforceFileDownloadLocationConfig
    <*> getConfigForServer @LimitedEventFanoutConfig

getAllFeatureConfigsForUser ::
  forall r.
  ( Member BrigAccess r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS 'TeamNotFound) r,
    Member (ErrorS OperationDenied) r,
    Member (Input Opts) r,
    Member LegalHoldStore r,
    Member TeamFeatureStore r,
    Member TeamStore r
  ) =>
  UserId ->
  Sem r AllFeatureConfigs
getAllFeatureConfigsForUser uid = do
  mTid <- getTeamAndCheckMembership uid
  AllFeatures
    <$> getConfigForTeamUser uid mTid
    <*> getConfigForTeamUser uid mTid
    <*> getConfigForTeamUser uid mTid
    <*> getConfigForTeamUser uid mTid
    <*> getConfigForTeamUser uid mTid
    <*> getConfigForTeamUser uid mTid
    <*> getConfigForTeamUser uid mTid
    <*> getConfigForTeamUser uid mTid
    <*> getConfigForTeamUser uid mTid
    <*> getConfigForTeamUser uid mTid
    <*> getConfigForTeamUser uid mTid
    <*> getConfigForTeamUser uid mTid
    <*> getConfigForTeamUser uid mTid
    <*> getConfigForTeamUser uid mTid
    <*> getConfigForTeamUser uid mTid
    <*> getConfigForTeamUser uid mTid
    <*> getConfigForTeamUser uid mTid
    <*> getConfigForTeamUser uid mTid
    <*> getConfigForTeamUser uid mTid
    <*> getConfigForTeamUser uid mTid

getSingleFeatureConfigForUser ::
  forall cfg r.
  ( GetFeatureConfig cfg,
    Member (Input Opts) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS 'TeamNotFound) r,
    Member TeamStore r,
    Member TeamFeatureStore r,
    GetConfigForUserConstraints cfg r,
    ComputeFeatureConstraints cfg r
  ) =>
  UserId ->
  Sem r (LockableFeature cfg)
getSingleFeatureConfigForUser uid = do
  mTid <- getTeamAndCheckMembership uid
  getConfigForTeamUser @cfg uid mTid

getConfigForTeam ::
  forall cfg r.
  ( GetFeatureConfig cfg,
    ComputeFeatureConstraints cfg r,
    Member (Input Opts) r,
    Member TeamFeatureStore r
  ) =>
  TeamId ->
  Sem r (LockableFeature cfg)
getConfigForTeam tid = do
  dbFeature <- TeamFeatures.getFeatureConfig (featureSingleton @cfg) tid
  lockStatus <- TeamFeatures.getFeatureLockStatus (featureSingleton @cfg) tid
  defFeature <- getConfigForServer
  computeFeature @cfg
    tid
    defFeature
    lockStatus
    dbFeature

-- Note: this function assumes the feature cannot be locked
getConfigForMultiTeam ::
  forall cfg r.
  ( GetFeatureConfig cfg,
    ComputeFeatureConstraints cfg r,
    Member TeamFeatureStore r,
    Member (Input Opts) r
  ) =>
  [TeamId] ->
  Sem r [(TeamId, LockableFeature cfg)]
getConfigForMultiTeam tids = do
  defFeature <- getConfigForServer
  features <- TeamFeatures.getFeatureConfigMulti (featureSingleton @cfg) tids
  for features $ \(tid, dbFeature) -> do
    feat <- computeFeature @cfg tid defFeature (Just LockStatusUnlocked) dbFeature
    pure (tid, feat)

getConfigForTeamUser ::
  forall cfg r.
  ( GetFeatureConfig cfg,
    GetConfigForUserConstraints cfg r,
    ComputeFeatureConstraints cfg r,
    Member (Input Opts) r,
    Member TeamFeatureStore r
  ) =>
  UserId ->
  Maybe TeamId ->
  Sem r (LockableFeature cfg)
getConfigForTeamUser uid Nothing = getConfigForUser uid
getConfigForTeamUser _ (Just tid) = getConfigForTeam @cfg tid

-------------------------------------------------------------------------------
-- GetFeatureConfig instances

instance GetFeatureConfig SSOConfig where
  getConfigForServer = do
    status <-
      inputs (view (settings . featureFlags . flagSSO)) <&> \case
        FeatureSSOEnabledByDefault -> FeatureStatusEnabled
        FeatureSSODisabledByDefault -> FeatureStatusDisabled
    pure $ def {status = status}

instance GetFeatureConfig SearchVisibilityAvailableConfig where
  getConfigForServer = do
    status <-
      inputs (view (settings . featureFlags . flagTeamSearchVisibility)) <&> \case
        FeatureTeamSearchVisibilityAvailableByDefault -> FeatureStatusEnabled
        FeatureTeamSearchVisibilityUnavailableByDefault -> FeatureStatusDisabled
    pure $ def {status = status}

instance GetFeatureConfig ValidateSAMLEmailsConfig where
  getConfigForServer =
    inputs (view (settings . featureFlags . flagsTeamFeatureValidateSAMLEmailsStatus . unDefaults . unImplicitLockStatus))

instance GetFeatureConfig DigitalSignaturesConfig

instance GetFeatureConfig LegalholdConfig where
  type
    GetConfigForUserConstraints LegalholdConfig (r :: EffectRow) =
      ( Member (Input Opts) r,
        Member TeamFeatureStore r,
        Member LegalHoldStore r,
        Member TeamStore r,
        Member (ErrorS OperationDenied) r,
        Member (ErrorS 'NotATeamMember) r,
        Member (ErrorS 'TeamNotFound) r
      )
  type
    ComputeFeatureConstraints LegalholdConfig r =
      (Member TeamStore r, Member LegalHoldStore r)

  computeFeature tid defFeature _lockStatus dbFeature = do
    status <- computeLegalHoldFeatureStatus tid dbFeature
    pure $ defFeature {status = status}

instance GetFeatureConfig FileSharingConfig where
  getConfigForServer =
    input <&> view (settings . featureFlags . flagFileSharing . unDefaults)

instance GetFeatureConfig AppLockConfig where
  getConfigForServer =
    input <&> view (settings . featureFlags . flagAppLockDefaults . unDefaults . unImplicitLockStatus)

instance GetFeatureConfig ClassifiedDomainsConfig where
  getConfigForServer =
    input <&> view (settings . featureFlags . flagClassifiedDomains . unImplicitLockStatus)

-- | Conference calling gets enabled automatically once unlocked. To achieve
-- that, the default feature status in the unlocked case is forced to be
-- "enabled" before the database data is applied.
--
-- Previously, we were assuming that this feature would be left as "unlocked",
-- and the clients were simply setting the status field. Now, the pre-existing
-- status field is reinterpreted as the lock status, which means that the
-- status will be NULL in many cases. The defaulting logic in 'computeFeature'
-- here makes sure that the status is aligned with the lock status in those
-- situations.
instance GetFeatureConfig ConferenceCallingConfig where
  type
    GetConfigForUserConstraints ConferenceCallingConfig r =
      ( Member (Input Opts) r,
        Member (ErrorS OperationDenied) r,
        Member (ErrorS 'NotATeamMember) r,
        Member (ErrorS 'TeamNotFound) r,
        Member TeamStore r,
        Member TeamFeatureStore r,
        Member BrigAccess r
      )

  getConfigForServer =
    input <&> view (settings . featureFlags . flagConferenceCalling . unDefaults)

  getConfigForUser uid = do
    feat <- getAccountConferenceCallingConfigClient uid
    pure $ withLockStatus (def @(LockableFeature ConferenceCallingConfig)).lockStatus feat

  computeFeature _tid defFeature lockStatus dbFeature =
    pure $ case fromMaybe defFeature.lockStatus lockStatus of
      LockStatusLocked -> defFeature {lockStatus = LockStatusLocked}
      LockStatusUnlocked ->
        withUnlocked $
          (applyDbFeature dbFeature)
            (forgetLock defFeature)
              { status = FeatureStatusEnabled
              }

instance GetFeatureConfig SelfDeletingMessagesConfig where
  getConfigForServer =
    input <&> view (settings . featureFlags . flagSelfDeletingMessages . unDefaults)

instance GetFeatureConfig GuestLinksConfig where
  getConfigForServer =
    input <&> view (settings . featureFlags . flagConversationGuestLinks . unDefaults)

instance GetFeatureConfig SndFactorPasswordChallengeConfig where
  getConfigForServer =
    input <&> view (settings . featureFlags . flagTeamFeatureSndFactorPasswordChallengeStatus . unDefaults)

instance GetFeatureConfig SearchVisibilityInboundConfig where
  getConfigForServer =
    input <&> view (settings . featureFlags . flagTeamFeatureSearchVisibilityInbound . unDefaults . unImplicitLockStatus)

instance GetFeatureConfig MLSConfig where
  getConfigForServer =
    input <&> view (settings . featureFlags . flagMLS . unDefaults)

instance GetFeatureConfig ExposeInvitationURLsToTeamAdminConfig where
  type
    ComputeFeatureConstraints ExposeInvitationURLsToTeamAdminConfig r =
      (Member (Input Opts) r)

  -- the lock status of this feature is calculated from the allow list, not the database
  computeFeature tid defFeature _lockStatus dbFeature = do
    allowList <- input <&> view (settings . exposeInvitationURLsTeamAllowlist . to (fromMaybe []))
    let teamAllowed = tid `elem` allowList
        lockStatus = if teamAllowed then LockStatusUnlocked else LockStatusLocked
    pure $ genericComputeFeature defFeature (Just lockStatus) dbFeature

instance GetFeatureConfig OutlookCalIntegrationConfig where
  getConfigForServer =
    input <&> view (settings . featureFlags . flagOutlookCalIntegration . unDefaults)

instance GetFeatureConfig MlsE2EIdConfig where
  getConfigForServer =
    input <&> view (settings . featureFlags . flagMlsE2EId . unDefaults)

instance GetFeatureConfig MlsMigrationConfig where
  getConfigForServer =
    input <&> view (settings . featureFlags . flagMlsMigration . unDefaults)

instance GetFeatureConfig EnforceFileDownloadLocationConfig where
  getConfigForServer =
    input <&> view (settings . featureFlags . flagEnforceFileDownloadLocation . unDefaults)

instance GetFeatureConfig LimitedEventFanoutConfig where
  getConfigForServer =
    input <&> view (settings . featureFlags . flagLimitedEventFanout . unDefaults . unImplicitLockStatus)

-- | If second factor auth is enabled, make sure that end-points that don't support it, but
-- should, are blocked completely.  (This is a workaround until we have 2FA for those
-- end-points as well.)
--
-- This function exists to resolve a cyclic dependency.
guardSecondFactorDisabled ::
  forall r.
  ( Member TeamFeatureStore r,
    Member (Input Opts) r,
    Member (ErrorS 'AccessDenied) r,
    Member TeamStore r,
    Member ConversationStore r
  ) =>
  UserId ->
  ConvId ->
  Sem r ()
guardSecondFactorDisabled uid cid = do
  mTid <- fmap hush . runError @() $ do
    convData <- ConversationStore.getConversationMetadata cid >>= note ()
    tid <- note () convData.cnvmTeam
    mapError (unTagged @'TeamNotFound @()) $ assertTeamExists tid
    pure tid

  tf <- getConfigForTeamUser @SndFactorPasswordChallengeConfig uid mTid
  case tf.status of
    FeatureStatusDisabled -> pure ()
    FeatureStatusEnabled -> throwS @'AccessDenied

featureEnabledForTeam ::
  forall cfg r.
  ( GetFeatureConfig cfg,
    Member (Input Opts) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS 'TeamNotFound) r,
    Member TeamStore r,
    Member TeamFeatureStore r,
    ComputeFeatureConstraints cfg r
  ) =>
  TeamId ->
  Sem r Bool
featureEnabledForTeam tid =
  (==) FeatureStatusEnabled
    . (.status)
    <$> getFeatureStatus @cfg DontDoAuth tid
