{-# LANGUAGE UndecidableSuperClasses #-}
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
  ( getFeature,
    getFeatureInternal,
    getFeatureMulti,
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
import Data.Default
import Data.Id
import Data.Kind
import Data.Qualified (Local, tUnqualified)
import Data.SOP
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
    DbFeature cfg ->
    Sem r (LockableFeature cfg)
  default computeFeature ::
    TeamId ->
    LockableFeature cfg ->
    DbFeature cfg ->
    Sem r (LockableFeature cfg)
  computeFeature _tid defFeature dbFeature =
    pure $
      genericComputeFeature @cfg defFeature dbFeature

getFeature ::
  forall cfg r.
  ( GetFeatureConfig cfg,
    ComputeFeatureConstraints cfg r,
    Member (Input Opts) r,
    Member TeamFeatureStore r,
    Member (ErrorS 'NotATeamMember) r,
    Member TeamStore r
  ) =>
  UserId ->
  TeamId ->
  Sem r (LockableFeature cfg)
getFeature uid tid = do
  void $ getTeamMember tid uid >>= noteS @'NotATeamMember
  getConfigForTeam @cfg tid

getFeatureInternal ::
  ( GetFeatureConfig cfg,
    ComputeFeatureConstraints cfg r,
    Member (Input Opts) r,
    Member (ErrorS 'TeamNotFound) r,
    Member TeamFeatureStore r,
    Member TeamStore r
  ) =>
  TeamId ->
  Sem r (LockableFeature cfg)
getFeatureInternal tid = do
  assertTeamExists tid
  getConfigForTeam tid

getFeatureMulti ::
  forall cfg r.
  ( GetFeatureConfig cfg,
    ComputeFeatureConstraints cfg r,
    Member (Input Opts) r,
    Member TeamFeatureStore r
  ) =>
  Multi.TeamFeatureNoConfigMultiRequest ->
  Sem r (Multi.TeamFeatureNoConfigMultiResponse cfg)
getFeatureMulti (Multi.TeamFeatureNoConfigMultiRequest tids) = do
  cfgs <- getConfigForMultiTeam @cfg tids
  let xs = uncurry toTeamStatus <$> cfgs
  pure $ Multi.TeamFeatureNoConfigMultiResponse xs

toTeamStatus :: TeamId -> LockableFeature cfg -> Multi.TeamStatus cfg
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

class (GetFeatureConfig cfg, ComputeFeatureConstraints cfg r) => GetAllFeatureConfigsForServerConstraints r cfg

instance (GetFeatureConfig cfg, ComputeFeatureConstraints cfg r) => GetAllFeatureConfigsForServerConstraints r cfg

getAllFeatureConfigsForServer ::
  forall r.
  (Member (Input Opts) r) =>
  Sem r AllFeatureConfigs
getAllFeatureConfigsForServer = hsequence' $ hcpure (Proxy @GetFeatureConfig) $ Comp getConfigForServer

getAllFeatureConfigs ::
  forall r.
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
  hsequence' $ hcliftA2 (Proxy @(GetAllFeatureConfigsForServerConstraints r)) compute defFeatures features
  where
    compute ::
      (ComputeFeatureConstraints p r, GetFeatureConfig p) =>
      LockableFeature p ->
      DbFeature p ->
      (Sem r :.: LockableFeature) p
    compute defFeature feat = Comp $ computeFeature tid defFeature feat

class (GetConfigForUserConstraints cfg r, GetFeatureConfig cfg, ComputeFeatureConstraints cfg r) => GetAllFeatureConfigsForUserConstraints r cfg

instance (GetConfigForUserConstraints cfg r, GetFeatureConfig cfg, ComputeFeatureConstraints cfg r) => GetAllFeatureConfigsForUserConstraints r cfg

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
  hsequence' $ hcpure (Proxy @(GetAllFeatureConfigsForUserConstraints r)) $ Comp $ getConfigForTeamUser uid mTid

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
  defFeature <- getConfigForServer
  computeFeature @cfg
    tid
    defFeature
    dbFeature

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
    feat <- computeFeature @cfg tid defFeature dbFeature
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

  computeFeature tid defFeature dbFeature = do
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

  computeFeature _tid defFeature dbFeature =
    pure $
      let feat = applyDbFeature dbFeature defFeature {status = FeatureStatusEnabled}
       in case feat.lockStatus of
            LockStatusLocked -> defFeature {lockStatus = LockStatusLocked}
            LockStatusUnlocked -> feat

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
  computeFeature tid defFeature dbFeature = do
    allowList <- input <&> view (settings . exposeInvitationURLsTeamAllowlist . to (fromMaybe []))
    let teamAllowed = tid `elem` allowList
        lockStatus = if teamAllowed then LockStatusUnlocked else LockStatusLocked
    pure $ genericComputeFeature defFeature (dbFeatureLockStatus lockStatus <> dbFeature)

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
    <$> getFeatureInternal @cfg tid
