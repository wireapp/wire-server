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
    getAllTeamFeaturesForServer,
    getAllTeamFeaturesForTeam,
    getAllTeamFeaturesForUser,
    getSingleFeatureForUser,
    GetFeatureConfig (..),
    getFeatureForTeam,
    getFeatureForServer,
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
import Data.Text.Lazy qualified as LT
import Galley.API.Error
import Galley.API.LegalHold.Team
import Galley.API.Util
import Galley.Effects
import Galley.Effects.BrigAccess (getAccountConferenceCallingConfigClient)
import Galley.Effects.ConversationStore as ConversationStore
import Galley.Effects.TeamFeatureStore
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

type DefaultGetFeatureForUserConstraints cfg r =
  ( Member (Input Opts) r,
    Member TeamFeatureStore r,
    ComputeFeatureConstraints cfg r
  )

-- | Don't export methods of this typeclass
class
  ( IsFeatureConfig cfg,
    ParseDbFeature cfg,
    GetFeatureDefaults (FeatureDefaults cfg),
    NpProject cfg Features
  ) =>
  GetFeatureConfig cfg
  where
  type GetFeatureForUserConstraints cfg (r :: EffectRow) :: Constraint
  type
    GetFeatureForUserConstraints cfg (r :: EffectRow) =
      DefaultGetFeatureForUserConstraints cfg r

  type ComputeFeatureConstraints cfg (r :: EffectRow) :: Constraint
  type ComputeFeatureConstraints cfg r = (Member (Error InternalError) r)

  getFeatureForUser ::
    (GetFeatureForUserConstraints cfg r) =>
    UserId ->
    Sem r (LockableFeature cfg)
  default getFeatureForUser ::
    (DefaultGetFeatureForUserConstraints cfg r) =>
    UserId ->
    Sem r (LockableFeature cfg)
  getFeatureForUser _ = getFeatureForServer

  computeFeature ::
    (ComputeFeatureConstraints cfg r) =>
    TeamId ->
    LockableFeature cfg ->
    Tagged cfg DbFeature ->
    Sem r (LockableFeature cfg)
  default computeFeature ::
    (Member (Error InternalError) r) =>
    TeamId ->
    LockableFeature cfg ->
    Tagged cfg DbFeature ->
    Sem r (LockableFeature cfg)
  computeFeature _tid defFeature dbFeature =
    runFeatureParser $
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
  getFeatureForTeam @cfg tid

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
  getFeatureForTeam tid

toTeamStatus :: TeamId -> LockableFeature cfg -> Multi.TeamStatus cfg
toTeamStatus tid feat = Multi.TeamStatus tid feat.status

runFeatureParser ::
  forall r a.
  (Member (Error InternalError) r) =>
  Either Text a ->
  Sem r a
runFeatureParser =
  mapError (InternalErrorWithDescription . LT.fromStrict)
    . fromEither

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

getAllTeamFeaturesForTeam ::
  forall r.
  ( Member (Input Opts) r,
    Member (Error InternalError) r,
    Member (ErrorS 'NotATeamMember) r,
    Member LegalHoldStore r,
    Member TeamFeatureStore r,
    Member TeamStore r
  ) =>
  Local UserId ->
  TeamId ->
  Sem r AllTeamFeatures
getAllTeamFeaturesForTeam luid tid = do
  void $ getTeamMember tid (tUnqualified luid) >>= noteS @'NotATeamMember
  getAllTeamFeatures tid

class
  (GetFeatureConfig cfg, ComputeFeatureConstraints cfg r) =>
  GetAllFeaturesForServerConstraints r cfg

instance
  (GetFeatureConfig cfg, ComputeFeatureConstraints cfg r) =>
  GetAllFeaturesForServerConstraints r cfg

getAllTeamFeaturesForServer ::
  forall r.
  (Member (Input Opts) r) =>
  Sem r AllTeamFeatures
getAllTeamFeaturesForServer =
  hsequence' $
    hcpure (Proxy @GetFeatureConfig) $
      Comp getFeatureForServer

getAllTeamFeatures ::
  forall r.
  ( Member (Error InternalError) r,
    Member (Input Opts) r,
    Member LegalHoldStore r,
    Member TeamFeatureStore r,
    Member TeamStore r
  ) =>
  TeamId ->
  Sem r AllTeamFeatures
getAllTeamFeatures tid = do
  features <- getAllDbFeatures tid
  defFeatures <- getAllTeamFeaturesForServer
  hsequence' $ hcliftA2 (Proxy @(GetAllFeaturesForServerConstraints r)) compute defFeatures features
  where
    compute ::
      (ComputeFeatureConstraints p r, GetFeatureConfig p) =>
      LockableFeature p ->
      K DbFeature p ->
      (Sem r :.: LockableFeature) p
    compute defFeature (K feat) = Comp $ computeFeature tid defFeature (Tagged feat)

class (GetFeatureForUserConstraints cfg r, GetFeatureConfig cfg, ComputeFeatureConstraints cfg r) => GetAllTeamFeaturesForUserConstraints r cfg

instance (GetFeatureForUserConstraints cfg r, GetFeatureConfig cfg, ComputeFeatureConstraints cfg r) => GetAllTeamFeaturesForUserConstraints r cfg

getAllTeamFeaturesForUser ::
  forall r.
  ( Member BrigAccess r,
    Member (Error InternalError) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS 'TeamNotFound) r,
    Member (ErrorS OperationDenied) r,
    Member (Input Opts) r,
    Member LegalHoldStore r,
    Member TeamFeatureStore r,
    Member TeamStore r
  ) =>
  UserId ->
  Sem r AllTeamFeatures
getAllTeamFeaturesForUser uid = do
  mTid <- getTeamAndCheckMembership uid
  case mTid of
    Nothing -> hsequence' $ hcpure (Proxy @(GetAllTeamFeaturesForUserConstraints r)) $ Comp $ getFeatureForUser uid
    Just tid -> getAllTeamFeatures tid

getSingleFeatureForUser ::
  forall cfg r.
  ( GetFeatureConfig cfg,
    Member (Input Opts) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS 'TeamNotFound) r,
    Member TeamStore r,
    Member TeamFeatureStore r,
    GetFeatureForUserConstraints cfg r,
    ComputeFeatureConstraints cfg r
  ) =>
  UserId ->
  Sem r (LockableFeature cfg)
getSingleFeatureForUser uid = do
  mTid <- getTeamAndCheckMembership uid
  getFeatureForTeamUser @cfg uid mTid

getFeatureForTeam ::
  forall cfg r.
  ( GetFeatureConfig cfg,
    ComputeFeatureConstraints cfg r,
    Member (Input Opts) r,
    Member TeamFeatureStore r
  ) =>
  TeamId ->
  Sem r (LockableFeature cfg)
getFeatureForTeam tid = do
  dbFeature <- getDbFeature tid
  defFeature <- getFeatureForServer
  computeFeature @cfg
    tid
    defFeature
    dbFeature

getFeatureForTeamUser ::
  forall cfg r.
  ( GetFeatureConfig cfg,
    GetFeatureForUserConstraints cfg r,
    ComputeFeatureConstraints cfg r,
    Member (Input Opts) r,
    Member TeamFeatureStore r
  ) =>
  UserId ->
  Maybe TeamId ->
  Sem r (LockableFeature cfg)
getFeatureForTeamUser uid Nothing = getFeatureForUser uid
getFeatureForTeamUser _ (Just tid) = getFeatureForTeam @cfg tid

getFeatureForServer ::
  forall cfg r.
  ( GetFeatureDefaults (FeatureDefaults cfg),
    NpProject cfg Features,
    Member (Input Opts) r
  ) =>
  Sem r (LockableFeature cfg)
getFeatureForServer = inputs $ view (settings . featureFlags . to (featureDefaults @cfg))

-------------------------------------------------------------------------------
-- GetFeatureConfig instances

instance GetFeatureConfig SSOConfig

instance GetFeatureConfig SearchVisibilityAvailableConfig

instance GetFeatureConfig ValidateSAMLEmailsConfig

instance GetFeatureConfig DigitalSignaturesConfig

instance GetFeatureConfig LegalholdConfig where
  type
    GetFeatureForUserConstraints LegalholdConfig (r :: EffectRow) =
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

instance GetFeatureConfig FileSharingConfig

instance GetFeatureConfig AppLockConfig

instance GetFeatureConfig ClassifiedDomainsConfig

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
    GetFeatureForUserConstraints ConferenceCallingConfig r =
      ( Member (Input Opts) r,
        Member (ErrorS OperationDenied) r,
        Member (ErrorS 'NotATeamMember) r,
        Member (ErrorS 'TeamNotFound) r,
        Member TeamStore r,
        Member TeamFeatureStore r,
        Member BrigAccess r
      )

  getFeatureForUser uid = do
    feat <- getAccountConferenceCallingConfigClient uid
    pure $ withLockStatus (def @(LockableFeature ConferenceCallingConfig)).lockStatus feat

  computeFeature _tid defFeature dbFeature =
    runFeatureParser $ do
      feat <- genericComputeFeature defFeature {status = FeatureStatusEnabled} dbFeature
      pure $ case feat.lockStatus of
        LockStatusLocked -> defFeature {lockStatus = LockStatusLocked}
        LockStatusUnlocked -> feat

instance GetFeatureConfig SelfDeletingMessagesConfig

instance GetFeatureConfig GuestLinksConfig

instance GetFeatureConfig SndFactorPasswordChallengeConfig

instance GetFeatureConfig SearchVisibilityInboundConfig

instance GetFeatureConfig MLSConfig

instance GetFeatureConfig ExposeInvitationURLsToTeamAdminConfig where
  type
    ComputeFeatureConstraints ExposeInvitationURLsToTeamAdminConfig r =
      (Member (Error InternalError) r, Member (Input Opts) r)

  -- the lock status of this feature is calculated from the allow list, not the database
  computeFeature tid defFeature dbFeature = do
    allowList <- input <&> view (settings . exposeInvitationURLsTeamAllowlist . to (fromMaybe []))
    let teamAllowed = tid `elem` allowList
        lockStatus = if teamAllowed then LockStatusUnlocked else LockStatusLocked
    runFeatureParser $
      genericComputeFeature
        defFeature
        (fmap (\f -> f {lockStatus = Just lockStatus} :: DbFeature) dbFeature)

instance GetFeatureConfig OutlookCalIntegrationConfig

instance GetFeatureConfig MlsE2EIdConfig

instance GetFeatureConfig MlsMigrationConfig

instance GetFeatureConfig EnforceFileDownloadLocationConfig

instance GetFeatureConfig LimitedEventFanoutConfig

instance GetFeatureConfig DomainRegistrationConfig

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
    Member (Error InternalError) r,
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

  tf <- getFeatureForTeamUser @SndFactorPasswordChallengeConfig uid mTid
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
