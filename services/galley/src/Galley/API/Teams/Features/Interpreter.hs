{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Galley.API.Teams.Features.Interpreter where

import Control.Lens
import Data.Default
import Data.Id
import Data.Qualified (tUnqualified)
import Data.SOP
import Galley.API.LegalHold.Team (computeLegalHoldFeatureStatus)
import Galley.Effects
import Galley.Effects.TeamFeatureStore
import Galley.Options
import Galley.Types.Teams
import Imports
import Polysemy
import Polysemy.Input
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Team.Feature
import Wire.BrigAPIAccess (getAccountConferenceCallingConfigClient)
import Wire.FeaturesConfigCompute
import Wire.FeaturesConfigRead
import Wire.FeaturesConfigRead.Types
import Wire.TeamSubsystem (TeamSubsystem)
import Wire.TeamSubsystem qualified as TeamSubsystem

runFeaturesConfigCompute ::
  ( Member (Input Opts) r,
    Member LegalHoldStore r,
    Member (Input (FeatureDefaults LegalholdConfig)) r,
    Member BrigAPIAccess r
  ) =>
  Sem (FeaturesConfigCompute : r) a ->
  Sem r a
runFeaturesConfigCompute = interpret $ \case
  ResolveGenericDbFeature _tid defFeature dbFeature ->
    pure $ resolveDbFeature defFeature dbFeature
  ResolveServerFeature ->
    doResolveServerFeature
  ResolveLegalhold tid defFeature dbFeature ->
    setLockableFeatureStatus defFeature <$> computeLegalHoldFeatureStatus tid dbFeature
  ResolveConferenceCalling _tid defFeature dbFeature ->
    pure $
      let feat = applyDbFeature dbFeature defFeature {status = FeatureStatusEnabled}
       in case feat.lockStatus of
            LockStatusLocked -> setLockableFeatureLockStatus defFeature LockStatusLocked
            LockStatusUnlocked -> feat
  ResolveConferenceCallingUser uid -> do
    feat <- getAccountConferenceCallingConfigClient uid
    pure $ withLockStatus (def @(LockableFeature ConferenceCallingConfig)).lockStatus feat
  ResolveExposeInvitationURLsToTeamAdmin tid defFeature dbFeature -> do
    allowList <- inputs $ view (settings . exposeInvitationURLsTeamAllowlist . to (fromMaybe []))
    let teamAllowed = tid `elem` allowList
        lockStatus = if teamAllowed then LockStatusUnlocked else LockStatusLocked
    pure $ resolveDbFeature defFeature (dbFeatureLockStatus lockStatus <> dbFeature)

runFeaturesConfigRead ::
  forall r a.
  ( Member (Input Opts) r,
    Member TeamFeatureStore r,
    Member FeaturesConfigCompute r,
    Member TeamSubsystem r,
    Member (ErrorS 'NotATeamMember) r
  ) =>
  Sem (FeaturesConfigRead : r) a ->
  Sem r a
runFeaturesConfigRead = interpret $ \case
  GetFeature uid tid -> do
    void $ TeamSubsystem.internalGetTeamMember uid tid >>= noteS @'NotATeamMember
    doGetFeatureForTeam tid
  GetFeatureForTeam tid ->
    doGetFeatureForTeam tid
  GetFeatureForServer ->
    resolveServerFeature
  GetFeatureForTeamUser uid mTid ->
    doGetFeatureForTeamUser uid mTid
  GetAllTeamFeaturesForTeamMember luid tid -> do
    void $ TeamSubsystem.internalGetTeamMember (tUnqualified luid) tid >>= noteS @'NotATeamMember
    doGetAllTeamFeatures tid
  GetAllTeamFeaturesForTeam tid ->
    doGetAllTeamFeatures tid
  GetAllTeamFeaturesForServer ->
    doGetAllTeamFeaturesForServer

-- Internal helpers

doGetFeatureForTeam ::
  forall cfg r.
  ( GetFeatureConfig cfg,
    Member TeamFeatureStore r,
    Member (Input Opts) r,
    Member FeaturesConfigCompute r
  ) =>
  TeamId ->
  Sem r (LockableFeature cfg)
doGetFeatureForTeam tid = do
  dbFeature <- getDbFeature tid
  defFeature <- doResolveServerFeature
  computeFeature tid defFeature dbFeature

doResolveServerFeature ::
  forall cfg r.
  ( GetFeatureDefaults (FeatureDefaults cfg),
    NpProject cfg Features,
    Member (Input Opts) r
  ) =>
  Sem r (LockableFeature cfg)
doResolveServerFeature = inputs $ view (settings . featureFlags . to (featureDefaults @cfg))

doGetFeatureForTeamUser ::
  forall cfg r.
  ( GetFeatureConfig cfg,
    Member (Input Opts) r,
    Member TeamFeatureStore r,
    Member FeaturesConfigCompute r
  ) =>
  UserId ->
  Maybe TeamId ->
  Sem r (LockableFeature cfg)
doGetFeatureForTeamUser uid Nothing = getFeatureForUser uid
doGetFeatureForTeamUser _uid (Just tid) = doGetFeatureForTeam tid

doGetAllTeamFeatures ::
  forall r.
  ( Member (Input Opts) r,
    Member TeamFeatureStore r,
    Member FeaturesConfigCompute r
  ) =>
  TeamId ->
  Sem r AllTeamFeatures
doGetAllTeamFeatures tid = do
  features <- getAllDbFeatures tid
  defFeatures <- doGetAllTeamFeaturesForServer
  hsequence' $ hcliftA2 (Proxy @(GetAllFeaturesForServerConstraints r)) compute defFeatures features
  where
    compute :: forall p. (GetFeatureConfig p) => LockableFeature p -> DbFeature p -> (Sem r :.: LockableFeature) p
    compute defFeature feat = Comp $ computeFeature tid defFeature feat

doGetAllTeamFeaturesForServer :: forall r. (Member (Input Opts) r) => Sem r AllTeamFeatures
doGetAllTeamFeaturesForServer =
  hsequence' $
    hcpure (Proxy @GetFeatureConfig) $
      Comp doResolveServerFeature
