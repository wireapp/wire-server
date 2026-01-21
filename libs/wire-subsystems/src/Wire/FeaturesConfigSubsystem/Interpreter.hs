{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Wire.FeaturesConfigSubsystem.Interpreter where

import Data.Id
import Data.Qualified (tUnqualified)
import Data.SOP
import Galley.Types.Teams
import Imports
import Polysemy
import Polysemy.Input
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Team.Feature
import Wire.FeaturesConfigSubsystem
import Wire.FeaturesConfigSubsystem.Types
import Wire.FeaturesConfigSubsystem.Utils
import Wire.TeamFeatureStore
import Wire.TeamSubsystem (TeamSubsystem)
import Wire.TeamSubsystem qualified as TeamSubsystem

runFeaturesConfigSubsystem ::
  forall r a.
  ( Member TeamFeatureStore r,
    Member TeamSubsystem r,
    Member (ErrorS 'NotATeamMember) r,
    GetFeatureConfigEffects r
  ) =>
  Sem (FeaturesConfigSubsystem : r) a ->
  Sem r a
runFeaturesConfigSubsystem = interpret $ \case
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
    GetFeatureConfigEffects r
  ) =>
  TeamId ->
  Sem r (LockableFeature cfg)
doGetFeatureForTeam tid = do
  dbFeature <- getDbFeature tid
  defFeature <- resolveServerFeature
  computeFeature tid defFeature dbFeature

doGetFeatureForTeamUser ::
  forall cfg r.
  ( GetFeatureConfig cfg,
    Member TeamFeatureStore r,
    GetFeatureConfigEffects r
  ) =>
  UserId ->
  Maybe TeamId ->
  Sem r (LockableFeature cfg)
doGetFeatureForTeamUser uid Nothing = getFeatureForUser uid
doGetFeatureForTeamUser _uid (Just tid) = doGetFeatureForTeam tid

doGetAllTeamFeatures ::
  forall r.
  ( Member TeamFeatureStore r,
    GetFeatureConfigEffects r
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

doGetAllTeamFeaturesForServer :: forall r. (Member (Input FeatureFlags) r) => Sem r AllTeamFeatures
doGetAllTeamFeaturesForServer =
  hsequence' $
    hcpure (Proxy @GetFeatureConfig) $
      Comp resolveServerFeature
