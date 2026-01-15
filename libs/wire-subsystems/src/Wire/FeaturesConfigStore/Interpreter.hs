{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Wire.FeaturesConfigStore.Interpreter where

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
import Wire.FeaturesConfigCompute
import Wire.FeaturesConfigCompute.Interpreter
import Wire.FeaturesConfigStore
import Wire.FeaturesConfigStore.Types
import Wire.TeamFeatureStore
import Wire.TeamSubsystem (TeamSubsystem)
import Wire.TeamSubsystem qualified as TeamSubsystem

runFeaturesConfigStore ::
  forall r a.
  ( Member (Input FeatureFlags) r,
    Member TeamFeatureStore r,
    Member FeaturesConfigCompute r,
    Member TeamSubsystem r,
    Member (ErrorS 'NotATeamMember) r
  ) =>
  Sem (FeaturesConfigStore : r) a ->
  Sem r a
runFeaturesConfigStore = interpret $ \case
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
    Member (Input FeatureFlags) r,
    Member FeaturesConfigCompute r
  ) =>
  TeamId ->
  Sem r (LockableFeature cfg)
doGetFeatureForTeam tid = do
  dbFeature <- getDbFeature tid
  defFeature <- doResolveServerFeature
  computeFeature tid defFeature dbFeature

doGetFeatureForTeamUser ::
  forall cfg r.
  ( GetFeatureConfig cfg,
    Member (Input FeatureFlags) r,
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
  ( Member (Input FeatureFlags) r,
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

doGetAllTeamFeaturesForServer :: forall r. (Member (Input FeatureFlags) r) => Sem r AllTeamFeatures
doGetAllTeamFeaturesForServer =
  hsequence' $
    hcpure (Proxy @GetFeatureConfig) $
      Comp doResolveServerFeature
