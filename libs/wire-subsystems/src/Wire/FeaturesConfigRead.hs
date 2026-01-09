{-# LANGUAGE TemplateHaskell #-}

module Wire.FeaturesConfigRead where

import Data.Id (ConvId, TeamId, UserId)
import Data.Proxy
import Data.Qualified (Local)
import Imports
import Polysemy
import Wire.API.Team.Feature
import Wire.FeaturesConfigRead.Types

data FeaturesConfigRead m a where
  GetFeature ::
    forall cfg m.
    (GetFeatureConfig cfg) =>
    UserId -> TeamId -> FeaturesConfigRead m (LockableFeature cfg)
  GetFeatureInternal ::
    forall cfg m.
    (GetFeatureConfig cfg) =>
    TeamId -> FeaturesConfigRead m (LockableFeature cfg)
  GetFeatureForTeam ::
    forall cfg m.
    (GetFeatureConfig cfg) =>
    TeamId -> FeaturesConfigRead m (LockableFeature cfg)
  GetFeatureForServer ::
    forall cfg m.
    (GetFeatureConfig cfg) =>
    FeaturesConfigRead m (LockableFeature cfg)
  GetFeatureForTeamUser ::
    forall cfg m.
    (GetFeatureConfig cfg) =>
    UserId -> Maybe TeamId -> FeaturesConfigRead m (LockableFeature cfg)
  GetSingleFeatureForUser ::
    forall cfg m.
    (GetFeatureConfig cfg) =>
    UserId -> FeaturesConfigRead m (LockableFeature cfg)
  GetAllTeamFeaturesForTeamMember :: Local UserId -> TeamId -> FeaturesConfigRead m AllTeamFeatures
  GetAllTeamFeaturesForTeam :: TeamId -> FeaturesConfigRead m AllTeamFeatures
  GetAllTeamFeaturesForServer :: FeaturesConfigRead m AllTeamFeatures
  GetAllTeamFeaturesForUser :: UserId -> FeaturesConfigRead m AllTeamFeatures
  GuardSecondFactorDisabled :: UserId -> ConvId -> FeaturesConfigRead m ()
  FeatureEnabledForTeam ::
    forall cfg m.
    (GetFeatureConfig cfg) =>
    Proxy cfg -> TeamId -> FeaturesConfigRead m Bool

makeSem ''FeaturesConfigRead
