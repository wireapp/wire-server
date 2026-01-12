{-# LANGUAGE TemplateHaskell #-}

module Wire.FeaturesConfigRead where

import Data.Id (TeamId, UserId)
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
  GetAllTeamFeaturesForTeamMember :: Local UserId -> TeamId -> FeaturesConfigRead m AllTeamFeatures
  GetAllTeamFeaturesForTeam :: TeamId -> FeaturesConfigRead m AllTeamFeatures
  GetAllTeamFeaturesForServer :: FeaturesConfigRead m AllTeamFeatures

makeSem ''FeaturesConfigRead
