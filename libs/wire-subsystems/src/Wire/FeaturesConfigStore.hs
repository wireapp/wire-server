{-# LANGUAGE TemplateHaskell #-}

module Wire.FeaturesConfigStore where

import Data.Id (TeamId, UserId)
import Data.Qualified (Local)
import Imports
import Polysemy
import Wire.API.Team.Feature (AllTeamFeatures, LockableFeature)
import Wire.FeaturesConfigStore.Types

data FeaturesConfigStore m a where
  GetFeature ::
    forall cfg m.
    (GetFeatureConfig cfg) =>
    UserId -> TeamId -> FeaturesConfigStore m (LockableFeature cfg)
  GetFeatureForTeam ::
    forall cfg m.
    (GetFeatureConfig cfg) =>
    TeamId -> FeaturesConfigStore m (LockableFeature cfg)
  GetFeatureForServer ::
    forall cfg m.
    (GetFeatureConfig cfg) =>
    FeaturesConfigStore m (LockableFeature cfg)
  GetFeatureForTeamUser ::
    forall cfg m.
    (GetFeatureConfig cfg) =>
    UserId -> Maybe TeamId -> FeaturesConfigStore m (LockableFeature cfg)
  GetAllTeamFeaturesForTeamMember :: Local UserId -> TeamId -> FeaturesConfigStore m AllTeamFeatures
  GetAllTeamFeaturesForTeam :: TeamId -> FeaturesConfigStore m AllTeamFeatures
  GetAllTeamFeaturesForServer :: FeaturesConfigStore m AllTeamFeatures

makeSem ''FeaturesConfigStore
