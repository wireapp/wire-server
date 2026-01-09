{-# LANGUAGE TemplateHaskell #-}

module Wire.FeaturesConfigRead where

import Data.Maybe (Maybe)

import Data.Id (TeamId, UserId)
import Polysemy
import Wire.API.Team.Feature
import Wire.FeaturesConfigRead.Types

data FeaturesConfigRead m a where
  GetFeatureForTeam :: (GetFeatureConfig cfg) => TeamId -> FeaturesConfigRead m (LockableFeature cfg)
  GetFeatureForServer :: (GetFeatureConfig cfg) => FeaturesConfigRead m (LockableFeature cfg)
  GetFeatureForUser :: (GetFeatureConfig cfg) => UserId -> FeaturesConfigRead m (LockableFeature cfg)
  GetFeatureForTeamUser :: (GetFeatureConfig cfg) => UserId -> Maybe TeamId -> FeaturesConfigRead m (LockableFeature cfg)
  GetSingleFeatureForUser :: (GetFeatureConfig cfg) => UserId -> FeaturesConfigRead m (LockableFeature cfg)
  GetAllTeamFeaturesForTeam :: TeamId -> FeaturesConfigRead m AllTeamFeatures
  GetAllTeamFeaturesForServer :: FeaturesConfigRead m AllTeamFeatures
  GetAllTeamFeaturesForUser :: UserId -> FeaturesConfigRead m AllTeamFeatures

makeSem ''FeaturesConfigRead
