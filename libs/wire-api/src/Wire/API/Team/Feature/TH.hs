{-# LANGUAGE TemplateHaskell #-}

module Wire.API.Team.Feature.TH where

import Data.Constraint
import Data.Singletons.Base.TH
import Wire.API.Team.Feature

featureSingIsFeature :: forall cfg. FeatureSingleton cfg -> Dict (IsFeatureConfig cfg)
featureSingIsFeature s = $(cases ''FeatureSingleton [|s|] [|Dict|])
