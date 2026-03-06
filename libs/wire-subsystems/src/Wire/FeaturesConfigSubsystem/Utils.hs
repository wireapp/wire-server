{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Wire.FeaturesConfigSubsystem.Utils where

import Imports
import Polysemy
import Polysemy.Input
import Wire.API.Team.Feature
import Wire.API.Team.FeatureFlags

resolveServerFeature ::
  forall cfg r.
  ( GetFeatureDefaults (FeatureDefaults cfg),
    NpProject cfg Features,
    Member (Input FeatureFlags) r
  ) =>
  Sem r (LockableFeature cfg)
resolveServerFeature =
  inputs $ featureDefaults @cfg
