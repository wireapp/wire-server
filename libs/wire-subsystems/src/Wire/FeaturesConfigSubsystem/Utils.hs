{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Wire.FeaturesConfigSubsystem.Utils where

import Galley.Types.Teams
import Imports
import Polysemy
import Polysemy.Input
import Wire.API.Team.Feature

resolveServerFeature ::
  forall cfg r.
  ( GetFeatureDefaults (FeatureDefaults cfg),
    NpProject cfg Features,
    Member (Input FeatureFlags) r
  ) =>
  Sem r (LockableFeature cfg)
resolveServerFeature =
  inputs $ featureDefaults @cfg
