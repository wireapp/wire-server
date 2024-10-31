module Wire.MockInterpreters.GalleyAPIAccess where

import Data.Id
import Data.Proxy
import Imports
import Polysemy
import Wire.API.Team.Feature
import Wire.API.Team.Member
import Wire.GalleyAPIAccess

-- | interprets galley by statically returning the values passed
miniGalleyAPIAccess ::
  -- | what to return when calling GetTeamMember
  Maybe TeamMember ->
  -- | what to return when calling GetAllTeamFeaturesForUser
  AllTeamFeatures ->
  InterpreterFor GalleyAPIAccess r
miniGalleyAPIAccess member configs = interpret $ \case
  GetTeamMember _ _ -> pure member
  GetAllTeamFeaturesForUser _ -> pure configs
  GetFeatureConfigForTeam tid -> pure $ getFeatureConfigForTeamImpl configs tid
  _ -> error "uninterpreted effect: GalleyAPIAccess"

getFeatureConfigForTeamImpl :: forall feature. (IsFeatureConfig feature) => AllTeamFeatures -> TeamId -> LockableFeature feature
getFeatureConfigForTeamImpl allfeatures _ = npProject' (Proxy @(feature)) allfeatures
