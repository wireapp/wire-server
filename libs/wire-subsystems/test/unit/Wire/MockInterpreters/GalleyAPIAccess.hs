module Wire.MockInterpreters.GalleyAPIAccess where

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
  _ -> error "uninterpreted effect: GalleyAPIAccess"
