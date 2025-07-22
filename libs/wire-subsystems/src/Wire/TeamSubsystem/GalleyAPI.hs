module Wire.TeamSubsystem.GalleyAPI where

import Imports
import Polysemy
import Wire.GalleyAPIAccess (GalleyAPIAccess)
import Wire.GalleyAPIAccess qualified as GalleyAPIAccess
import Wire.TeamSubsystem

intepreterTeamSubsystemToGalleyAPI :: (Member GalleyAPIAccess r) => InterpreterFor TeamSubsystem r
intepreterTeamSubsystemToGalleyAPI = interpret $ \case
  InternalGetTeamMember userId teamId -> GalleyAPIAccess.getTeamMember userId teamId
  InternalGetTeamMembers teamId -> GalleyAPIAccess.getTeamMembers teamId
