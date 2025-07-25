{-# LANGUAGE TemplateHaskell #-}

module Wire.TeamCollaboratorsStore where

import Data.Id
import Imports
import Polysemy
import Wire.API.Team.Collaborator

data TeamCollaboratorsStore m a where
  CreateTeamCollaborator :: UserId -> TeamId -> Set CollaboratorPermission -> TeamCollaboratorsStore m ()
  GetAllTeamCollaborators :: TeamId -> TeamCollaboratorsStore m [TeamCollaborator]
  GetTeamCollaborator :: TeamId -> UserId -> TeamCollaboratorsStore m (Maybe TeamCollaborator)

makeSem ''TeamCollaboratorsStore
