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
  GetTeamCollaborations :: UserId -> TeamCollaboratorsStore m ([TeamCollaborator])
  GetTeamCollaboratorsWithIds :: Set TeamId -> Set UserId -> TeamCollaboratorsStore m [TeamCollaborator]
  UpdateTeamCollaborator :: UserId -> TeamId -> Set CollaboratorPermission -> TeamCollaboratorsStore m ()
  RemoveTeamCollaborator :: UserId -> TeamId -> TeamCollaboratorsStore m ()

makeSem ''TeamCollaboratorsStore
