{-# LANGUAGE TemplateHaskell #-}

module Wire.TeamCollaboratorsSubsystem where

import Data.Id
import Data.Qualified
import Imports
import Polysemy
import Wire.API.Team.Collaborator

data TeamCollaboratorsSubsystem m a where
  CreateTeamCollaborator :: Local UserId -> UserId -> TeamId -> Set CollaboratorPermission -> TeamCollaboratorsSubsystem m ()
  GetAllTeamCollaborators :: Local UserId -> TeamId -> TeamCollaboratorsSubsystem m [TeamCollaborator]
  InternalGetTeamCollaborator :: TeamId -> UserId -> TeamCollaboratorsSubsystem m (Maybe TeamCollaborator)

makeSem ''TeamCollaboratorsSubsystem
