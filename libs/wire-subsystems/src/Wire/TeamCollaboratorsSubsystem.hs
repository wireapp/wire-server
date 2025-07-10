{-# LANGUAGE TemplateHaskell #-}

module Wire.TeamCollaboratorsSubsystem where

import Data.Id
import Data.Qualified
import Imports
import Polysemy
import Wire.API.Team.Collaborator

data TeamCollaboratorsSubsystem m a where
  CreateTeamCollaborator :: Local UserId -> UserId -> TeamId -> Set CollaboratorPermission -> TeamCollaboratorsSubsystem m ()
  -- TODO: Create a data type for the response containing: UserID, TeamID and permissions
  -- Only the UserId isn't enough
  GetAllTeamCollaborators :: Local UserId -> TeamId -> TeamCollaboratorsSubsystem m (Set UserId)

makeSem ''TeamCollaboratorsSubsystem
