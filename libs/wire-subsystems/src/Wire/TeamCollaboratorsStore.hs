{-# LANGUAGE TemplateHaskell #-}

module Wire.TeamCollaboratorsStore where

import Data.Id
import Imports
import Polysemy

data CollaboratorPermission = CreateTeamConversation | ImplicitConnection

data TeamCollaboratorsStore m a where
  CreateTeamCollaborator :: UserId -> TeamId -> Set CollaboratorPermission -> TeamCollaboratorsStore m ()
  GetAllTeamCollaborators :: TeamId -> TeamCollaboratorsStore m [UserId]

makeSem ''TeamCollaboratorsStore
