module Wire.API.Team.Collaborator where

data CollaboratorPermission = CreateTeamConversation | ImplicitConnection

data TeamCollaboratorsError = InsufficientRights
