module Wire.API.Team.Collaborator where

import Data.Aeson qualified as A
import Data.Id
import Data.OpenApi qualified as S
import Data.Schema
import Imports

data CollaboratorPermission = CreateTeamConversation | ImplicitConnection
  deriving (Eq, Show, Ord)

data TeamCollaboratorsError = InsufficientRights

data AddTeamCollaborator = AddTeamCollaborator
  { user :: UserId,
    permissions :: [CollaboratorPermission]
  }
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via (Schema AddTeamCollaborator)

instance ToSchema AddTeamCollaborator where
  schema =
    object "AddTeamCollaborator" $
      AddTeamCollaborator
        <$> (user .= field "user" schema)
        <*> (permissions .= field "permissions" (array schema))

instance ToSchema CollaboratorPermission where
  schema =
    enum @Text "CollaboratorPermission" $
      mconcat
        [ element "create_team_conversation" CreateTeamConversation,
          element "implicit_connection" ImplicitConnection
        ]
