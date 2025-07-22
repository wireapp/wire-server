{-# LANGUAGE DuplicateRecordFields #-}

module Wire.API.Team.Collaborator where

import Data.Aeson qualified as A
import Data.Id
import Data.OpenApi qualified as S
import Data.Schema
import Imports
import Wire.Arbitrary

data CollaboratorPermission = CreateTeamConversation | ImplicitConnection
  deriving (Eq, Show, Ord, Generic)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via (Schema CollaboratorPermission)
  deriving (Arbitrary) via GenericUniform CollaboratorPermission

instance ToSchema CollaboratorPermission where
  schema =
    enum @Text "CollaboratorPermission" $
      mconcat
        [ element "create_team_conversation" CreateTeamConversation,
          element "implicit_connection" ImplicitConnection
        ]

data TeamCollaboratorsError
  = InsufficientRights
  | AlreadyExists
  deriving (Eq, Show)

instance Exception TeamCollaboratorsError

data NewTeamCollaborator = NewTeamCollaborator
  { aUser :: UserId,
    aPermissions :: Set CollaboratorPermission
  }
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via (Schema NewTeamCollaborator)

instance ToSchema NewTeamCollaborator where
  schema =
    object "NewTeamCollaborator" $
      NewTeamCollaborator
        <$> (aUser .= field "user" schema)
        <*> (aPermissions .= field "permissions" (set schema))

data TeamCollaborator = TeamCollaborator
  { gUser :: UserId,
    gTeam :: TeamId,
    gPermissions :: Set CollaboratorPermission
  }
  deriving (Eq, Show)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via (Schema TeamCollaborator)

instance ToSchema TeamCollaborator where
  schema =
    object "TeamCollaborator" $
      TeamCollaborator
        <$> (gUser .= field "user" schema)
        <*> (gTeam .= field "team" schema)
        <*> (gPermissions .= field "permissions" (set schema))
