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

data TeamCollaboratorsError = InsufficientRights
  deriving (Eq, Show)

instance Exception TeamCollaboratorsError

data AddTeamCollaborator = AddTeamCollaborator
  { aUser :: UserId,
    aPermissions :: [CollaboratorPermission]
  }
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via (Schema AddTeamCollaborator)

instance ToSchema AddTeamCollaborator where
  schema =
    object "AddTeamCollaborator" $
      AddTeamCollaborator
        <$> (aUser .= field "user" schema)
        <*> (aPermissions .= field "permissions" (array schema))

data GetTeamCollaborator = GetTeamCollaborator
  { gUser :: UserId,
    gTeam :: TeamId,
    gPermissions :: [CollaboratorPermission]
  }
  deriving (Eq, Show)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via (Schema GetTeamCollaborator)

instance ToSchema GetTeamCollaborator where
  schema =
    object "GetTeamCollaborator" $
      GetTeamCollaborator
        <$> (gUser .= field "user" schema)
        <*> (gTeam .= field "team" schema)
        <*> (gPermissions .= field "permissions" (array schema))
