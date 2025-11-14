{-# LANGUAGE DuplicateRecordFields #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

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
