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

import Control.Lens qualified as L
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
    (doc . description L.?~ descr) $
      enum @Text $
        mconcat
          [ element "create_team_conversation" CreateTeamConversation,
            element "implicit_connection" ImplicitConnection
          ]
    where
      descr =
        "<p>Permission granted to a team collaborator.</p>\
        \<ul><li>`create_team_conversation`: equivalent to the `CreateConversation` and \
        \`AddRemoveConvMember` permissions for team members (both implied in the `member` \
        \role); allows creating team group conversations and adding members to them.</li>\n\
        \<li>`implicit_connection`: team members are implicitly connected to each \
        \other, allowing conversations (1:1 or group) without an explicit connection \
        \request. This permission grants the same to a collaborator.</li></ul>\n\
        \<p>NB: a member of team A can always open conversations with a collaborator of \
        \team A; the permission only controls the collaborator's abilities.</p>"

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
    object $
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
    object $
      TeamCollaborator
        <$> (gUser .= field "user" schema)
        <*> (gTeam .= field "team" schema)
        <*> (gPermissions .= field "permissions" (set schema))
