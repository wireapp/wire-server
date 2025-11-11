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

module Wire.TeamCollaboratorsSubsystem.Interpreter where

import Control.Monad.Trans.Maybe
import Data.Id
import Data.Qualified
import Data.Set qualified as Set
import Imports
import Polysemy
import Polysemy.Error
import Wire.API.Error
import Wire.API.Error.Brig qualified as E
import Wire.API.Event.Team
import Wire.API.Team.Collaborator
import Wire.API.Team.Member qualified as TeamMember
import Wire.Error
import Wire.NotificationSubsystem
import Wire.Sem.Now
import Wire.TeamCollaboratorsStore qualified as Store
import Wire.TeamCollaboratorsSubsystem
import Wire.TeamSubsystem
import Wire.TeamSubsystem.Util

interpretTeamCollaboratorsSubsystem ::
  ( Member TeamSubsystem r,
    Member (Error TeamCollaboratorsError) r,
    Member Store.TeamCollaboratorsStore r,
    Member Now r,
    Member NotificationSubsystem r
  ) =>
  InterpreterFor TeamCollaboratorsSubsystem r
interpretTeamCollaboratorsSubsystem = interpret $ \case
  CreateTeamCollaborator zUser user team perms -> createTeamCollaboratorImpl zUser user team perms
  GetAllTeamCollaborators zUser team -> getAllTeamCollaboratorsImpl zUser team
  InternalGetTeamCollaborator team user -> internalGetTeamCollaboratorImpl team user
  InternalGetTeamCollaborations userId -> internalGetTeamCollaborationsImpl userId
  InternalGetTeamCollaboratorsWithIds teams userIds -> internalGetTeamCollaboratorsWithIdsImpl teams userIds
  InternalRemoveTeamCollaborator user team -> internalRemoveTeamCollaboratorImpl user team

internalGetTeamCollaboratorImpl ::
  (Member Store.TeamCollaboratorsStore r) =>
  TeamId ->
  UserId ->
  Sem r (Maybe TeamCollaborator)
internalGetTeamCollaboratorImpl teamId userId = do
  Store.getTeamCollaborator teamId userId

internalGetTeamCollaborationsImpl ::
  (Member Store.TeamCollaboratorsStore r) =>
  UserId ->
  Sem r [TeamCollaborator]
internalGetTeamCollaborationsImpl userId = do
  Store.getTeamCollaborations userId

createTeamCollaboratorImpl ::
  ( Member TeamSubsystem r,
    Member (Error TeamCollaboratorsError) r,
    Member Store.TeamCollaboratorsStore r,
    Member Now r,
    Member NotificationSubsystem r
  ) =>
  Local UserId ->
  UserId ->
  TeamId ->
  Set CollaboratorPermission ->
  Sem r ()
createTeamCollaboratorImpl zUser user team perms = do
  guardPermission (tUnqualified zUser) team TeamMember.NewTeamCollaborator InsufficientRights
  Store.createTeamCollaborator user team perms

  -- TODO: Review the event's values
  generateTeamEvent (tUnqualified zUser) team (EdCollaboratorAdd user (Set.toList perms))

getAllTeamCollaboratorsImpl ::
  ( Member TeamSubsystem r,
    Member (Error TeamCollaboratorsError) r,
    Member Store.TeamCollaboratorsStore r
  ) =>
  Local UserId ->
  TeamId ->
  Sem r [TeamCollaborator]
getAllTeamCollaboratorsImpl zUser team = do
  guardPermission (tUnqualified zUser) team TeamMember.NewTeamCollaborator InsufficientRights
  Store.getAllTeamCollaborators team

internalGetTeamCollaboratorsWithIdsImpl ::
  ( Member Store.TeamCollaboratorsStore r
  ) =>
  Set TeamId ->
  Set UserId ->
  Sem r [TeamCollaborator]
internalGetTeamCollaboratorsWithIdsImpl = do
  Store.getTeamCollaboratorsWithIds

internalRemoveTeamCollaboratorImpl ::
  ( Member Store.TeamCollaboratorsStore r
  ) =>
  UserId ->
  TeamId ->
  Sem r ()
internalRemoveTeamCollaboratorImpl user team = do
  Store.removeTeamCollaborator user team

-- This is of general usefulness. However, we cannot move this to wire-api as
-- this would lead to a cyclic dependency.
guardPermission ::
  ( Member TeamSubsystem r,
    Member (Error ex) r,
    TeamMember.IsPerm TeamMember.TeamMember perm
  ) =>
  UserId ->
  TeamId ->
  perm ->
  ex ->
  Sem r ()
guardPermission user team perm ex = do
  res <-
    isJust <$> runMaybeT do
      member <- MaybeT $ internalGetTeamMember user team
      guard (member `TeamMember.hasPermission` perm)
  unless res $
    throw ex

teamCollaboratorsSubsystemErrorToHttpError :: TeamCollaboratorsError -> HttpError
teamCollaboratorsSubsystemErrorToHttpError =
  StdError . \case
    InsufficientRights -> errorToWai @E.InsufficientTeamPermissions
    AlreadyExists -> errorToWai @E.DuplicateEntry
