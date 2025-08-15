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
import Wire.ConversationsSubsystem (ConversationsSubsystem, internalCloseConversationsFrom)
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
    Member NotificationSubsystem r,
    Member ConversationsSubsystem r
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
  ( Member Store.TeamCollaboratorsStore r,
    Member ConversationsSubsystem r
  ) =>
  UserId ->
  TeamId ->
  Sem r ()
internalRemoveTeamCollaboratorImpl user team = do
  Store.removeTeamCollaborator user team
  internalCloseConversationsFrom team user
