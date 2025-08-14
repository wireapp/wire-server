module Wire.TeamCollaboratorsSubsystem.Interpreter where

import Control.Lens
import Control.Monad.Trans.Maybe
import Data.Default
import Data.Id
import Data.Json.Util
import Data.Qualified
import Data.Set qualified as Set
import Imports
import Polysemy
import Polysemy.Error
import Wire.API.Error
import Wire.API.Error.Brig qualified as E
import Wire.API.Event.Team
import Wire.API.Push.V2 qualified as Push
import Wire.API.Team.Collaborator
import Wire.API.Team.Member qualified as TeamMember
import Wire.Error
import Wire.NotificationSubsystem
import Wire.Sem.Now
import Wire.TeamCollaboratorsStore qualified as Store
import Wire.TeamCollaboratorsSubsystem
import Wire.TeamSubsystem

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

  now <- get
  let event = newEvent team now (EdCollaboratorAdd user (Set.toList perms))
  teamMembersList <- internalGetTeamAdmins team
  let teamMembers :: [UserId] = view TeamMember.userId <$> (teamMembersList ^. TeamMember.teamMembers)
  -- TODO: Review the event's values
  pushNotifications
    [ def
        { origin = Just (tUnqualified zUser),
          json = toJSONObject $ event,
          recipients =
            ( \uid ->
                Recipient
                  { recipientUserId = uid,
                    recipientClients = Push.RecipientClientsAll
                  }
            )
              <$> teamMembers,
          transient = False
        }
    ]

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
