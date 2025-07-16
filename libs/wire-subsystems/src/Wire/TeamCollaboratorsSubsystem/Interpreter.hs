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
import Wire.GalleyAPIAccess
import Wire.NotificationSubsystem
import Wire.Sem.Now
import Wire.TeamCollaboratorsStore qualified as Store
import Wire.TeamCollaboratorsSubsystem

interpretTeamCollaboratorsSubsystem ::
  ( Member GalleyAPIAccess r,
    Member (Error TeamCollaboratorsError) r,
    Member Store.TeamCollaboratorsStore r,
    Member Now r,
    Member NotificationSubsystem r
  ) =>
  InterpreterFor TeamCollaboratorsSubsystem r
interpretTeamCollaboratorsSubsystem = interpret $ \case
  CreateTeamCollaborator zUser user team perms -> createTeamCollaboratorImpl zUser user team perms
  GetAllTeamCollaborators zUser team -> getAllTeamCollaboratorsImpl zUser team

createTeamCollaboratorImpl ::
  ( Member GalleyAPIAccess r,
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
  guardPermission (tUnqualified zUser) team TeamMember.AddTeamCollaborator InsufficientRights
  Store.createTeamCollaborator user team perms

  now <- get
  let event = newEvent team now (EdCollaboratorAdd user (Set.toList perms))
  teamMembersList <- getTeamMembers team
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
  ( Member GalleyAPIAccess r,
    Member (Error TeamCollaboratorsError) r,
    Member Store.TeamCollaboratorsStore r
  ) =>
  Local UserId ->
  TeamId ->
  Sem r [GetTeamCollaborator]
getAllTeamCollaboratorsImpl zUser team = do
  guardPermission (tUnqualified zUser) team TeamMember.AddTeamCollaborator InsufficientRights
  Store.getAllTeamCollaborators team

-- This is of general usefulness. However, we cannot move this to wire-api as
-- this would lead to a cyclic dependency.
guardPermission ::
  ( Member GalleyAPIAccess r,
    Member (Error ex) r,
    TeamMember.IsPerm perm
  ) =>
  UserId ->
  TeamId ->
  perm ->
  ex ->
  Sem r ()
guardPermission user team perm ex = do
  res <-
    isJust <$> runMaybeT do
      member <- MaybeT $ getTeamMember user team
      guard (member `TeamMember.hasPermission` perm)
  unless res $
    throw ex

teamCollaboratorsSubsystemErrorToHttpError :: TeamCollaboratorsError -> HttpError
teamCollaboratorsSubsystemErrorToHttpError =
  StdError . \case
    InsufficientRights -> errorToWai @E.InsufficientTeamPermissions
    AlreadyExists -> errorToWai @E.DuplicateEntry
