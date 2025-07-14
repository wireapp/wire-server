module Wire.TeamCollaboratorsSubsystem.Interpreter where

import Control.Lens
import Control.Monad.Trans.Maybe
import Data.Default
import Data.Id
import Data.Json.Util
import Data.Qualified
import Data.Time
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Wire.API.Error
import Wire.API.Error.Brig qualified as E
import Wire.API.Event.Team
import Wire.API.Push.V2 qualified as Push
import Wire.API.Team.Collaborator
import Wire.API.Team.Member qualified as Team
import Wire.API.Team.Member qualified as TeamMember
import Wire.Error
import Wire.GalleyAPIAccess
import Wire.NotificationSubsystem
import Wire.TeamCollaboratorsStore qualified as Store
import Wire.TeamCollaboratorsSubsystem

runTeamCollaboratorsSubsystem ::
  ( Member GalleyAPIAccess r,
    Member (Error TeamCollaboratorsError) r,
    Member Store.TeamCollaboratorsStore r,
    Member (Input UTCTime) r,
    Member NotificationSubsystem r
  ) =>
  InterpreterFor TeamCollaboratorsSubsystem r
runTeamCollaboratorsSubsystem = interpret $ \case
  CreateTeamCollaborator zUser user team perms -> createTeamCollaboratorImpl zUser user team perms
  GetAllTeamCollaborators zUser team -> getAllTeamCollaboratorsImpl zUser team

createTeamCollaboratorImpl ::
  ( Member GalleyAPIAccess r,
    Member (Error TeamCollaboratorsError) r,
    Member Store.TeamCollaboratorsStore r,
    Member (Input UTCTime) r,
    Member NotificationSubsystem r
  ) =>
  Local UserId ->
  UserId ->
  TeamId ->
  Set CollaboratorPermission ->
  Sem r ()
createTeamCollaboratorImpl zUser user team perms = do
  unlessM (isTeamAdmin (tUnqualified zUser) team) $
    throw InsufficientRights
  Store.createTeamCollaborator user team perms

  now <- input
  let event = newEvent team now (EdCollaboratorAdd user)
  teamMembersList <- getTeamMembers team
  let teamMembers :: [UserId] = view Team.userId <$> (teamMembersList ^. Team.teamMembers)
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
          transient = True
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
  unlessM (isTeamAdmin (tUnqualified zUser) team) $
    throw InsufficientRights
  Store.getAllTeamCollaborators team

isTeamAdmin ::
  (Member GalleyAPIAccess r) =>
  UserId ->
  TeamId ->
  Sem r Bool
isTeamAdmin user team =
  isJust <$> runMaybeT do
    member <- MaybeT $ getTeamMember user team
    guard (TeamMember.isAdminOrOwner (member ^. TeamMember.permissions))

teamCollaboratorsSubsystemErrorToHttpError :: TeamCollaboratorsError -> HttpError
teamCollaboratorsSubsystemErrorToHttpError =
  StdError . \case
    InsufficientRights -> errorToWai @E.InsufficientTeamPermissions
