module Wire.TeamCollaboratorsSubsystem.Interpreter where

import Control.Lens
import Control.Monad.Trans.Maybe
import Data.Id
import Data.Qualified
import Data.Set qualified as Set
import Imports
import Polysemy
import Polysemy.Error
import Wire.API.Team.Collaborator
import Wire.API.Team.Member
import Wire.GalleyAPIAccess
import Wire.TeamCollaboratorsStore qualified as Store
import Wire.TeamCollaboratorsSubsystem

runTeamCollaboratorsSubsystem ::
  ( Member GalleyAPIAccess r,
    Member (Error TeamCollaboratorsError) r,
    Member Store.TeamCollaboratorsStore r
  ) =>
  InterpreterFor TeamCollaboratorsSubsystem r
runTeamCollaboratorsSubsystem = interpret $ \case
  CreateTeamCollaborator zUser user team perms -> createTeamCollaboratorImpl zUser user team perms
  GetAllTeamCollaborators zUser team -> getAllTeamCollaboratorsImpl zUser team

createTeamCollaboratorImpl ::
  ( Member GalleyAPIAccess r,
    Member (Error TeamCollaboratorsError) r,
    Member Store.TeamCollaboratorsStore r
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

getAllTeamCollaboratorsImpl ::
  ( Member GalleyAPIAccess r,
    Member (Error TeamCollaboratorsError) r,
    Member Store.TeamCollaboratorsStore r
  ) =>
  Local UserId ->
  TeamId ->
  Sem r (Set UserId)
getAllTeamCollaboratorsImpl zUser team = do
  unlessM (isTeamAdmin (tUnqualified zUser) team) $
    throw InsufficientRights
  Set.fromList <$> Store.getAllTeamCollaborators team

isTeamAdmin ::
  (Member GalleyAPIAccess r) =>
  UserId ->
  TeamId ->
  Sem r Bool
isTeamAdmin user team =
  isJust <$> runMaybeT do
    member <- MaybeT $ getTeamMember user team
    guard (isAdminOrOwner (member ^. permissions))
