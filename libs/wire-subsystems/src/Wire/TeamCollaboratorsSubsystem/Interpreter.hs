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
  CreateTeamCollaborator zUser userId teamId permissions -> todo "Implement me"
  GetAllTeamCollaborators zUser teamId -> getAllTeamCollaboratorsImpl zUser teamId

getAllTeamCollaboratorsImpl ::
  ( Member GalleyAPIAccess r,
    Member (Error TeamCollaboratorsError) r,
    Member Store.TeamCollaboratorsStore r
  ) =>
  Local UserId ->
  TeamId ->
  Sem r (Set UserId)
getAllTeamCollaboratorsImpl lUsr team = do
  unlessM (isTeamAdmin (tUnqualified lUsr) team) $
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
