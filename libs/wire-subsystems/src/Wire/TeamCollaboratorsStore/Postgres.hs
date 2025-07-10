module Wire.TeamCollaboratorsStore.Postgres
  ( interpretTeamCollaboratorsStoreToPostgres,
  )
where

import Data.Id
import Data.Profunctor
import Data.Set
import Data.Vector
import Hasql.Pool
import Hasql.Session
import Hasql.Statement
import Hasql.TH
import Imports
import Polysemy
import Polysemy.Error (Error, throw)
import Polysemy.Input
import Wire.API.Team.Collaborator
import Wire.TeamCollaboratorsStore

interpretTeamCollaboratorsStoreToPostgres ::
  ( Member (Embed IO) r,
    Member (Input Pool) r,
    Member (Error UsageError) r
  ) =>
  InterpreterFor TeamCollaboratorsStore r
interpretTeamCollaboratorsStoreToPostgres =
  interpret $ \case
    CreateTeamCollaborator userId teamId permissions -> createTeamCollaboratorImpl userId teamId permissions
    GetAllTeamCollaborators teamId -> getAllTeamCollaboratorsImpl teamId

createTeamCollaboratorImpl ::
  ( Member (Input Pool) r,
    Member (Embed IO) r,
    Member (Error UsageError) r
  ) =>
  UserId ->
  TeamId ->
  Set CollaboratorPermission ->
  Sem r ()
createTeamCollaboratorImpl userId teamId permissions = do
  pool <- input
  eitherTeamCollaborators <- liftIO $ use pool session
  either throw pure eitherTeamCollaborators
  where
    session :: Session ()
    session = statement (userId, teamId, permissions) insertStatement

    insertStatement :: Statement (UserId, TeamId, Set CollaboratorPermission) ()
    insertStatement =
      lmap
        ( \(uid, tid, pms) ->
            (toUUID uid, toUUID tid, collaboratorPermissionToPostgreslRep <$> (Data.Vector.fromList . toAscList) pms)
        )
        $ [resultlessStatement|
          insert into collaborators (user_id, team_id, permissions) values ($1 :: uuid, $2 :: uuid, $3 :: text[])
          |]

getAllTeamCollaboratorsImpl ::
  ( Member (Input Pool) r,
    Member (Embed IO) r,
    Member (Error UsageError) r
  ) =>
  TeamId ->
  Sem r [UserId]
getAllTeamCollaboratorsImpl teamId = do
  pool <- input
  eitherTeamCollaborators <- liftIO $ use pool session
  either throw pure eitherTeamCollaborators
  where
    session :: Session [UserId]
    session = statement teamId getAllTeamCollaboratorsStatement

    getAllTeamCollaboratorsStatement :: Statement TeamId [UserId]
    getAllTeamCollaboratorsStatement =
      dimap toUUID (Data.Vector.toList . (Id <$>)) $
        [vectorStatement|
          select (user_id :: uuid) from collaborators where team_id = ($1 :: uuid)
          |]

collaboratorPermissionToPostgreslRep :: CollaboratorPermission -> Text
collaboratorPermissionToPostgreslRep CreateTeamConversation = "create_team_conversation"
collaboratorPermissionToPostgreslRep ImplicitConnection = "implicit_connection"
