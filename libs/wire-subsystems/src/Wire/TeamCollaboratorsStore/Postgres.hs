module Wire.TeamCollaboratorsStore.Postgres
  ( interpretTeamCollaboratorsStoreToPostgres,
  )
where

import Data.Bimap qualified as Bimap
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
          insert into collaborators (user_id, team_id, permissions) values ($1 :: uuid, $2 :: uuid, $3 :: smallint[])
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

-- We could rely on an `Ord` instance here. Howver, when the order is changed,
-- this will mess up spectaculary at run time. So, this extra mapping is meant
-- as a guard: Add to it, but don't change existing mappings!

collaboratorPermissionMap :: Bimap.Bimap Int16 CollaboratorPermission
collaboratorPermissionMap = Bimap.fromAscPairList [(0, CreateTeamConversation), (1, ImplicitConnection)]

collaboratorPermissionToPostgreslRep :: CollaboratorPermission -> Int16
collaboratorPermissionToPostgreslRep =
  (collaboratorPermissionMap Bimap.!> {- `!>` throws if the element isn't found -})
