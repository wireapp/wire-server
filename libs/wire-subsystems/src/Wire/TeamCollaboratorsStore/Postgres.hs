{-# LANGUAGE RecordWildCards #-}

module Wire.TeamCollaboratorsStore.Postgres
  ( interpretTeamCollaboratorsStoreToPostgres,
  )
where

import Data.Bimap qualified as Bimap
import Data.Id
import Data.Profunctor
import Data.Set
import Data.Set qualified as Set
import Data.UUID
import Data.Vector hiding (mapM)
import Hasql.Pool
import Hasql.Session
import Hasql.Statement
import Hasql.TH
import Imports
import Polysemy
import Polysemy.Error (Error, throw)
import Polysemy.Input
import PostgreSQL.ErrorCodes
import Wire.API.Team.Collaborator
import Wire.TeamCollaboratorsStore

interpretTeamCollaboratorsStoreToPostgres ::
  ( Member (Embed IO) r,
    Member (Input Pool) r,
    Member (Error UsageError) r,
    Member (Error TeamCollaboratorsError) r
  ) =>
  InterpreterFor TeamCollaboratorsStore r
interpretTeamCollaboratorsStoreToPostgres =
  interpret $ \case
    CreateTeamCollaborator userId teamId permissions -> createTeamCollaboratorImpl userId teamId permissions
    GetAllTeamCollaborators teamId -> getAllTeamCollaboratorsImpl teamId
    GetTeamCollaborator teamId userId -> getTeamCollaboratorImpl teamId userId
    GetTeamCollaborations userId -> getTeamCollaborationsImpl userId

getTeamCollaboratorImpl ::
  ( Member (Input Pool) r,
    Member (Embed IO) r,
    Member (Error UsageError) r
  ) =>
  TeamId ->
  UserId ->
  Sem r (Maybe TeamCollaborator)
getTeamCollaboratorImpl teamId userId = do
  pool <- input
  eitherTeamCollaborator <- liftIO $ use pool session
  either throw pure eitherTeamCollaborator
  where
    session :: Session (Maybe TeamCollaborator)
    session = statement (teamId, userId) getTeamCollaboratorStatement

    getTeamCollaboratorStatement :: Statement (TeamId, UserId) (Maybe TeamCollaborator)
    getTeamCollaboratorStatement =
      dimap
        (bimap toUUID toUUID)
        (fmap toTeamCollaborator)
        $ [maybeStatement|
          select user_id :: uuid, team_id :: uuid, permissions :: int2[] from collaborators where team_id = ($1 :: uuid) and user_id = ($2 :: uuid)
          |]

createTeamCollaboratorImpl ::
  ( Member (Input Pool) r,
    Member (Embed IO) r,
    Member (Error UsageError) r,
    Member (Error TeamCollaboratorsError) r
  ) =>
  UserId ->
  TeamId ->
  Set CollaboratorPermission ->
  Sem r ()
createTeamCollaboratorImpl userId teamId permissions = do
  pool <- input
  eitherErrorOrUnit <- liftIO $ use pool session
  either errHandler pure eitherErrorOrUnit
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

    errHandler ::
      ( Member (Error UsageError) r',
        Member (Error TeamCollaboratorsError) r'
      ) =>
      UsageError ->
      Sem r' ()
    errHandler (SessionUsageError (QueryError _ _ (ResultError (ServerError code _ _ _ _))))
      | code == unique_violation = throw AlreadyExists
    errHandler e = throw e

getAllTeamCollaboratorsImpl ::
  ( Member (Input Pool) r,
    Member (Embed IO) r,
    Member (Error UsageError) r
  ) =>
  TeamId ->
  Sem r [TeamCollaborator]
getAllTeamCollaboratorsImpl teamId = do
  pool <- input
  eitherTeamCollaborators <- liftIO $ use pool session
  either throw pure eitherTeamCollaborators
  where
    session :: Session [TeamCollaborator]
    session = statement teamId getAllTeamCollaboratorsStatement

    getAllTeamCollaboratorsStatement :: Statement TeamId [TeamCollaborator]
    getAllTeamCollaboratorsStatement =
      dimap toUUID (Data.Vector.toList . (toTeamCollaborator <$>)) $
        [vectorStatement|
          select user_id :: uuid, team_id :: uuid, permissions :: int2[] from collaborators where team_id = ($1 :: uuid)
          |]

toTeamCollaborator :: (UUID, UUID, Vector Int16) -> TeamCollaborator
toTeamCollaborator ((Id -> gUser), (Id -> gTeam), (toPermissions -> gPermissions)) =
  TeamCollaborator {..}

toPermissions :: Vector Int16 -> Set CollaboratorPermission
toPermissions = Data.Vector.foldr (Set.insert . postgreslRepToCollaboratorPermission) Set.empty

-- We could rely on an `Ord` instance here. Howver, when the order is changed,
-- this will mess up spectaculary at run time. So, this extra mapping is meant
-- as a guard: Add to it, but don't change existing mappings!

collaboratorPermissionMap :: Bimap.Bimap Int16 CollaboratorPermission
collaboratorPermissionMap = Bimap.fromAscPairList [(0, CreateTeamConversation), (1, ImplicitConnection)]

collaboratorPermissionToPostgreslRep :: CollaboratorPermission -> Int16
collaboratorPermissionToPostgreslRep =
  (collaboratorPermissionMap Bimap.!> {- `!>` throws if the element isn't found -})

postgreslRepToCollaboratorPermission :: Int16 -> CollaboratorPermission
postgreslRepToCollaboratorPermission =
  (collaboratorPermissionMap Bimap.! {- `!` throws if the element isn't found -})

getTeamCollaborationsImpl ::
  ( Member (Input Pool) r,
    Member (Embed IO) r,
    Member (Error UsageError) r
  ) =>
  UserId ->
  Sem r [TeamCollaborator]
getTeamCollaborationsImpl teamId = do
  pool <- input
  eitherTeamCollaborators <- liftIO $ use pool session
  either throw pure eitherTeamCollaborators
  where
    session :: Session [TeamCollaborator]
    session = statement teamId getAllUserCollaborationsStatement

    getAllUserCollaborationsStatement :: Statement UserId [TeamCollaborator]
    getAllUserCollaborationsStatement =
      dimap toUUID (Data.Vector.toList . (toTeamCollaborator <$>)) $
        [vectorStatement|
          select user_id :: uuid, team_id :: uuid, permissions :: int2[] from collaborators where user_id = ($1 :: uuid)
          |]
