{-# LANGUAGE RecordWildCards #-}

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
import Hasql.Statement
import Hasql.TH
import Imports
import Polysemy
import Polysemy.Error (Error, throw)
import Polysemy.Input
import Wire.API.Team.Collaborator
import Wire.Postgres
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
    GetTeamCollaboratorsWithIds teamIds userIds -> getTeamCollaboratorsWithIdsImpl teamIds userIds
    UpdateTeamCollaborator userId teamId permissions -> updateTeamCollaboratorImpl userId teamId permissions
    RemoveTeamCollaborator userId teamId -> removeTeamCollaboratorImpl userId teamId

getTeamCollaboratorImpl ::
  (PGConstraints r) =>
  TeamId ->
  UserId ->
  Sem r (Maybe TeamCollaborator)
getTeamCollaboratorImpl teamId userId = do
  runStatement (teamId, userId) getTeamCollaboratorStatement
  where
    getTeamCollaboratorStatement :: Statement (TeamId, UserId) (Maybe TeamCollaborator)
    getTeamCollaboratorStatement =
      dimap
        (bimap toUUID toUUID)
        (fmap toTeamCollaborator)
        $ [maybeStatement|
          select user_id :: uuid, team_id :: uuid, permissions :: int2[] from collaborators where team_id = ($1 :: uuid) and user_id = ($2 :: uuid)
          |]

createTeamCollaboratorImpl ::
  ( PGConstraints r,
    Member (Error TeamCollaboratorsError) r
  ) =>
  UserId ->
  TeamId ->
  Set CollaboratorPermission ->
  Sem r ()
createTeamCollaboratorImpl userId teamId permissions = do
  mReturn <- runStatement (userId, teamId, permissions) insertStatement
  case mReturn of
    Just _ -> pure ()
    Nothing -> throw AlreadyExists
  where
    insertStatement :: Statement (UserId, TeamId, Set CollaboratorPermission) (Maybe Int32)
    insertStatement =
      lmap
        ( \(uid, tid, pms) ->
            (toUUID uid, toUUID tid, collaboratorPermissionToPostgreslRep <$> (Data.Vector.fromList . toAscList) pms)
        )
        $ [maybeStatement|
          insert into collaborators (user_id, team_id, permissions) values ($1 :: uuid, $2 :: uuid, $3 :: smallint[])
          on conflict do nothing
          returning (1 :: integer)
          |]

getAllTeamCollaboratorsImpl ::
  ( Member (Input Pool) r,
    Member (Embed IO) r,
    Member (Error UsageError) r
  ) =>
  TeamId ->
  Sem r [TeamCollaborator]
getAllTeamCollaboratorsImpl teamId = do
  runStatement teamId getAllTeamCollaboratorsStatement
  where
    getAllTeamCollaboratorsStatement :: Statement TeamId [TeamCollaborator]
    getAllTeamCollaboratorsStatement =
      dimap toUUID (Data.Vector.toList . (toTeamCollaborator <$>)) $
        [vectorStatement|
          select user_id :: uuid, team_id :: uuid, permissions :: int2[] from collaborators where team_id = ($1 :: uuid)
          |]

updateTeamCollaboratorImpl ::
  ( Member (Input Pool) r,
    Member (Embed IO) r,
    Member (Error UsageError) r
  ) =>
  UserId ->
  TeamId ->
  Set CollaboratorPermission ->
  Sem r ()
updateTeamCollaboratorImpl userId teamId permissions = do
  runStatement (userId, teamId, permissions) updateStatement
  where
    updateStatement :: Statement (UserId, TeamId, Set CollaboratorPermission) ()
    updateStatement =
      lmap
        ( \(uid, tid, pms) ->
            (toUUID uid, toUUID tid, collaboratorPermissionToPostgreslRep <$> (Data.Vector.fromList . toAscList) pms)
        )
        $ [resultlessStatement|
          update collaborators set permissions = ($3 :: smallint[]) where user_id = ($1 :: uuid) and team_id = ($2 :: uuid)
          |]

removeTeamCollaboratorImpl ::
  ( Member (Input Pool) r,
    Member (Embed IO) r,
    Member (Error UsageError) r
  ) =>
  UserId ->
  TeamId ->
  Sem r ()
removeTeamCollaboratorImpl userId teamId = do
  runStatement (userId, teamId) deleteStatement
  where
    deleteStatement :: Statement (UserId, TeamId) ()
    deleteStatement =
      lmap
        (bimap toUUID toUUID)
        $ [resultlessStatement|
          delete from collaborators where user_id = ($1 :: uuid) and team_id = ($2 :: uuid)
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
  runStatement teamId getAllCollaborationsByUserStatement
  where
    getAllCollaborationsByUserStatement :: Statement UserId [TeamCollaborator]
    getAllCollaborationsByUserStatement =
      dimap toUUID (Data.Vector.toList . (toTeamCollaborator <$>)) $
        [vectorStatement|
          select user_id :: uuid, team_id :: uuid, permissions :: int2[] from collaborators where user_id = ($1 :: uuid)
          |]

getTeamCollaboratorsWithIdsImpl ::
  ( Member (Input Pool) r,
    Member (Embed IO) r,
    Member (Error UsageError) r
  ) =>
  Set TeamId ->
  Set UserId ->
  Sem r [TeamCollaborator]
getTeamCollaboratorsWithIdsImpl teamIds userIds = do
  runStatement (Data.Set.toList teamIds, Data.Set.toList userIds) getTeamCollaboratorStatement
  where
    getTeamCollaboratorStatement :: Statement ([TeamId], [UserId]) [TeamCollaborator]
    getTeamCollaboratorStatement =
      dimap
        (bimap (Data.Vector.fromList . Imports.map toUUID) (Data.Vector.fromList . Imports.map toUUID))
        (Data.Vector.toList . (toTeamCollaborator <$>))
        $ [vectorStatement|
            select user_id :: uuid, team_id :: uuid, permissions :: int2[] from collaborators where team_id = ANY($1 :: uuid[]) and user_id = ANY($2 :: uuid[])
          |]
