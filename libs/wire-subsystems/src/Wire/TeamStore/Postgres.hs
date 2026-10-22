-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.TeamStore.Postgres
  ( interpretTeamStoreMetadataToPostgres,
    routeCassandra,
    insertTeamPg,
    updateTeamPg,
    updateTeamStatusPg,
    setSearchVisibilityPg,
    resetSearchVisibilityPg,
    deleteTeamPg,
  )
where

import Cassandra (ClientState)
import Control.Lens ((.~), (^.))
import Data.Id
import Data.Range (Range, fromRange)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import Data.Vector (Vector)
import Data.Vector qualified as V
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog
import Wire.API.PostgresMarshall
import Wire.API.Routes.Internal.Galley.TeamsIntra
import Wire.API.Team
import Wire.API.Team.SearchVisibility
import Wire.ConversationStore (ConversationStore)
import Wire.Postgres
import Wire.TeamStore
import Wire.TeamStore.Cassandra qualified as Cassandra

-- | Interprets the team-metadata constructors of 'TeamStore' against the
-- Postgres @team@ table.  All other constructors (team members) are routed
-- through the Cassandra interpreter.
interpretTeamStoreMetadataToPostgres ::
  ( PGConstraints r,
    Member (Input ClientState) r,
    Member TinyLog r,
    Member ConversationStore r
  ) =>
  InterpreterFor TeamStore r
interpretTeamStoreMetadataToPostgres = interpret $ \case
  CreateTeam mtid uid n i k b -> createTeamPg mtid uid n i k b
  GetTeam tid -> getTeamPg tid
  GetTeamName tid -> getTeamNamePg tid
  GetTeamsBindings tids -> getTeamsBindingsPg tids
  GetTeamBinding tid -> getTeamBindingPg tid
  GetTeamCreationTime tid -> getTeamCreationTimePg tid
  DeleteTeam tid -> do
    -- The member removal and conversation deletion live in the Cassandra
    -- implementation (member tables are still Cassandra-owned in this PR), so
    -- the whole constructor is additionally routed through the Cassandra
    -- interpreter, which also rewrites the metadata there.
    deleteTeamPg tid
    routeCassandra (deleteTeam tid)
  SetTeamData tid upd -> updateTeamPg tid upd
  SetTeamStatus tid st -> updateTeamStatusPg tid st
  GetSearchVisibility tid -> getSearchVisibilityPg tid
  SetSearchVisibility tid v -> setSearchVisibilityPg tid v
  ResetSearchVisibility tid -> resetSearchVisibilityPg tid
  -- Team members are out of scope for the Postgres metadata migration.
  CreateTeamMember tid mem -> routeCassandra (createTeamMember tid mem)
  SetTeamMemberPermissions p0 tid uid p1 -> routeCassandra (setTeamMemberPermissions p0 tid uid p1)
  DeleteTeamMember tid uid -> routeCassandra (deleteTeamMember tid uid)
  GetTeamMember tid uid -> routeCassandra (getTeamMember tid uid)
  GetTeamMembers tid -> routeCassandra (getTeamMembers tid)
  GetTeamMembersWithLimit tid n -> routeCassandra (getTeamMembersWithLimit tid n)
  SelectTeamMembers tid uids -> routeCassandra (selectTeamMembers tid uids)
  SelectTeamMemberInfos tid uids -> routeCassandra (selectTeamMemberInfos tid uids)
  GetBillingTeamMembers tid -> routeCassandra (getBillingTeamMembers tid)
  GetTeamAdmins tid -> routeCassandra (getTeamAdmins tid)
  SelectTeams uid tids -> routeCassandra (selectTeams uid tids)
  GetUserTeams uid -> routeCassandra (getUserTeams uid)
  GetUsersTeams uids -> routeCassandra (getUsersTeams uids)
  GetOneUserTeam uid -> routeCassandra (getOneUserTeam uid)

-- | Runs a 'TeamStore' action through the Cassandra interpreter.
routeCassandra ::
  ( Member (Embed IO) r,
    Member (Input ClientState) r,
    Member TinyLog r,
    Member ConversationStore r
  ) =>
  Sem (TeamStore ': r) x ->
  Sem r x
routeCassandra = Cassandra.interpretTeamStoreToCassandra

--------------------------------------------------------------------------------
-- Create

createTeamPg ::
  (PGConstraints r) =>
  Maybe TeamId ->
  UserId ->
  Range 1 256 Text ->
  Icon ->
  Maybe (Range 1 256 Text) ->
  TeamBinding ->
  Sem r Team
createTeamPg mtid uid (fromRange -> n) i k b = do
  tid <- embed @IO (maybe (Id <$> UUID.nextRandom) pure mtid)
  let t = newTeam tid uid n i b & teamIconKey .~ (fromRange <$> k)
  insertTeamPg t
  pure t

-- | Insert a fully specified team row.  Shared by the direct-create path
-- ('createTeamPg') and the DualWrite path (which must reuse the
-- Cassandra-generated team id).  The status is derived from the binding flag
-- exactly like the Cassandra implementation does at creation time, and
-- @created_at@ is stamped with the insert time (the copy loop reconciles it
-- with Cassandra's @writetime(binding)@ during migration).
insertTeamPg :: (PGConstraints r) => Team -> Sem r ()
insertTeamPg t =
  runStatement
    ( toUUID (t ^. teamId),
      toUUID (t ^. teamCreator),
      t ^. teamName,
      postgresMarshall (t ^. teamIcon),
      t ^. teamIconKey,
      postgresMarshall (initialTeamStatus (t ^. teamBinding)),
      postgresMarshall (t ^. teamBinding)
    )
    insertTeamStatement

initialTeamStatus :: TeamBinding -> TeamStatus
initialTeamStatus Binding = PendingActive
initialTeamStatus NonBinding = Active

insertTeamStatement :: Hasql.Statement (UUID, UUID, Text, Text, Maybe Text, Int32, Bool) ()
insertTeamStatement =
  [resultlessStatement|
    insert into team (team, creator, name, icon, icon_key, deleted, status, binding, created_at)
    values ($1 :: uuid, $2 :: uuid, $3 :: text, $4 :: text, $5 :: text?, false, $6 :: int, $7 :: bool, now())
  |]

--------------------------------------------------------------------------------
-- Reads

getTeamPg :: (PGConstraints r) => TeamId -> Sem r (Maybe TeamData)
getTeamPg tid = runStatement (toUUID tid) selectTeamStatement

-- | Mirrors the row decoding of the Cassandra implementation: a deleted row
-- reports @PendingDelete@, missing status/binding/splash-screen fields fall
-- back to their defaults, and there is no status writetime in Postgres.
selectTeamStatement :: Hasql.Statement UUID (Maybe TeamData)
selectTeamStatement =
  Hasql.refineResult
    (traverse toTeamData)
    [maybeStatement|
      select team :: uuid, creator :: uuid, name :: text, icon :: text, icon_key :: text?,
             deleted :: bool, status :: int, binding :: bool, splash_screen :: text?
        from team
       where team = $1 :: uuid
    |]

toTeamData ::
  (UUID, UUID, Text, Text, Maybe Text, Bool, Int32, Bool, Maybe Text) ->
  Either Text TeamData
toTeamData (tid, creator, name, icon, iconKey, deleted, status, binding, splash) = do
  i <- postgresUnmarshall icon
  s <- postgresUnmarshall status
  b <- postgresUnmarshall binding
  ss <- traverse (postgresUnmarshall @Text) splash
  let t =
        newTeam (Id tid) (Id creator) name i b
          & teamIconKey .~ iconKey
          & teamSplashScreen .~ fromMaybe DefaultIcon ss
  pure (TeamData t (if deleted then PendingDelete else s) Nothing)

getTeamNamePg :: (PGConstraints r) => TeamId -> Sem r (Maybe Text)
getTeamNamePg tid = runStatement (toUUID tid) selectTeamNameStatement

selectTeamNameStatement :: Hasql.Statement UUID (Maybe Text)
selectTeamNameStatement =
  [maybeStatement|
    select name :: text from team where team = $1 :: uuid
  |]

-- | Returns the bindings of the requested teams; teams without a row are
-- omitted and a NULL binding defaults to 'NonBinding' (mirroring the
-- Cassandra implementation).
getTeamsBindingsPg :: (PGConstraints r) => [TeamId] -> Sem r [TeamBinding]
getTeamsBindingsPg tids =
  runStatement (V.fromList (map toUUID tids)) selectTeamBindingsStatement

selectTeamBindingsStatement :: Hasql.Statement (Vector UUID) [TeamBinding]
selectTeamBindingsStatement =
  Hasql.refineResult
    (traverse toBinding . V.toList)
    [vectorStatement|
      select binding :: bool? from team where team = any($1 :: uuid[])
    |]
  where
    toBinding :: Maybe Bool -> Either Text TeamBinding
    toBinding Nothing = Right NonBinding
    toBinding (Just b) = postgresUnmarshall b

-- | 'Nothing' for a missing team row; a NULL binding defaults to 'NonBinding'
-- (mirroring the Cassandra implementation).
getTeamBindingPg :: (PGConstraints r) => TeamId -> Sem r (Maybe TeamBinding)
getTeamBindingPg tid = runStatement (toUUID tid) selectTeamBindingStatement

selectTeamBindingStatement :: Hasql.Statement UUID (Maybe TeamBinding)
selectTeamBindingStatement =
  Hasql.refineResult
    (traverse toBinding)
    [maybeStatement|
      select binding :: bool? from team where team = $1 :: uuid
    |]
  where
    toBinding :: Maybe Bool -> Either Text TeamBinding
    toBinding Nothing = Right NonBinding
    toBinding (Just b) = postgresUnmarshall b

getTeamCreationTimePg :: (PGConstraints r) => TeamId -> Sem r (Maybe TeamCreationTime)
getTeamCreationTimePg tid = do
  v <- runStatement (toUUID tid) selectTeamCreationTimeStatement
  pure (toCreationTime <$> join v)

toCreationTime :: UTCTime -> TeamCreationTime
toCreationTime t =
  TeamCreationTime (floor ((1e6 :: Double) * realToFrac (utcTimeToPOSIXSeconds t)))

-- | The @created_at@ column is backfilled from Cassandra's
-- @writetime(binding)@, which is microseconds since the epoch; a missing
-- timestamp (very old rows) maps to 'Nothing'.
selectTeamCreationTimeStatement :: Hasql.Statement UUID (Maybe (Maybe UTCTime))
selectTeamCreationTimeStatement =
  [maybeStatement|
    select created_at :: timestamptz? from team where team = $1 :: uuid
  |]

getSearchVisibilityPg :: (PGConstraints r) => TeamId -> Sem r TeamSearchVisibility
getSearchVisibilityPg tid = runStatement (toUUID tid) selectSearchVisibilityStatement

selectSearchVisibilityStatement :: Hasql.Statement UUID TeamSearchVisibility
selectSearchVisibilityStatement =
  Hasql.refineResult
    toSearchVisibility
    [maybeStatement|
      select search_visibility :: int? from team where team = $1 :: uuid
    |]

toSearchVisibility :: Maybe (Maybe Int32) -> Either Text TeamSearchVisibility
toSearchVisibility mv = maybe (Right SearchVisibilityStandard) postgresUnmarshall (join mv)

--------------------------------------------------------------------------------
-- Writes

-- | The metadata half of team deletion: resets name and icon to @default@ and
-- marks the team deleted (mirroring the Cassandra implementation).  Removing
-- the member rows is done by the Cassandra interpreter.
deleteTeamPg :: (PGConstraints r) => TeamId -> Sem r ()
deleteTeamPg tid =
  runStatement (postgresMarshall Deleted, toUUID tid) deleteTeamStatement

deleteTeamStatement :: Hasql.Statement (Int32, UUID) ()
deleteTeamStatement =
  [resultlessStatement|
    update team set name = 'default', icon = 'default', status = $1 :: int where team = $2 :: uuid
  |]

-- | Replaces the Cassandra logged batch of per-column updates with a single
-- UPDATE; absent update fields keep their current value.
updateTeamPg :: (PGConstraints r) => TeamId -> TeamUpdateData -> Sem r ()
updateTeamPg tid u =
  runStatement
    ( toUUID tid,
      fromRange <$> u ^. nameUpdate,
      postgresMarshall <$> u ^. iconUpdate,
      fromRange <$> u ^. iconKeyUpdate,
      postgresMarshall <$> u ^. splashScreenUpdate
    )
    updateTeamStatement

updateTeamStatement :: Hasql.Statement (UUID, Maybe Text, Maybe Text, Maybe Text, Maybe Text) ()
updateTeamStatement =
  [resultlessStatement|
    update team
       set name = coalesce($2 :: text?, name),
           icon = coalesce($3 :: text?, icon),
           icon_key = coalesce($4 :: text?, icon_key),
           splash_screen = coalesce($5 :: text?, splash_screen)
     where team = $1 :: uuid
  |]

updateTeamStatusPg :: (PGConstraints r) => TeamId -> TeamStatus -> Sem r ()
updateTeamStatusPg tid st =
  runStatement (postgresMarshall st, toUUID tid) updateTeamStatusStatement

updateTeamStatusStatement :: Hasql.Statement (Int32, UUID) ()
updateTeamStatusStatement =
  [resultlessStatement|
    update team set status = $1 :: int where team = $2 :: uuid
  |]

setSearchVisibilityPg :: (PGConstraints r) => TeamId -> TeamSearchVisibility -> Sem r ()
setSearchVisibilityPg tid v =
  runStatement (postgresMarshall v, toUUID tid) setSearchVisibilityStatement

setSearchVisibilityStatement :: Hasql.Statement (Int32, UUID) ()
setSearchVisibilityStatement =
  [resultlessStatement|
    update team set search_visibility = $1 :: int where team = $2 :: uuid
  |]

resetSearchVisibilityPg :: (PGConstraints r) => TeamId -> Sem r ()
resetSearchVisibilityPg tid = setSearchVisibilityPg tid SearchVisibilityStandard
