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
    insertTeamRowIfAbsentPg,
    updateTeamPg,
    updateTeamNamePg,
    updateTeamIconPg,
    updateTeamIconKeyPg,
    updateTeamSplashScreenPg,
    updateTeamStatusPg,
    updateTeamSearchVisibilityPg,
    deleteTeamWithCassandra,
  )
where

import Cassandra (ClientState)
import Cassandra.Util (Writetime (..))
import Control.Lens ((.~), (^.))
import Data.Id
import Data.Range (Range, fromRange)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
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
  DeleteTeam tid -> deleteTeamWithCassandra tid
  SetTeamData tid upd -> do
    stamp <- Writetime <$> embed @IO getCurrentTime
    updateTeamPg tid upd (writetimeToUTC stamp)
  SetTeamStatus tid st -> do
    stamp <- Writetime <$> embed @IO getCurrentTime
    updateTeamStatusPg tid st (writetimeToUTC stamp)
  GetSearchVisibility tid -> getSearchVisibilityPg tid
  SetSearchVisibility tid v -> do
    stamp <- Writetime <$> embed @IO getCurrentTime
    updateTeamSearchVisibilityPg tid v (writetimeToUTC stamp)
  ResetSearchVisibility tid -> do
    stamp <- Writetime <$> embed @IO getCurrentTime
    updateTeamSearchVisibilityPg tid SearchVisibilityStandard (writetimeToUTC stamp)
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
  stamp <- embed @IO getCurrentTime
  insertTeamPg t stamp
  pure t

-- | Insert a fully specified team row guarded by the given writetime.
-- Shared by the direct-create path ('createTeamPg') and the DualWrite path
-- (which must reuse the Cassandra-generated team id); the migration copy
-- loop uses 'insertTeamRowIfAbsentPg' directly.  The row is inserted only
-- if absent and then reconciled with
-- per-column guards (@*col*_time is null or *col*_time <= stamp@), mirroring
-- Cassandra's last-write-wins semantics: a concurrent or later write with a
-- newer stamp always wins over this one, and vice versa.
insertTeamPg :: (PGConstraints r) => Team -> UTCTime -> Sem r ()
insertTeamPg t stamp = do
  insertTeamRowIfAbsentPg
    (t ^. teamId)
    (t ^. teamCreator)
    (t ^. teamName)
    (Just stamp)
    (postgresMarshall (t ^. teamIcon))
    (Just stamp)
    (t ^. teamIconKey)
    (Just stamp)
    Nothing
    Nothing
    (initialTeamStatus (t ^. teamBinding))
    (Just stamp)
    (t ^. teamBinding)
    (Just stamp)
    Nothing
    Nothing
  -- The guarded updates below also cover the conflict path of the
  -- insert-if-absent above.
  updateTeamNamePg (t ^. teamId) (t ^. teamName) stamp
  updateTeamIconPg (t ^. teamId) (postgresMarshall (t ^. teamIcon)) stamp
  updateTeamIconKeyPg (t ^. teamId) (t ^. teamIconKey) stamp
  updateTeamStatusPg (t ^. teamId) (initialTeamStatus (t ^. teamBinding)) stamp

initialTeamStatus :: TeamBinding -> TeamStatus
initialTeamStatus Binding = PendingActive
initialTeamStatus NonBinding = Active

-- | Insert-if-absent primitive over the full team row; the writetime columns
-- are passed through verbatim (@Nothing@ when the Cassandra column has no
-- writetime).  Shared with the migration copy loop.
insertTeamRowIfAbsentPg ::
  (PGConstraints r) =>
  TeamId ->
  UserId ->
  Text ->
  Maybe UTCTime ->
  Text ->
  Maybe UTCTime ->
  Maybe Text ->
  Maybe UTCTime ->
  Maybe Text ->
  Maybe UTCTime ->
  TeamStatus ->
  Maybe UTCTime ->
  TeamBinding ->
  Maybe UTCTime ->
  Maybe TeamSearchVisibility ->
  Maybe UTCTime ->
  Sem r ()
insertTeamRowIfAbsentPg tid creator name mNameTime icon mIconTime iconKey mIconKeyTime splash mSplashTime status mStatusTime binding mBindingTime searchVis mSearchVisTime =
  runStatement
    ( toUUID tid,
      toUUID creator,
      name,
      mNameTime,
      icon,
      mIconTime,
      iconKey,
      mIconKeyTime,
      splash,
      mSplashTime,
      postgresMarshall status,
      mStatusTime,
      postgresMarshall binding,
      mBindingTime,
      postgresMarshall <$> searchVis,
      mSearchVisTime
    )
    insertTeamRowIfAbsentStatement

insertTeamRowIfAbsentStatement ::
  Hasql.Statement
    (UUID, UUID, Text, Maybe UTCTime, Text, Maybe UTCTime, Maybe Text, Maybe UTCTime, Maybe Text, Maybe UTCTime, Text, Maybe UTCTime, Text, Maybe UTCTime, Maybe Text, Maybe UTCTime)
    ()
insertTeamRowIfAbsentStatement =
  [resultlessStatement|
    insert into team (team, creator, name, name_time, icon, icon_time, icon_key, icon_key_time, splash_screen, splash_screen_time, deleted, status, status_time, binding, binding_time, search_visibility, search_visibility_time)
    values ($1 :: uuid, $2 :: uuid, $3 :: text, $4 :: timestamptz?, $5 :: text, $6 :: timestamptz?, $7 :: text?, $8 :: timestamptz?, $9 :: text?, $10 :: timestamptz?, false, $11 :: text, $12 :: timestamptz?, $13 :: text, $14 :: timestamptz?, $15 :: text?, $16 :: timestamptz?)
    on conflict (team) do nothing
  |]

--------------------------------------------------------------------------------
-- Reads

getTeamPg :: (PGConstraints r) => TeamId -> Sem r (Maybe TeamData)
getTeamPg tid = runStatement (toUUID tid) selectTeamStatement

-- | Mirrors the row decoding of the Cassandra implementation: a deleted row
-- reports @PendingDelete@, missing status/splash-screen fields fall back to
-- their defaults, and the status time comes from the maintained
-- @status_time@ column (Cassandra's @writetime(status)@ carried over by the
-- copy loop).
selectTeamStatement :: Hasql.Statement UUID (Maybe TeamData)
selectTeamStatement =
  Hasql.refineResult
    (traverse toTeamData)
    [maybeStatement|
      select team :: uuid, creator :: uuid, name :: text, icon :: text, icon_key :: text?,
             deleted :: bool, status :: text?, status_time :: timestamptz?,
             binding :: text?, splash_screen :: text?
        from team
       where team = $1 :: uuid
    |]

toTeamData ::
  (UUID, UUID, Text, Text, Maybe Text, Bool, Maybe Text, Maybe UTCTime, Maybe Text, Maybe Text) ->
  Either Text TeamData
toTeamData (tid, creator, name, icon, iconKey, deleted, status, statusTime, binding, splash) = do
  i <- postgresUnmarshall icon
  s <- traverse (postgresUnmarshall @Text) status
  b' <- traverse (postgresUnmarshall @Text) binding
  ss <- traverse (postgresUnmarshall @Text) splash
  let st = fromMaybe Active s
      b = fromMaybe NonBinding b'
      t =
        newTeam (Id tid) (Id creator) name i b
          & teamIconKey .~ iconKey
          & teamSplashScreen .~ fromMaybe DefaultIcon ss
  pure (TeamData t (if deleted then PendingDelete else st) statusTime)

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
      select binding :: text? from team where team = any($1 :: uuid[])
    |]
  where
    toBinding :: Maybe Text -> Either Text TeamBinding
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
      select binding :: text? from team where team = $1 :: uuid
    |]
  where
    toBinding :: Maybe Text -> Either Text TeamBinding
    toBinding Nothing = Right NonBinding
    toBinding (Just b) = postgresUnmarshall b

getTeamCreationTimePg :: (PGConstraints r) => TeamId -> Sem r (Maybe TeamCreationTime)
getTeamCreationTimePg tid = do
  v <- runStatement (toUUID tid) selectTeamCreationTimeStatement
  pure (toCreationTime <$> join v)

toCreationTime :: UTCTime -> TeamCreationTime
toCreationTime t =
  TeamCreationTime (floor ((1e6 :: Double) * realToFrac (utcTimeToPOSIXSeconds t)))

-- | The @binding_time@ column carries over Cassandra's @writetime(binding)@;
-- like Cassandra's @checkCreation@, it is 'Nothing' both for a missing team
-- row and for a row whose binding column has no writetime.
selectTeamCreationTimeStatement :: Hasql.Statement UUID (Maybe (Maybe UTCTime))
selectTeamCreationTimeStatement =
  [maybeStatement|
    select binding_time :: timestamptz? from team where team = $1 :: uuid
  |]

getSearchVisibilityPg :: (PGConstraints r) => TeamId -> Sem r TeamSearchVisibility
getSearchVisibilityPg tid = runStatement (toUUID tid) selectSearchVisibilityStatement

selectSearchVisibilityStatement :: Hasql.Statement UUID TeamSearchVisibility
selectSearchVisibilityStatement =
  Hasql.refineResult
    toSearchVisibility
    [maybeStatement|
      select search_visibility :: text? from team where team = $1 :: uuid
    |]

toSearchVisibility :: Maybe (Maybe Text) -> Either Text TeamSearchVisibility
toSearchVisibility mv = maybe (Right SearchVisibilityStandard) postgresUnmarshall (join mv)

--------------------------------------------------------------------------------
-- Writes

-- | The Cassandra terminal delete writetime: the CQL literal
-- @32503680000000000@ microseconds since the epoch
-- (Wire.TeamStore.Cassandra.Queries.deleteTeam).  Finalizing the Postgres
-- deletion with the same stamp makes every later guarded write lose the
-- @*_time <= stamp@ comparison, so deletion can never be resurrected by an
-- in-flight update.
teamDeleteWritetime :: UTCTime
teamDeleteWritetime = posixSecondsToUTCTime 32503680000

-- | Team deletion with the Postgres metadata leg and the Cassandra member/
-- conversation cleanup: the team is first parked in @PendingDelete@ (the
-- requeue scan in Galley.API.Teams.lookupTeam only picks up
-- @PendingDelete@ teams), then the Cassandra cleanup runs, then the
-- Postgres row is finalized with the terminal Cassandra delete writetime.
-- If the cleanup is interrupted, the @PendingDelete@ status makes the next
-- requeue pass retry the whole deletion.
deleteTeamWithCassandra ::
  ( PGConstraints r,
    Member (Input ClientState) r,
    Member TinyLog r,
    Member ConversationStore r
  ) =>
  TeamId ->
  Sem r ()
deleteTeamWithCassandra tid = do
  -- The park uses the terminal delete writetime (not now()) so that no
  -- guarded write can overwrite the PendingDelete marker while the Cassandra
  -- cleanup runs; the guard passes unconditionally at this stamp, making
  -- retries of this function idempotent.
  updateTeamStatusPg tid PendingDelete teamDeleteWritetime
  routeCassandra (deleteTeam tid)
  runStatement
    (postgresMarshall Deleted, teamDeleteWritetime, teamDeleteWritetime, teamDeleteWritetime, toUUID tid)
    finalizeTeamDeletionStatement

finalizeTeamDeletionStatement :: Hasql.Statement (Text, UTCTime, UTCTime, UTCTime, UUID) ()
finalizeTeamDeletionStatement =
  [resultlessStatement|
    update team
       set name = 'default',
           icon = 'default',
           status = $1 :: text,
           name_time = $2 :: timestamptz,
           icon_time = $3 :: timestamptz,
           status_time = $4 :: timestamptz
     where team = $5 :: uuid
  |]

-- | Replaces the Cassandra logged batch of per-column updates with one
-- guarded update per present field; absent update fields keep their current
-- value.
updateTeamPg :: (PGConstraints r) => TeamId -> TeamUpdateData -> UTCTime -> Sem r ()
updateTeamPg tid u stamp = do
  for_ (u ^. nameUpdate) $ \n -> updateTeamNamePg tid (fromRange n) stamp
  for_ (u ^. iconUpdate) $ \i -> updateTeamIconPg tid (postgresMarshall i) stamp
  for_ (u ^. iconKeyUpdate) $ \k -> updateTeamIconKeyPg tid (Just (fromRange k)) stamp
  for_ (u ^. splashScreenUpdate) $ \ss -> updateTeamSplashScreenPg tid (postgresMarshall ss) stamp

-- | Guarded per-column writers below: a write only applies when the column
-- has no writetime yet or its writetime is not newer than the given stamp
-- (Cassandra LWW semantics; the row lock makes the WHERE re-evaluation
-- atomic under READ COMMITTED).
updateTeamNamePg :: (PGConstraints r) => TeamId -> Text -> UTCTime -> Sem r ()
updateTeamNamePg tid n stamp =
  runStatement (toUUID tid, n, stamp, stamp) updateTeamNameStatement

updateTeamNameStatement :: Hasql.Statement (UUID, Text, UTCTime, UTCTime) ()
updateTeamNameStatement =
  [resultlessStatement|
    update team set name = $2 :: text, name_time = $3 :: timestamptz
     where team = $1 :: uuid and (name_time is null or name_time <= $4 :: timestamptz)
  |]

updateTeamIconPg :: (PGConstraints r) => TeamId -> Text -> UTCTime -> Sem r ()
updateTeamIconPg tid i stamp =
  runStatement (toUUID tid, i, stamp, stamp) updateTeamIconStatement

updateTeamIconStatement :: Hasql.Statement (UUID, Text, UTCTime, UTCTime) ()
updateTeamIconStatement =
  [resultlessStatement|
    update team set icon = $2 :: text, icon_time = $3 :: timestamptz
     where team = $1 :: uuid and (icon_time is null or icon_time <= $4 :: timestamptz)
  |]

updateTeamIconKeyPg :: (PGConstraints r) => TeamId -> Maybe Text -> UTCTime -> Sem r ()
updateTeamIconKeyPg tid k stamp =
  runStatement (toUUID tid, k, stamp, stamp) updateTeamIconKeyStatement

updateTeamIconKeyStatement :: Hasql.Statement (UUID, Maybe Text, UTCTime, UTCTime) ()
updateTeamIconKeyStatement =
  [resultlessStatement|
    update team set icon_key = $2 :: text?, icon_key_time = $3 :: timestamptz
     where team = $1 :: uuid and (icon_key_time is null or icon_key_time <= $4 :: timestamptz)
  |]

updateTeamSplashScreenPg :: (PGConstraints r) => TeamId -> Text -> UTCTime -> Sem r ()
updateTeamSplashScreenPg tid ss stamp =
  runStatement (toUUID tid, ss, stamp, stamp) updateTeamSplashScreenStatement

updateTeamSplashScreenStatement :: Hasql.Statement (UUID, Text, UTCTime, UTCTime) ()
updateTeamSplashScreenStatement =
  [resultlessStatement|
    update team set splash_screen = $2 :: text, splash_screen_time = $3 :: timestamptz
     where team = $1 :: uuid and (splash_screen_time is null or splash_screen_time <= $4 :: timestamptz)
  |]

updateTeamStatusPg :: (PGConstraints r) => TeamId -> TeamStatus -> UTCTime -> Sem r ()
updateTeamStatusPg tid st stamp =
  runStatement (toUUID tid, postgresMarshall st, stamp, stamp) updateTeamStatusStatement

updateTeamStatusStatement :: Hasql.Statement (UUID, Text, UTCTime, UTCTime) ()
updateTeamStatusStatement =
  [resultlessStatement|
    update team set status = $2 :: text, status_time = $3 :: timestamptz
     where team = $1 :: uuid and (status_time is null or status_time <= $4 :: timestamptz)
  |]

updateTeamSearchVisibilityPg :: (PGConstraints r) => TeamId -> TeamSearchVisibility -> UTCTime -> Sem r ()
updateTeamSearchVisibilityPg tid v stamp =
  runStatement (toUUID tid, postgresMarshall v, stamp, stamp) updateTeamSearchVisibilityStatement

updateTeamSearchVisibilityStatement :: Hasql.Statement (UUID, Text, UTCTime, UTCTime) ()
updateTeamSearchVisibilityStatement =
  [resultlessStatement|
    update team set search_visibility = $2 :: text, search_visibility_time = $3 :: timestamptz
     where team = $1 :: uuid and (search_visibility_time is null or search_visibility_time <= $4 :: timestamptz)
  |]
