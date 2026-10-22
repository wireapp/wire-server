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
    insertTeamPg,
    updateTeamPg,
    updateTeamStatusPg,
    setSearchVisibilityPg,
    resetSearchVisibilityPg,
    deleteTeamPg,
    addTeamMemberPg,
    updateTeamMemberPg,
    removeTeamMemberPg,
    removeTeamMemberRowsPg,
  )
where

import Cassandra (ClientState)
import Control.Lens ((.~), (^.), (^?), _1, _2, _Just)
import Data.Id
import Data.Json.Util (UTCTimeMillis (..), toUTCTimeMillis)
import Data.LegalHold (defUserLegalHoldStatus)
import Data.Map.Strict qualified as Map
import Data.Range (Range, fromRange, unsafeRange)
import Data.Set qualified as Set
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import Data.Vector (Vector)
import Data.Vector qualified as V
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Hasql.Transaction qualified as Transaction
import Hasql.Transaction.Sessions (IsolationLevel (ReadCommitted), Mode (..))
import Imports
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog
import Wire.API.PostgresMarshall
import Wire.API.Routes.Internal.Galley.TeamsIntra
import Wire.API.Team
import Wire.API.Team.Member
import Wire.API.Team.Member.Info qualified as Info
import Wire.API.Team.Permission (Perm (SetBilling), Permissions, self)
import Wire.API.Team.SearchVisibility
import Wire.ConversationStore (ConversationStore)
import Wire.Postgres
import Wire.TeamStore
import Wire.TeamStore.Cassandra qualified as Cassandra

-- | Interprets 'TeamStore' against Postgres: team metadata (WPB-22956) and
-- team members (WPB-22957) live in the @team@, @team_member@, @user_team@,
-- @billing_team_member@ and @team_admin@ tables.
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
    removeTeamMemberRowsPg tid
    Cassandra.interpretTeamStoreToCassandra (deleteTeam tid)
  SetTeamData tid upd -> updateTeamPg tid upd
  SetTeamStatus tid st -> updateTeamStatusPg tid st
  GetSearchVisibility tid -> getSearchVisibilityPg tid
  SetSearchVisibility tid v -> setSearchVisibilityPg tid v
  ResetSearchVisibility tid -> resetSearchVisibilityPg tid
  -- Team members (WPB-22957).
  CreateTeamMember tid mem -> addTeamMemberPg tid mem
  SetTeamMemberPermissions old tid uid new -> updateTeamMemberPg old tid uid new
  DeleteTeamMember tid uid -> removeTeamMemberPg tid uid
  GetTeamMember tid uid -> teamMemberPg tid uid
  GetTeamMembers tid -> teamMembersCollectedPg tid
  GetTeamMembersWithLimit tid n -> teamMembersWithLimitPg tid n
  SelectTeamMembers tid uids -> teamMembersLimitedPg tid uids
  SelectTeamMemberInfos tid uids -> teamMemberInfosPg tid uids
  GetBillingTeamMembers tid -> map Id . V.toList <$> runStatement (toUUID tid) listBillingTeamMembersStatement
  GetTeamAdmins tid -> map Id . V.toList <$> runStatement (toUUID tid) listTeamAdminsStatement
  SelectTeams uid tids -> map Id . V.toList <$> runStatement (toUUID uid, V.fromList (map toUUID tids)) selectUserTeamsInStatement
  GetUserTeams uid -> map Id . V.toList <$> runStatement (toUUID uid) selectUserTeamsStatement
  GetUsersTeams uids -> usersTeamsPg uids
  GetOneUserTeam uid -> fmap (fmap Id) (runStatement (toUUID uid) selectOneUserTeamStatement)

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

--------------------------------------------------------------------------------
-- Team members (WPB-22957)

-- | A @team_member@ row minus the primary key: permissions (@smallint[]@
-- holding the self and copy masks), invited_by, invited_at, legalhold status.
type MemberColumns = (Vector Int16, Maybe UUID, Maybe UTCTime, Maybe Int16)

-- | Invited-by and invited-at are both-or-neither (enforced at read time, as
-- in Cassandra); rows with incomplete invitation metadata are dropped.
teamMemberFromRow :: UserId -> MemberColumns -> Maybe TeamMember
teamMemberFromRow uid (perms, minvu, minvt, mlh) = do
  perms' <- either (const Nothing) Just (postgresUnmarshall perms)
  mlh' <- traverse (either (const Nothing) Just . postgresUnmarshall @Int16) mlh
  let minvu' = Id <$> minvu
      minvt' = toUTCTimeMillis <$> minvt
  pure $ mkTeamMember uid perms' ((,) <$> minvu' <*> minvt') (fromMaybe defUserLegalHoldStatus mlh')

addTeamMemberPg :: (PGConstraints r) => TeamId -> TeamMember -> Sem r ()
addTeamMemberPg t m =
  runTransactionWithRetry ReadCommitted Write $ do
    Transaction.statement
      ( toUUID t,
        toUUID (m ^. userId),
        postgresMarshall (m ^. permissions),
        toUUID <$> (m ^? invitation . _Just . _1),
        fromUTCTimeMillis <$> (m ^? invitation . _Just . _2)
      )
      insertTeamMemberStatement
    Transaction.statement (toUUID (m ^. userId), toUUID t) insertUserTeamStatement
    when (m `hasPermission` SetBilling) $
      Transaction.statement (toUUID t, toUUID (m ^. userId)) insertBillingTeamMemberStatement
    when (isAdminOrOwner (m ^. permissions)) $
      Transaction.statement (toUUID t, toUUID (m ^. userId)) insertTeamAdminStatement

updateTeamMemberPg :: (PGConstraints r) => Permissions -> TeamId -> UserId -> Permissions -> Sem r ()
updateTeamMemberPg oldPerms tid uid newPerms = do
  let permDiff = Set.difference `on` self
      acquiredPerms = newPerms `permDiff` oldPerms
      lostPerms = oldPerms `permDiff` newPerms
  runTransactionWithRetry ReadCommitted Write $ do
    Transaction.statement (postgresMarshall newPerms, toUUID tid, toUUID uid) updatePermissionsStatement
    when (SetBilling `Set.member` acquiredPerms) $
      Transaction.statement (toUUID tid, toUUID uid) insertBillingTeamMemberStatement
    when (SetBilling `Set.member` lostPerms) $
      Transaction.statement (toUUID tid, toUUID uid) deleteBillingTeamMemberStatement
    when (isAdminOrOwner newPerms && not (isAdminOrOwner oldPerms)) $
      Transaction.statement (toUUID tid, toUUID uid) insertTeamAdminStatement
    when (isAdminOrOwner oldPerms && not (isAdminOrOwner newPerms)) $
      Transaction.statement (toUUID tid, toUUID uid) deleteTeamAdminStatement

-- | Mirrors the Cassandra logged batch: the member row and all auxiliary rows
-- (@user_team@, @billing_team_member@, @team_admin@) are removed atomically.
removeTeamMemberPg :: (PGConstraints r) => TeamId -> UserId -> Sem r ()
removeTeamMemberPg tid uid =
  runTransactionWithRetry ReadCommitted Write $ do
    Transaction.statement (toUUID tid, toUUID uid) deleteTeamMemberStatement
    Transaction.statement (toUUID uid, toUUID tid) deleteUserTeamStatement
    Transaction.statement (toUUID tid, toUUID uid) deleteBillingTeamMemberStatement
    Transaction.statement (toUUID tid, toUUID uid) deleteTeamAdminStatement

-- | Removes every member (and their auxiliary rows) of a team from Postgres;
-- the Postgres-side counterpart of the Cassandra member sweep that
-- 'DeleteTeam' triggers.
removeTeamMemberRowsPg :: (PGConstraints r) => TeamId -> Sem r ()
removeTeamMemberRowsPg tid =
  runTransactionWithRetry ReadCommitted Write $ do
    Transaction.statement (toUUID tid) deleteTeamMembersByTeamStatement
    Transaction.statement (toUUID tid) deleteUserTeamsByTeamStatement
    Transaction.statement (toUUID tid) deleteBillingTeamMembersByTeamStatement
    Transaction.statement (toUUID tid) deleteTeamAdminsByTeamStatement

teamMemberPg :: (PGConstraints r) => TeamId -> UserId -> Sem r (Maybe TeamMember)
teamMemberPg t u = do
  mrow <- runStatement (toUUID t, toUUID u) selectTeamMemberStatement
  pure (teamMemberFromRow u =<< mrow)

-- | One keyset-paginated page of a team's members, ordered by user.
teamMemberPage ::
  (PGConstraints r) =>
  TeamId ->
  Maybe UserId ->
  Range 1 HardTruncationLimit Int32 ->
  Sem r [(UserId, MemberColumns)]
teamMemberPage tid start maxMembers = do
  rows <- case start of
    Just u -> runStatement (toUUID tid, toUUID u, fromRange maxMembers) selectTeamMembersFromStatement
    Nothing -> runStatement (toUUID tid, fromRange maxMembers) selectTeamMembersStatement
  pure (map (\(u, p, i, t, l) -> (Id u, (p, i, t, l))) (V.toList rows))

teamMembersCollectedPg :: (PGConstraints r) => TeamId -> Sem r [TeamMember]
teamMembersCollectedPg tid = go Nothing []
  where
    go start acc = do
      page <- teamMemberPage tid start (unsafeRange 2000 :: Range 1 HardTruncationLimit Int32)
      let ms = mapMaybe (uncurry teamMemberFromRow) page
          acc' = acc ++ ms
      if length page < 2000
        then pure acc'
        else go (Just (fst (last page))) acc'

-- | Mirrors the Cassandra implementation: one extra row is fetched to decide
-- truncation.
teamMembersWithLimitPg :: (PGConstraints r) => TeamId -> Range 1 HardTruncationLimit Int32 -> Sem r TeamMemberList
teamMembersWithLimitPg t maxMembers = do
  page <- teamMemberPage t Nothing (unsafeRange (fromRange maxMembers + 1))
  let ms = mapMaybe (uncurry teamMemberFromRow) (take (fromIntegral (fromRange maxMembers)) page)
  pure $
    if length page > fromIntegral (fromRange maxMembers :: Int32)
      then newTeamMemberList ms ListTruncated
      else newTeamMemberList ms ListComplete

teamMembersLimitedPg :: (PGConstraints r) => TeamId -> [UserId] -> Sem r [TeamMember]
teamMembersLimitedPg t uids = do
  rows <- runStatement (toUUID t, V.fromList (map toUUID uids)) selectTeamMembersInStatement
  pure (mapMaybe (\(u, p, i, ti, l) -> teamMemberFromRow (Id u) (p, i, ti, l)) (V.toList rows))

-- | Mirrors the Cassandra @selectTeamMembers'@ row: the permissions write
-- time is not reproducible in Postgres and is taken from the current time.
teamMemberInfosPg :: (PGConstraints r) => TeamId -> [UserId] -> Sem r [Info.TeamMemberInfo]
teamMemberInfosPg t uids = do
  now <- embed @IO getCurrentTime
  rows <- runStatement (toUUID t, V.fromList (map toUUID uids)) selectTeamMembersInStatement
  pure $
    mapMaybe
      ( \(u, p, i, ti, l) ->
          teamMemberFromRow (Id u) (p, i, ti, l)
            <&> ( \m ->
                    Info.TeamMemberInfo
                      { Info.userId = m ^. userId,
                        Info.permissions = m ^. permissions,
                        Info.permissionsWriteTime = toUTCTimeMillis now
                      }
                )
      )
      (V.toList rows)

usersTeamsPg :: (PGConstraints r) => [UserId] -> Sem r (Map UserId TeamId)
usersTeamsPg uids = do
  rows <- runStatement (V.fromList (map toUUID uids)) selectOneUserTeamsStatement
  pure (Map.fromList (map (bimap Id Id) (V.toList rows)))

-- | One team per user (the first by team id), mirroring the Cassandra
-- @selectOneUserTeam@ semantics.
selectOneUserTeamsStatement :: Hasql.Statement (Vector UUID) (Vector (UUID, UUID))
selectOneUserTeamsStatement =
  [vectorStatement|
    select distinct on ("user") "user" :: uuid, team :: uuid
      from user_team
     where "user" = any($1 :: uuid[])
     order by "user", team
  |]

--------------------------------------------------------------------------------
-- team_member statements

selectTeamMemberStatement :: Hasql.Statement (UUID, UUID) (Maybe MemberColumns)
selectTeamMemberStatement =
  [maybeStatement|
    select perms :: smallint[], invited_by :: uuid?, invited_at :: timestamptz?, legalhold_status :: smallint?
      from team_member
     where team = $1 :: uuid and "user" = $2 :: uuid
  |]

selectTeamMembersStatement :: Hasql.Statement (UUID, Int32) (Vector (UUID, Vector Int16, Maybe UUID, Maybe UTCTime, Maybe Int16))
selectTeamMembersStatement =
  [vectorStatement|
    select "user" :: uuid, perms :: smallint[], invited_by :: uuid?, invited_at :: timestamptz?, legalhold_status :: smallint?
      from team_member
     where team = $1 :: uuid
     order by "user"
     limit $2 :: int
  |]

selectTeamMembersFromStatement :: Hasql.Statement (UUID, UUID, Int32) (Vector (UUID, Vector Int16, Maybe UUID, Maybe UTCTime, Maybe Int16))
selectTeamMembersFromStatement =
  [vectorStatement|
    select "user" :: uuid, perms :: smallint[], invited_by :: uuid?, invited_at :: timestamptz?, legalhold_status :: smallint?
      from team_member
     where team = $1 :: uuid and "user" > $2 :: uuid
     order by "user"
     limit $3 :: int
  |]

selectTeamMembersInStatement :: Hasql.Statement (UUID, Vector UUID) (Vector (UUID, Vector Int16, Maybe UUID, Maybe UTCTime, Maybe Int16))
selectTeamMembersInStatement =
  [vectorStatement|
    select "user" :: uuid, perms :: smallint[], invited_by :: uuid?, invited_at :: timestamptz?, legalhold_status :: smallint?
      from team_member
     where team = $1 :: uuid and "user" = any($2 :: uuid[])
     order by "user"
  |]

insertTeamMemberStatement :: Hasql.Statement (UUID, UUID, Vector Int16, Maybe UUID, Maybe UTCTime) ()
insertTeamMemberStatement =
  [resultlessStatement|
    insert into team_member (team, "user", perms, invited_by, invited_at)
    values ($1 :: uuid, $2 :: uuid, $3 :: smallint[], $4 :: uuid?, $5 :: timestamptz?)
    on conflict (team, "user") do update set
      perms = excluded.perms,
      invited_by = excluded.invited_by,
      invited_at = excluded.invited_at
  |]

updatePermissionsStatement :: Hasql.Statement (Vector Int16, UUID, UUID) ()
updatePermissionsStatement =
  [resultlessStatement|
    update team_member set perms = $1 :: smallint[] where team = $2 :: uuid and "user" = $3 :: uuid
  |]

deleteTeamMemberStatement :: Hasql.Statement (UUID, UUID) ()
deleteTeamMemberStatement =
  [resultlessStatement|
    delete from team_member where team = $1 :: uuid and "user" = $2 :: uuid
  |]

deleteTeamMembersByTeamStatement :: Hasql.Statement UUID ()
deleteTeamMembersByTeamStatement =
  [resultlessStatement|
    delete from team_member where team = $1 :: uuid
  |]

deleteUserTeamsByTeamStatement :: Hasql.Statement UUID ()
deleteUserTeamsByTeamStatement =
  [resultlessStatement|
    delete from user_team where team = $1 :: uuid
  |]

deleteBillingTeamMembersByTeamStatement :: Hasql.Statement UUID ()
deleteBillingTeamMembersByTeamStatement =
  [resultlessStatement|
    delete from billing_team_member where team = $1 :: uuid
  |]

deleteTeamAdminsByTeamStatement :: Hasql.Statement UUID ()
deleteTeamAdminsByTeamStatement =
  [resultlessStatement|
    delete from team_admin where team = $1 :: uuid
  |]

--------------------------------------------------------------------------------
-- user_team / billing_team_member / team_admin statements

insertUserTeamStatement :: Hasql.Statement (UUID, UUID) ()
insertUserTeamStatement =
  [resultlessStatement|
    insert into user_team ("user", team) values ($1 :: uuid, $2 :: uuid)
    on conflict ("user", team) do update set team = excluded.team
  |]

deleteUserTeamStatement :: Hasql.Statement (UUID, UUID) ()
deleteUserTeamStatement =
  [resultlessStatement|
    delete from user_team where "user" = $1 :: uuid and team = $2 :: uuid
  |]

selectUserTeamsStatement :: Hasql.Statement UUID (Vector UUID)
selectUserTeamsStatement =
  [vectorStatement|
    select team :: uuid from user_team where "user" = $1 :: uuid order by team
  |]

selectOneUserTeamStatement :: Hasql.Statement UUID (Maybe UUID)
selectOneUserTeamStatement =
  [maybeStatement|
    select team :: uuid from user_team where "user" = $1 :: uuid order by team limit 1
  |]

selectUserTeamsInStatement :: Hasql.Statement (UUID, Vector UUID) (Vector UUID)
selectUserTeamsInStatement =
  [vectorStatement|
    select team :: uuid from user_team where "user" = $1 :: uuid and team = any($2 :: uuid[]) order by team
  |]

insertBillingTeamMemberStatement :: Hasql.Statement (UUID, UUID) ()
insertBillingTeamMemberStatement =
  [resultlessStatement|
    insert into billing_team_member (team, "user") values ($1 :: uuid, $2 :: uuid)
    on conflict (team, "user") do update set "user" = excluded."user"
  |]

deleteBillingTeamMemberStatement :: Hasql.Statement (UUID, UUID) ()
deleteBillingTeamMemberStatement =
  [resultlessStatement|
    delete from billing_team_member where team = $1 :: uuid and "user" = $2 :: uuid
  |]

listBillingTeamMembersStatement :: Hasql.Statement UUID (Vector UUID)
listBillingTeamMembersStatement =
  [vectorStatement|
    select "user" :: uuid from billing_team_member where team = $1 :: uuid
  |]

insertTeamAdminStatement :: Hasql.Statement (UUID, UUID) ()
insertTeamAdminStatement =
  [resultlessStatement|
    insert into team_admin (team, "user") values ($1 :: uuid, $2 :: uuid)
    on conflict (team, "user") do update set "user" = excluded."user"
  |]

deleteTeamAdminStatement :: Hasql.Statement (UUID, UUID) ()
deleteTeamAdminStatement =
  [resultlessStatement|
    delete from team_admin where team = $1 :: uuid and "user" = $2 :: uuid
  |]

listTeamAdminsStatement :: Hasql.Statement UUID (Vector UUID)
listTeamAdminsStatement =
  [vectorStatement|
    select "user" :: uuid from team_admin where team = $1 :: uuid
  |]
