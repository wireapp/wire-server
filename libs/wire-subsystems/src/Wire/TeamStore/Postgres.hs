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
    userLegalHoldStatusToText,
  )
where

import Cassandra (ClientState)
import Control.Lens ((.~), (^.), (^?), _1, _2, _Just)
import Data.Id
import Data.Json.Util (UTCTimeMillis (..), toUTCTimeMillis)
import Data.LegalHold (UserLegalHoldStatus (..), defUserLegalHoldStatus)
import Data.Map.Strict qualified as Map
import Data.Range (Range, fromRange, unsafeRange)
import Data.Set qualified as Set
import Data.Time.Clock (UTCTime)
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
-- exactly like the Cassandra implementation does at creation time; the
-- maintained writetime columns (@status_time@, @binding_time@) default to
-- now() on insert and are refreshed on conflict.
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

userLegalHoldStatusToText :: UserLegalHoldStatus -> Text
userLegalHoldStatusToText = \case
  UserLegalHoldDisabled -> "disabled"
  UserLegalHoldPending -> "pending"
  UserLegalHoldEnabled -> "enabled"
  UserLegalHoldNoConsent -> "no_consent"

userLegalHoldStatusFromText :: Text -> Either Text UserLegalHoldStatus
userLegalHoldStatusFromText = \case
  "disabled" -> Right UserLegalHoldDisabled
  "pending" -> Right UserLegalHoldPending
  "enabled" -> Right UserLegalHoldEnabled
  "no_consent" -> Right UserLegalHoldNoConsent
  t -> Left ("invalid user legal hold status: " <> t)

permissionsToPair :: Permissions -> (Int64, Int64)
permissionsToPair = postgresMarshall

pairToPermissions :: (Int64, Int64) -> Either Text Permissions
pairToPermissions = postgresUnmarshall

insertTeamStatement :: Hasql.Statement (UUID, UUID, Text, Text, Maybe Text, Text, Text) ()
insertTeamStatement =
  [resultlessStatement|
    insert into team (team, creator, name, icon, icon_key, deleted, status, binding)
    values ($1 :: uuid, $2 :: uuid, $3 :: text, $4 :: text, $5 :: text?, false, $6 :: text, $7 :: text)
    on conflict (team) do update
      set creator = ($2 :: uuid),
          name = ($3 :: text),
          icon = ($4 :: text),
          icon_key = ($5 :: text?),
          status = ($6 :: text),
          status_time = now(),
          binding = ($7 :: text),
          binding_time = now()
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
             deleted :: bool, status :: text?, status_time :: timestamptz,
             binding :: text?, splash_screen :: text?
        from team
       where team = $1 :: uuid
    |]

toTeamData ::
  (UUID, UUID, Text, Text, Maybe Text, Bool, Maybe Text, UTCTime, Maybe Text, Maybe Text) ->
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
  pure (TeamData t (if deleted then PendingDelete else st) (Just statusTime))

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
  pure (toCreationTime <$> v)

toCreationTime :: UTCTime -> TeamCreationTime
toCreationTime t =
  TeamCreationTime (floor ((1e6 :: Double) * realToFrac (utcTimeToPOSIXSeconds t)))

-- | The @binding_time@ column carries over Cassandra's @writetime(binding)@,
-- which is microseconds since the epoch; it is maintained (NOT NULL with a
-- now() default), so a missing team row is the only 'Nothing' case.
selectTeamCreationTimeStatement :: Hasql.Statement UUID (Maybe UTCTime)
selectTeamCreationTimeStatement =
  [maybeStatement|
    select binding_time :: timestamptz from team where team = $1 :: uuid
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

-- | The metadata half of team deletion: resets name and icon to @default@ and
-- marks the team deleted (mirroring the Cassandra implementation).  Removing
-- the member rows is done by the Cassandra interpreter.
deleteTeamPg :: (PGConstraints r) => TeamId -> Sem r ()
deleteTeamPg tid =
  runStatement (postgresMarshall Deleted, toUUID tid) deleteTeamStatement

deleteTeamStatement :: Hasql.Statement (Text, UUID) ()
deleteTeamStatement =
  [resultlessStatement|
    update team set name = 'default', icon = 'default', status = $1 :: text, status_time = now() where team = $2 :: uuid
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

updateTeamStatusStatement :: Hasql.Statement (Text, UUID) ()
updateTeamStatusStatement =
  [resultlessStatement|
    update team set status = $1 :: text, status_time = now() where team = $2 :: uuid
  |]

setSearchVisibilityPg :: (PGConstraints r) => TeamId -> TeamSearchVisibility -> Sem r ()
setSearchVisibilityPg tid v =
  runStatement (postgresMarshall v, toUUID tid) setSearchVisibilityStatement

setSearchVisibilityStatement :: Hasql.Statement (Text, UUID) ()
setSearchVisibilityStatement =
  [resultlessStatement|
    update team set search_visibility = $1 :: text where team = $2 :: uuid
  |]

resetSearchVisibilityPg :: (PGConstraints r) => TeamId -> Sem r ()
resetSearchVisibilityPg tid = setSearchVisibilityPg tid SearchVisibilityStandard

--------------------------------------------------------------------------------
-- Team members (WPB-22957)

-- | A @team_member@ row minus the primary key: permissions (two int8 masks,
-- @self@ and @copy@, mirroring the Cassandra UDT), invited_by, invited_at,
-- legalhold status.
type MemberColumns = (Int64, Int64, Maybe UUID, Maybe UTCTime, Maybe Text)

-- | Invited-by and invited-at are both-or-neither (enforced at read time, as
-- in Cassandra); rows with incomplete invitation metadata are dropped.
teamMemberFromRow :: UserId -> MemberColumns -> Maybe TeamMember
teamMemberFromRow uid (ps, pc, minvu, minvt, mlh) = do
  perms' <- either (const Nothing) Just (pairToPermissions (ps, pc))
  mlh' <- traverse (either (const Nothing) Just . userLegalHoldStatusFromText) mlh
  let minvu' = Id <$> minvu
      minvt' = toUTCTimeMillis <$> minvt
  pure $ mkTeamMember uid perms' ((,) <$> minvu' <*> minvt') (fromMaybe defUserLegalHoldStatus mlh')

addTeamMemberPg :: (PGConstraints r) => TeamId -> TeamMember -> Sem r ()
addTeamMemberPg t m =
  runTransactionWithRetry ReadCommitted Write $ do
    let (ps, pc) = permissionsToPair (m ^. permissions)
    Transaction.statement
      ( toUUID t,
        toUUID (m ^. userId),
        ps,
        pc,
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
  let (nps, npc) = permissionsToPair newPerms
      permDiff = Set.difference `on` self
      acquiredPerms = newPerms `permDiff` oldPerms
      lostPerms = oldPerms `permDiff` newPerms
  runTransactionWithRetry ReadCommitted Write $ do
    Transaction.statement (nps, npc, toUUID tid, toUUID uid) updatePermissionsStatement
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
  pure (map (\(u, ps, pc, i, t, l) -> (Id u, (ps, pc, i, t, l))) (V.toList rows))

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
  pure (mapMaybe (\(u, ps, pc, i, ti, l, _) -> teamMemberFromRow (Id u) (ps, pc, i, ti, l)) (V.toList rows))

-- | Mirrors the Cassandra @selectTeamMembers'@ row: the permissions write
-- time comes from the maintained @perms_write_time@ column (Cassandra's
-- @writetime(perms)@ carried over by the copy loop, now() on direct writes).
teamMemberInfosPg :: (PGConstraints r) => TeamId -> [UserId] -> Sem r [Info.TeamMemberInfo]
teamMemberInfosPg t uids = do
  rows <- runStatement (toUUID t, V.fromList (map toUUID uids)) selectTeamMembersInStatement
  pure $
    mapMaybe
      ( \(u, ps, pc, i, ti, l, wt) ->
          teamMemberFromRow (Id u) (ps, pc, i, ti, l)
            <&> ( \m ->
                    Info.TeamMemberInfo
                      { Info.userId = m ^. userId,
                        Info.permissions = m ^. permissions,
                        Info.permissionsWriteTime = toUTCTimeMillis wt
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
    select distinct on (user_id) user_id :: uuid, team :: uuid
      from user_team
     where user_id = any($1 :: uuid[])
     order by user_id, team
  |]

--------------------------------------------------------------------------------
-- team_member statements

selectTeamMemberStatement :: Hasql.Statement (UUID, UUID) (Maybe MemberColumns)
selectTeamMemberStatement =
  [maybeStatement|
    select perms_self :: int8, perms_copy :: int8, invited_by :: uuid?, invited_at :: timestamptz?, legalhold_status :: text?
      from team_member
     where team = $1 :: uuid and user_id = $2 :: uuid
  |]

selectTeamMembersStatement :: Hasql.Statement (UUID, Int32) (Vector (UUID, Int64, Int64, Maybe UUID, Maybe UTCTime, Maybe Text))
selectTeamMembersStatement =
  [vectorStatement|
    select user_id :: uuid, perms_self :: int8, perms_copy :: int8, invited_by :: uuid?, invited_at :: timestamptz?, legalhold_status :: text?
      from team_member
     where team = $1 :: uuid
     order by user_id
     limit $2 :: int
  |]

selectTeamMembersFromStatement :: Hasql.Statement (UUID, UUID, Int32) (Vector (UUID, Int64, Int64, Maybe UUID, Maybe UTCTime, Maybe Text))
selectTeamMembersFromStatement =
  [vectorStatement|
    select user_id :: uuid, perms_self :: int8, perms_copy :: int8, invited_by :: uuid?, invited_at :: timestamptz?, legalhold_status :: text?
      from team_member
     where team = $1 :: uuid and user_id > $2 :: uuid
     order by user_id
     limit $3 :: int
  |]

selectTeamMembersInStatement :: Hasql.Statement (UUID, Vector UUID) (Vector (UUID, Int64, Int64, Maybe UUID, Maybe UTCTime, Maybe Text, UTCTime))
selectTeamMembersInStatement =
  [vectorStatement|
    select user_id :: uuid, perms_self :: int8, perms_copy :: int8, invited_by :: uuid?, invited_at :: timestamptz?, legalhold_status :: text?, perms_write_time :: timestamptz
      from team_member
     where team = $1 :: uuid and user_id = any($2 :: uuid[])
     order by user_id
  |]

insertTeamMemberStatement :: Hasql.Statement (UUID, UUID, Int64, Int64, Maybe UUID, Maybe UTCTime) ()
insertTeamMemberStatement =
  [resultlessStatement|
    insert into team_member (team, user_id, perms_self, perms_copy, invited_by, invited_at)
    values ($1 :: uuid, $2 :: uuid, $3 :: int8, $4 :: int8, $5 :: uuid?, $6 :: timestamptz?)
    on conflict (team, user_id) do update set
      perms_self = excluded.perms_self,
      perms_copy = excluded.perms_copy,
      invited_by = excluded.invited_by,
      invited_at = excluded.invited_at,
      perms_write_time = now()
  |]

updatePermissionsStatement :: Hasql.Statement (Int64, Int64, UUID, UUID) ()
updatePermissionsStatement =
  [resultlessStatement|
    update team_member set perms_self = $1 :: int8, perms_copy = $2 :: int8, perms_write_time = now() where team = $3 :: uuid and user_id = $4 :: uuid
  |]

deleteTeamMemberStatement :: Hasql.Statement (UUID, UUID) ()
deleteTeamMemberStatement =
  [resultlessStatement|
    delete from team_member where team = $1 :: uuid and user_id = $2 :: uuid
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
    insert into user_team (user_id, team) values ($1 :: uuid, $2 :: uuid)
    on conflict (user_id, team) do nothing
  |]

deleteUserTeamStatement :: Hasql.Statement (UUID, UUID) ()
deleteUserTeamStatement =
  [resultlessStatement|
    delete from user_team where user_id = $1 :: uuid and team = $2 :: uuid
  |]

selectUserTeamsStatement :: Hasql.Statement UUID (Vector UUID)
selectUserTeamsStatement =
  [vectorStatement|
    select team :: uuid from user_team where user_id = $1 :: uuid order by team
  |]

selectOneUserTeamStatement :: Hasql.Statement UUID (Maybe UUID)
selectOneUserTeamStatement =
  [maybeStatement|
    select team :: uuid from user_team where user_id = $1 :: uuid order by team limit 1
  |]

selectUserTeamsInStatement :: Hasql.Statement (UUID, Vector UUID) (Vector UUID)
selectUserTeamsInStatement =
  [vectorStatement|
    select team :: uuid from user_team where user_id = $1 :: uuid and team = any($2 :: uuid[]) order by team
  |]

insertBillingTeamMemberStatement :: Hasql.Statement (UUID, UUID) ()
insertBillingTeamMemberStatement =
  [resultlessStatement|
    insert into billing_team_member (team, user_id) values ($1 :: uuid, $2 :: uuid)
    on conflict (team, user_id) do nothing
  |]

deleteBillingTeamMemberStatement :: Hasql.Statement (UUID, UUID) ()
deleteBillingTeamMemberStatement =
  [resultlessStatement|
    delete from billing_team_member where team = $1 :: uuid and user_id = $2 :: uuid
  |]

listBillingTeamMembersStatement :: Hasql.Statement UUID (Vector UUID)
listBillingTeamMembersStatement =
  [vectorStatement|
    select user_id :: uuid from billing_team_member where team = $1 :: uuid
  |]

insertTeamAdminStatement :: Hasql.Statement (UUID, UUID) ()
insertTeamAdminStatement =
  [resultlessStatement|
    insert into team_admin (team, user_id) values ($1 :: uuid, $2 :: uuid)
    on conflict (team, user_id) do nothing
  |]

deleteTeamAdminStatement :: Hasql.Statement (UUID, UUID) ()
deleteTeamAdminStatement =
  [resultlessStatement|
    delete from team_admin where team = $1 :: uuid and user_id = $2 :: uuid
  |]

listTeamAdminsStatement :: Hasql.Statement UUID (Vector UUID)
listTeamAdminsStatement =
  [vectorStatement|
    select user_id :: uuid from team_admin where team = $1 :: uuid
  |]
