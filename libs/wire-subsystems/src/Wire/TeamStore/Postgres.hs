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
-- FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program.  If not, see <https://www.gnu.org/licenses/>.

-- | PostgreSQL interpreter for 'TeamStore'.
--
-- Mirrors the Cassandra interpreter (Wire.TeamStore.Cassandra) over the five
-- tables @team@, @team_member@, @user_team@, @billing_team_member@ and
-- @team_admin@ (see @postgres-migrations/20261021175638-team-store.sql@).
--
-- Every column whose Cassandra writetime is consumed is mirrored by a nullable
-- @*_time@ column holding that writetime: @team.status_time@ (from
-- @writetime(status)@) and @team.binding_time@ (from @writetime(binding)@) are
-- carried over by the copy loop (NULL when Cassandra has no writetime), and
-- maintained by the dual-write leg via now().  They are served to
-- @TeamData.tdStatusTime@ and 'TeamCreationTime' respectively; like Cassandra,
-- absent writetimes are served as absent, never fabricated.
--
-- Rows that fail to decode are dropped silently (absent) instead of failing the
-- request — rows are only written through this interpreter or the migration
-- copy-loop, so a decode failure indicates schema drift.
module Wire.TeamStore.Postgres
  ( interpretTeamStoreToPostgres,
    deleteTeamStore,
    userLegalHoldStatusToText,
    teamMemberPage,
    migrateTeamRow,
    migrateTeamMemberRow,
    migrateUserTeamRow,
    migrateBillingTeamMemberRow,
    migrateTeamAdminRow,
    RawTeamMemberRow,
    rawTeamMemberRowToTeamMember,
    TeamRow (..),
  )
where

import Control.Lens ((.~), (^.), (^?), _1, _2, _Just)
import Data.Id
import Data.Json.Util (UTCTimeMillis (..), toUTCTimeMillis)
import Data.LegalHold
  ( UserLegalHoldStatus (..),
    defUserLegalHoldStatus,
  )
import Data.Map.Strict qualified as Map
import Data.Range
import Data.Set qualified as Set
import Data.Time.Clock
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Hasql.Transaction qualified as Transaction
import Hasql.Transaction.Sessions (IsolationLevel (ReadCommitted), Mode (Write))
import Imports
import Polysemy
import Polysemy.TinyLog
import Wire.API.PostgresMarshall (PostgresMarshall (..), PostgresUnmarshall (..))
import Wire.API.Routes.Internal.Galley.TeamsIntra
import Wire.API.Team
import Wire.API.Team.Member
import Wire.API.Team.Member.Info qualified as Info
import Wire.API.Team.Permission (Perm (SetBilling), Permissions, self)
import Wire.API.Team.SearchVisibility
import Wire.ConversationStore (ConversationStore)
import Wire.ConversationStore qualified as E
import Wire.Postgres
import Wire.TeamStore (TeamStore (..))
import Wire.Util (logEffect)

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

iconToText :: Icon -> Text
iconToText = postgresMarshall

iconFromText :: Text -> Either Text Icon
iconFromText = postgresUnmarshall

--------------------------------------------------------------------------------
-- interpreter

interpretTeamStoreToPostgres ::
  ( PGConstraints r,
    Member TinyLog r,
    Member ConversationStore r
  ) =>
  Sem (TeamStore ': r) a ->
  Sem r a
interpretTeamStoreToPostgres = interpret $ \case
  CreateTeamMember tid mem -> do
    logEffect "TeamStore.CreateTeamMember"
    addTeamMember tid mem
  SetTeamMemberPermissions perm0 tid uid perm1 -> do
    logEffect "TeamStore.SetTeamMemberPermissions"
    updateTeamMember perm0 tid uid perm1
  CreateTeam t uid n i k b -> do
    logEffect "TeamStore.CreateTeam"
    createTeam t uid n i k b
  DeleteTeamMember tid uid -> do
    logEffect "TeamStore.DeleteTeamMember"
    removeTeamMember tid uid
  GetBillingTeamMembers tid -> do
    logEffect "TeamStore.GetBillingTeamMembers"
    map Id . V.toList <$> runStatement (toUUID tid) listBillingTeamMembersStatement
  GetTeamAdmins tid -> do
    logEffect "TeamStore.GetTeamAdmins"
    map Id . V.toList <$> runStatement (toUUID tid) listTeamAdminsStatement
  GetTeam tid -> do
    logEffect "TeamStore.GetTeam"
    getTeam tid
  GetTeamName tid -> do
    logEffect "TeamStore.GetTeamName"
    runStatement (toUUID tid) selectTeamNameStatement
  SelectTeams uid tids -> do
    logEffect "TeamStore.SelectTeams"
    teamIdsOf uid tids
  GetTeamMember tid uid -> do
    logEffect "TeamStore.GetTeamMember"
    teamMember tid uid
  GetTeamMembers tid -> do
    logEffect "TeamStore.GetTeamMembers"
    teamMembersCollected tid
  GetTeamMembersWithLimit tid n -> do
    logEffect "TeamStore.GetTeamMembersWithLimit"
    teamMembersWithLimit tid n
  SelectTeamMembers tid uids -> do
    logEffect "TeamStore.SelectTeamMembers"
    teamMembersLimited tid uids
  SelectTeamMemberInfos tid uids -> do
    logEffect "TeamStore.SelectTeamMemberInfos"
    teamMemberInfos tid uids
  GetUserTeams uid -> do
    logEffect "TeamStore.GetUserTeams"
    map Id . V.toList <$> runStatement (toUUID uid) selectUserTeamsStatement
  GetUsersTeams uids -> do
    logEffect "TeamStore.GetUsersTeams"
    usersTeams uids
  GetOneUserTeam uid -> do
    logEffect "TeamStore.GetOneUserTeam"
    fmap Id <$> runStatement (toUUID uid) selectOneUserTeamStatement
  GetTeamsBindings tids -> do
    logEffect "TeamStore.GetTeamsBindings"
    rows <- runStatement (V.fromList (toUUID <$> tids)) selectTeamBindingsInStatement
    -- Preserve the input order and the Cassandra semantics: teams without a
    -- row are skipped, teams with a NULL binding count as non-binding.
    let byId = Map.fromList (V.toList rows)
    pure $
      mapMaybe
        ( \tid -> do
            t <- Map.lookup (toUUID tid) byId
            either (const Nothing) Just (teamBindingFromText t)
        )
        tids
  GetTeamBinding tid -> do
    logEffect "TeamStore.GetTeamBinding"
    getTeamBinding tid
  GetTeamCreationTime tid -> do
    logEffect "TeamStore.GetTeamCreationTime"
    mres <- runStatement (toUUID tid) selectTeamCreationTimeStatement
    -- Mirrors the Cassandra read: Nothing when the row is absent or when
    -- Cassandra had no writetime(binding) (NULL binding_time) — never
    -- fabricated.
    pure $
      do
        mt <- mres
        t <- mt
        pure (TeamCreationTime (round (utcTimeToPOSIXSeconds t * 1_000_000)))
  DeleteTeam tid -> do
    logEffect "TeamStore.DeleteTeam"
    deleteTeam tid
  SetTeamData tid upd -> do
    logEffect "TeamStore.SetTeamData"
    updateTeam tid upd
  SetTeamStatus tid st -> do
    logEffect "TeamStore.SetTeamStatus"
    runStatement (teamStatusToText st, toUUID tid) updateTeamStatusStatement
  GetSearchVisibility tid -> do
    logEffect "TeamStore.GetSearchVisibility"
    getSearchVisibility tid
  SetSearchVisibility tid value -> do
    logEffect "TeamStore.SetSearchVisibility"
    runStatement (teamSearchVisibilityToText value, toUUID tid) updateSearchVisibilityStatement
  ResetSearchVisibility tid -> do
    logEffect "TeamStore.ResetSearchVisibility"
    runStatement (teamSearchVisibilityToText SearchVisibilityStandard, toUUID tid) updateSearchVisibilityStatement

--------------------------------------------------------------------------------
-- shared conversions

teamStatusToText :: TeamStatus -> Text
teamStatusToText = postgresMarshall

teamStatusFromText :: Text -> Either Text TeamStatus
teamStatusFromText = postgresUnmarshall

teamBindingToText :: TeamBinding -> Text
teamBindingToText = postgresMarshall

teamBindingFromText :: Text -> Either Text TeamBinding
teamBindingFromText = postgresUnmarshall

teamSearchVisibilityToText :: TeamSearchVisibility -> Text
teamSearchVisibilityToText = postgresMarshall

teamSearchVisibilityFromText :: Text -> Either Text TeamSearchVisibility
teamSearchVisibilityFromText = postgresUnmarshall

--------------------------------------------------------------------------------
-- team

createTeam ::
  (PGConstraints r) =>
  Maybe TeamId ->
  UserId ->
  Range 1 256 Text ->
  Icon ->
  Maybe (Range 1 256 Text) ->
  TeamBinding ->
  Sem r Team
createTeam mtid uid n i k b = do
  tid <- embed @IO (maybe (Id <$> liftIO nextRandom) pure mtid)
  runStatement
    ( toUUID tid,
      toUUID uid,
      fromRange n,
      iconToText i,
      fromRange <$> k,
      teamStatusToText (initialStatus b),
      teamBindingToText b
    )
    insertTeamStatement
  pure (newTeam tid uid (fromRange n) i b & teamIconKey .~ (fromRange <$> k))
  where
    initialStatus Binding = PendingActive
    initialStatus NonBinding = Active

-- | The @team@ row as decoded from Postgres. The boolean is the legacy
-- @deleted@ flag (only ever false on live rows).
data TeamRow = TeamRow
  { trCreator :: UUID,
    trBindingTime :: Maybe UTCTime,
    trName :: Text,
    trIcon :: Text,
    trIconKey :: Maybe Text,
    trDeleted :: Bool,
    trStatus :: Maybe Text,
    trStatusTime :: Maybe UTCTime,
    trBinding :: Maybe Text,
    trSplashScreen :: Maybe Text,
    trSearchVisibility :: Maybe Text
  }

toTeamData :: UUID -> TeamRow -> Either Text TeamData
toTeamData tid tr = do
  statusTxt <- maybe (Left "team row: missing status") Right tr.trStatus
  status <- teamStatusFromText statusTxt
  bindingTxt <- maybe (Left "team row: missing binding") Right tr.trBinding
  binding <- teamBindingFromText bindingTxt
  icon <- iconFromText tr.trIcon
  splash <- traverse iconFromText tr.trSplashScreen
  let team =
        newTeam (Id tid) (Id tr.trCreator) tr.trName icon binding
          & teamId .~ Id tid
          & teamIconKey .~ tr.trIconKey
          & teamSplashScreen .~ fromMaybe DefaultIcon splash
      -- The legacy @deleted@ flag is only ever true on rows whose Cassandra
      -- delete rewrote the status; surface such rows as PendingDelete so the
      -- requeue scan keeps retrying their deletion.
      status' = if tr.trDeleted then PendingDelete else status
  pure (TeamData team status' tr.trStatusTime)

getTeam :: (PGConstraints r) => TeamId -> Sem r (Maybe TeamData)
getTeam tid = do
  mrow <- runStatement (toUUID tid) selectTeamStatement
  pure $ case mrow of
    Just (creator, name, icon, iconKey, deleted, status, statusTime, bindingTime, binding, splashScreen, searchVisibility) ->
      either (const Nothing) Just $
        toTeamData
          (toUUID tid)
          TeamRow
            { trCreator = creator,
              trBindingTime = bindingTime,
              trName = name,
              trIcon = icon,
              trIconKey = iconKey,
              trDeleted = deleted,
              trStatus = status,
              trStatusTime = statusTime,
              trBinding = binding,
              trSplashScreen = splashScreen,
              trSearchVisibility = searchVisibility
            }
    Nothing -> Nothing

updateTeam :: (PGConstraints r) => TeamId -> TeamUpdateData -> Sem r ()
updateTeam tid u =
  runTransactionWithRetry ReadCommitted Write $ do
    for_ (u ^. nameUpdate) $ \n ->
      Transaction.statement (fromRange n, toUUID tid) updateTeamNameStatement
    for_ (u ^. iconUpdate) $ \i ->
      Transaction.statement (iconToText i, toUUID tid) updateTeamIconStatement
    for_ (u ^. iconKeyUpdate) $ \k ->
      Transaction.statement (fromRange k, toUUID tid) updateTeamIconKeyStatement
    for_ (u ^. splashScreenUpdate) $ \ss ->
      Transaction.statement (iconToText ss, toUUID tid) updateTeamSplashScreenStatement

getSearchVisibility :: (PGConstraints r) => TeamId -> Sem r TeamSearchVisibility
getSearchVisibility tid = do
  mres <- runStatement (toUUID tid) selectSearchVisibilityStatement
  let mv = join mres
  pure $ fromMaybe SearchVisibilityStandard (mv >>= either (const Nothing) Just . teamSearchVisibilityFromText)

-- | The team-table part of 'DeleteTeam' (no conversation deletion); shared by
-- the DualWrite interpreter, which must not delete conversations twice.
deleteTeamStore :: (PGConstraints r) => TeamId -> Sem r ()
deleteTeamStore tid = do
  markTeamDeletedAndRemoveTeamMembers tid
  runStatement (teamStatusToText Deleted, toUUID tid) deleteTeamStatement

deleteTeam :: (PGConstraints r, Member ConversationStore r) => TeamId -> Sem r ()
deleteTeam tid = do
  deleteTeamStore tid
  E.deleteTeamConversations tid

markTeamDeletedAndRemoveTeamMembers :: (PGConstraints r) => TeamId -> Sem r ()
markTeamDeletedAndRemoveTeamMembers tid = do
  runStatement (teamStatusToText PendingDelete, toUUID tid) markTeamDeletedStatement
  -- Set-based removal of the member row and all auxiliary rows: one
  -- transaction instead of a per-member round trip per page.
  runTransactionWithRetry ReadCommitted Write $ do
    Transaction.statement (toUUID tid) deleteTeamMembersByTeamStatement
    Transaction.statement (toUUID tid) deleteUserTeamsByTeamStatement
    Transaction.statement (toUUID tid) deleteBillingTeamMembersByTeamStatement
    Transaction.statement (toUUID tid) deleteTeamAdminsByTeamStatement

--------------------------------------------------------------------------------
-- team_member

utcmToUTC :: UTCTimeMillis -> UTCTime
utcmToUTC = fromUTCTimeMillis

-- | Raw team_member row as stored: user, perms (int8 pair), invited_by,
-- invited_at, legalhold status, perms write time.
type RawTeamMemberRow =
  ( UUID,
    Int64,
    Int64,
    Maybe UUID,
    Maybe UTCTime,
    Maybe Text,
    UTCTime
  )

userIdOf :: RawTeamMemberRow -> UserId
userIdOf (u, _, _, _, _, _, _) = Id u

data RawTeamMember = RawTeamMember
  { rtmUser :: UserId,
    rtmPermissions :: Permissions,
    rtmInvitedBy :: Maybe UserId,
    rtmInvitedAt :: Maybe UTCTimeMillis,
    rtmLegalHoldStatus :: Maybe UserLegalHoldStatus,
    -- | Cassandra's @writetime(perms)@ carried in the maintained @perms_write_time@
    -- column; served as 'Info.permissionsWriteTime'.
    rtmPermsTime :: UTCTime
  }

decodeRawTeamMemberRow :: RawTeamMemberRow -> Either Text RawTeamMember
decodeRawTeamMemberRow (u, permsSelf, permsCopy, minvu, minvt, mlh, permsTime) = do
  perms' <- pairToPermissions (permsSelf, permsCopy)
  mlh' <- traverse userLegalHoldStatusFromText mlh
  pure (RawTeamMember (Id u) perms' (Id <$> minvu) (toUTCTimeMillis <$> minvt) mlh' permsTime)

rawTeamMemberRowToTeamMember :: RawTeamMemberRow -> Maybe TeamMember
rawTeamMemberRowToTeamMember row = do
  rtm <- either (const Nothing) Just (decodeRawTeamMemberRow row)
  pure $
    mkTeamMember
      rtm.rtmUser
      rtm.rtmPermissions
      ((,) <$> rtm.rtmInvitedBy <*> rtm.rtmInvitedAt)
      (fromMaybe defUserLegalHoldStatus rtm.rtmLegalHoldStatus)

addTeamMember :: (PGConstraints r) => TeamId -> TeamMember -> Sem r ()
addTeamMember t m = do
  let (ps, pc) = permissionsToPair (m ^. permissions)
  runTransactionWithRetry ReadCommitted Write $ do
    Transaction.statement
      ( toUUID t,
        toUUID (m ^. userId),
        ps,
        pc,
        toUUID <$> (m ^? invitation . _Just . _1),
        fmap utcmToUTC (m ^? invitation . _Just . _2)
      )
      insertTeamMemberStatement
    Transaction.statement (toUUID (m ^. userId), toUUID t) insertUserTeamStatement
    when (m `hasPermission` SetBilling) $
      Transaction.statement (toUUID t, toUUID (m ^. userId)) insertBillingTeamMemberStatement
    when (isAdminOrOwner (m ^. permissions)) $
      Transaction.statement (toUUID t, toUUID (m ^. userId)) insertTeamAdminStatement

updateTeamMember :: (PGConstraints r) => Permissions -> TeamId -> UserId -> Permissions -> Sem r ()
updateTeamMember oldPerms tid uid newPerms = do
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

-- | Mirrors the Cassandra batch: the member row and all auxiliary rows
-- (user_team, billing_team_member, team_admin) are removed unconditionally.
-- | Mirrors the Cassandra batch: the member row and all auxiliary rows
-- (user_team, billing_team_member, team_admin) are removed unconditionally.
-- | Mirrors the Cassandra logged batch: the member row and all auxiliary rows
-- (user_team, billing_team_member, team_admin) are removed atomically.
removeTeamMember :: (PGConstraints r) => TeamId -> UserId -> Sem r ()
removeTeamMember tid uid =
  runTransactionWithRetry ReadCommitted Write $ do
    Transaction.statement (toUUID tid, toUUID uid) deleteTeamMemberStatement
    Transaction.statement (toUUID uid, toUUID tid) deleteUserTeamStatement
    Transaction.statement (toUUID tid, toUUID uid) deleteBillingTeamMemberStatement
    Transaction.statement (toUUID tid, toUUID uid) deleteTeamAdminStatement

teamMember :: (PGConstraints r) => TeamId -> UserId -> Sem r (Maybe TeamMember)
teamMember t u = do
  mres <- runStatement (toUUID t, toUUID u) selectTeamMemberStatement
  pure $ case mres of
    Just (ps, pc, minvu, minvt, mlh, permsTime) ->
      rawTeamMemberRowToTeamMember (toUUID u, ps, pc, minvu, minvt, mlh, permsTime)
    Nothing -> Nothing

-- | One page of a team's members, keyset-paginated by user_id.
teamMemberPage :: (PGConstraints r) => TeamId -> Maybe UserId -> Range 1 HardTruncationLimit Int32 -> Sem r [RawTeamMemberRow]
teamMemberPage tid start maxMembers = do
  rows <- case start of
    Just u ->
      runStatement (toUUID tid, toUUID u, fromRange maxMembers) selectTeamMembersFromStatement
    Nothing ->
      runStatement (toUUID tid, fromRange maxMembers) selectTeamMembersStatement
  pure (V.toList rows)

teamMembersCollected :: (PGConstraints r) => TeamId -> Sem r [TeamMember]
teamMembersCollected tid = go Nothing []
  where
    go start acc = do
      page <- teamMemberPage tid start (unsafeRange 2000 :: Range 1 HardTruncationLimit Int32)
      let ms = mapMaybe rawTeamMemberRowToTeamMember page
          acc' = acc ++ ms
      if length page < 2000
        then pure acc'
        else go (Just (userIdOf (last page))) acc'

teamMembersWithLimit :: (PGConstraints r) => TeamId -> Range 1 HardTruncationLimit Int32 -> Sem r TeamMemberList
teamMembersWithLimit t maxMembers = do
  -- Fetch one extra row to detect truncation, mirroring the Cassandra
  -- implementation (limit + 1, then take limit).
  page <- teamMemberPage t Nothing (unsafeRange (fromRange maxMembers + 1))
  let ms = mapMaybe rawTeamMemberRowToTeamMember (take (fromIntegral (fromRange maxMembers)) page)
  pure $
    if length page > fromIntegral (fromRange maxMembers)
      then newTeamMemberList ms ListTruncated
      else newTeamMemberList ms ListComplete

teamMembersLimited :: (PGConstraints r) => TeamId -> [UserId] -> Sem r [TeamMember]
teamMembersLimited t u = do
  rows <- runStatement (toUUID t, V.fromList (toUUID <$> u)) selectTeamMembersInStatement
  pure (mapMaybe rawTeamMemberRowToTeamMember (V.toList rows))

-- | Mirrors the Cassandra @selectTeamMembers'@ row: the permissions write time
-- is Cassandra's @writetime(perms)@, carried over into the @perms_write_time@
-- column by the copy loop and maintained by the dual-write leg via now().
teamMemberInfos :: (PGConstraints r) => TeamId -> [UserId] -> Sem r [Info.TeamMemberInfo]
teamMemberInfos t u = do
  rows <- runStatement (toUUID t, V.fromList (toUUID <$> u)) selectTeamMembersInStatement
  pure $
    mapMaybe
      ( \row -> do
          rtm <- either (const Nothing) Just (decodeRawTeamMemberRow row)
          pure $
            Info.TeamMemberInfo
              { Info.userId = rtm.rtmUser,
                Info.permissions = rtm.rtmPermissions,
                Info.permissionsWriteTime = toUTCTimeMillis rtm.rtmPermsTime
              }
      )
      (V.toList rows)

--------------------------------------------------------------------------------
-- user_team

teamIdsOf :: (PGConstraints r) => UserId -> [TeamId] -> Sem r [TeamId]
teamIdsOf uid tids = map Id . V.toList <$> runStatement (toUUID uid, V.fromList (toUUID <$> tids)) selectUserTeamsInStatement

usersTeams :: (PGConstraints r) => [UserId] -> Sem r (Map UserId TeamId)
usersTeams uids = do
  pairs <-
    V.toList
      <$> runStatement (V.fromList (toUUID <$> uids)) selectUserTeamsForUsersStatement
  pure $
    foldl'
      (\m (uid, tid) -> Map.insert (Id uid) (Id tid) m)
      Map.empty
      pairs

--------------------------------------------------------------------------------
-- migration row loaders (shared with Wire.TeamStore.Migration)

-- | Upsert one @team@ row (copy-loop path: full row, no auxiliary rows).
-- The Cassandra writetimes carried in 'trStatusTime'/'trBindingTime' are
-- stored in the maintained @status_time@/@binding_time@ columns (NULL when
-- Cassandra has no writetime); the insert is deliberately do-nothing on
-- conflict so a stale snapshot can never clobber a newer dual-written row.
migrateTeamRow :: (PGConstraints r) => TeamRow -> UUID -> Sem r ()
migrateTeamRow tr tid =
  runStatement
    ( tid,
      tr.trCreator,
      tr.trName,
      tr.trIcon,
      tr.trIconKey,
      fromMaybe "active" tr.trStatus,
      tr.trStatusTime,
      fromMaybe "non_binding" tr.trBinding,
      tr.trBindingTime,
      tr.trSplashScreen,
      tr.trSearchVisibility
    )
    migrateTeamStatement

migrateTeamMemberRow :: (PGConstraints r) => UUID -> RawTeamMemberRow -> Sem r ()
migrateTeamMemberRow tid (u, ps, pc, minvu, minvt, mlh, permsTime) =
  runStatement
    ( tid,
      u,
      ps,
      pc,
      minvu,
      minvt,
      mlh,
      permsTime
    )
    migrateTeamMemberStatement

migrateUserTeamRow :: (PGConstraints r) => (UUID, UUID) -> Sem r ()
migrateUserTeamRow (u, t) = runStatement (u, t) insertUserTeamStatement

migrateBillingTeamMemberRow :: (PGConstraints r) => (UUID, UUID) -> Sem r ()
migrateBillingTeamMemberRow (t, u) = runStatement (t, u) insertBillingTeamMemberStatement

migrateTeamAdminRow :: (PGConstraints r) => (UUID, UUID) -> Sem r ()
migrateTeamAdminRow (t, u) = runStatement (t, u) insertTeamAdminStatement

--------------------------------------------------------------------------------
-- statements

selectTeamStatement ::
  Hasql.Statement
    UUID
    (Maybe (UUID, Text, Text, Maybe Text, Bool, Maybe Text, Maybe UTCTime, Maybe UTCTime, Maybe Text, Maybe Text, Maybe Text))
selectTeamStatement =
  [maybeStatement|
    select creator :: uuid, name :: text, icon :: text, icon_key :: text?, deleted :: bool,
           status :: text?, status_time :: timestamptz?, binding_time :: timestamptz?, binding :: text?,
           splash_screen :: text?, search_visibility :: text?
      from team
     where team = $1 :: uuid
  |]

selectTeamNameStatement :: Hasql.Statement UUID (Maybe Text)
selectTeamNameStatement =
  [maybeStatement|
    select name :: text from team where team = $1 :: uuid
  |]

selectTeamBindingStatement :: Hasql.Statement UUID (Maybe Text)
selectTeamBindingStatement =
  [maybeStatement|
    select binding :: text from team where team = $1 :: uuid
  |]

getTeamBinding :: (PGConstraints r) => TeamId -> Sem r (Maybe TeamBinding)
getTeamBinding tid = do
  mres <- runStatement (toUUID tid) selectTeamBindingStatement
  pure $ case mres of
    Just t -> either (const Nothing) Just (teamBindingFromText t)
    Nothing -> Nothing

selectTeamCreationTimeStatement :: Hasql.Statement UUID (Maybe (Maybe UTCTime))
selectTeamCreationTimeStatement =
  [maybeStatement|
    select binding_time :: timestamptz? from team where team = $1 :: uuid
  |]

selectSearchVisibilityStatement :: Hasql.Statement UUID (Maybe (Maybe Text))
selectSearchVisibilityStatement =
  [maybeStatement|
    select search_visibility :: text? from team where team = $1 :: uuid
  |]

insertTeamStatement ::
  Hasql.Statement
    (UUID, UUID, Text, Text, Maybe Text, Text, Text)
    ()
insertTeamStatement =
  [resultlessStatement|
    insert into team (team, creator, name, icon, icon_key, deleted, status, binding, status_time, binding_time)
    values ($1 :: uuid, $2 :: uuid, $3 :: text, $4 :: text, $5 :: text?, false, $6 :: text, $7 :: text, now(), now())
    on conflict (team) do update
      set creator = ($2 :: uuid),
          name = ($3 :: text),
          icon = ($4 :: text),
          icon_key = ($5 :: text?),
          status = ($6 :: text),
          binding = ($7 :: text),
          status_time = now(),
          binding_time = now()
  |]

updateTeamNameStatement :: Hasql.Statement (Text, UUID) ()
updateTeamNameStatement =
  [resultlessStatement|
    update team set name = ($1 :: text) where team = ($2 :: uuid)
  |]

updateTeamIconStatement :: Hasql.Statement (Text, UUID) ()
updateTeamIconStatement =
  [resultlessStatement|
    update team set icon = ($1 :: text) where team = ($2 :: uuid)
  |]

updateTeamIconKeyStatement :: Hasql.Statement (Text, UUID) ()
updateTeamIconKeyStatement =
  [resultlessStatement|
    update team set icon_key = ($1 :: text) where team = ($2 :: uuid)
  |]

updateTeamSplashScreenStatement :: Hasql.Statement (Text, UUID) ()
updateTeamSplashScreenStatement =
  [resultlessStatement|
    update team set splash_screen = ($1 :: text) where team = ($2 :: uuid)
  |]

updateTeamStatusStatement :: Hasql.Statement (Text, UUID) ()
updateTeamStatusStatement =
  [resultlessStatement|
    update team set status = ($1 :: text), status_time = now() where team = ($2 :: uuid)
  |]

markTeamDeletedStatement :: Hasql.Statement (Text, UUID) ()
markTeamDeletedStatement =
  [resultlessStatement|
    update team set status = ($1 :: text), status_time = now() where team = ($2 :: uuid)
  |]

-- | Mirrors the Cassandra @deleteTeam@ write (name/icon reset + Deleted
-- status); the far-future tombstone timestamp has no Postgres equivalent.
deleteTeamStatement :: Hasql.Statement (Text, UUID) ()
deleteTeamStatement =
  [resultlessStatement|
    update team
       set name = 'default', icon = 'default', status = ($1 :: text), status_time = now()
     where team = ($2 :: uuid)
  |]

updateSearchVisibilityStatement :: Hasql.Statement (Text, UUID) ()
updateSearchVisibilityStatement =
  [resultlessStatement|
    update team set search_visibility = ($1 :: text) where team = ($2 :: uuid)
  |]

selectTeamMemberStatement ::
  Hasql.Statement
    (UUID, UUID)
    (Maybe (Int64, Int64, Maybe UUID, Maybe UTCTime, Maybe Text, UTCTime))
selectTeamMemberStatement =
  [maybeStatement|
    select perms_self :: int8, perms_copy :: int8, invited_by :: uuid?,
           invited_at :: timestamptz?, legalhold_status :: text?, perms_write_time :: timestamptz
      from team_member
     where team = $1 :: uuid and user_id = $2 :: uuid
  |]

selectTeamMembersStatement ::
  Hasql.Statement (UUID, Int32) (Vector RawTeamMemberRow)
selectTeamMembersStatement =
  [vectorStatement|
    select user_id :: uuid, perms_self :: int8, perms_copy :: int8, invited_by :: uuid?,
           invited_at :: timestamptz?, legalhold_status :: text?, perms_write_time :: timestamptz
      from team_member
     where team = $1 :: uuid
     order by user_id
     limit $2 :: int4
  |]

selectTeamMembersFromStatement ::
  Hasql.Statement (UUID, UUID, Int32) (Vector RawTeamMemberRow)
selectTeamMembersFromStatement =
  [vectorStatement|
    select user_id :: uuid, perms_self :: int8, perms_copy :: int8, invited_by :: uuid?,
           invited_at :: timestamptz?, legalhold_status :: text?, perms_write_time :: timestamptz
      from team_member
     where team = $1 :: uuid and user_id > $2 :: uuid
     order by user_id
     limit $3 :: int4
  |]

selectTeamMembersInStatement ::
  Hasql.Statement (UUID, Vector UUID) (Vector RawTeamMemberRow)
selectTeamMembersInStatement =
  [vectorStatement|
    select user_id :: uuid, perms_self :: int8, perms_copy :: int8, invited_by :: uuid?,
           invited_at :: timestamptz?, legalhold_status :: text?, perms_write_time :: timestamptz
      from team_member
     where team = $1 :: uuid and user_id = any ($2 :: uuid[])
     order by user_id
  |]

insertTeamMemberStatement ::
  Hasql.Statement
    (UUID, UUID, Int64, Int64, Maybe UUID, Maybe UTCTime)
    ()
insertTeamMemberStatement =
  [resultlessStatement|
    insert into team_member (team, user_id, perms_self, perms_copy, invited_by, invited_at, perms_write_time)
    values ($1 :: uuid, $2 :: uuid, $3 :: int8, $4 :: int8, $5 :: uuid?, $6 :: timestamptz?, now())
    on conflict (team, user_id) do update
      set perms_self = ($3 :: int8),
          perms_copy = ($4 :: int8),
          invited_by = ($5 :: uuid?),
          invited_at = ($6 :: timestamptz?),
          perms_write_time = now()
  |]

updatePermissionsStatement :: Hasql.Statement (Int64, Int64, UUID, UUID) ()
updatePermissionsStatement =
  [resultlessStatement|
    update team_member
       set perms_self = ($1 :: int8), perms_copy = ($2 :: int8), perms_write_time = now()
     where team = ($3 :: uuid) and user_id = ($4 :: uuid)
  |]

deleteTeamMemberStatement :: Hasql.Statement (UUID, UUID) ()
deleteTeamMemberStatement =
  [resultlessStatement|
    delete from team_member where team = $1 :: uuid and user_id = $2 :: uuid
  |]

-- | Set-based deletes used by 'markTeamDeletedAndRemoveTeamMembers'.
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

-- | Batched @usersTeams@: one round trip for many users.
selectUserTeamsForUsersStatement :: Hasql.Statement (Vector UUID) (Vector (UUID, UUID))
selectUserTeamsForUsersStatement =
  [vectorStatement|
    select user_id :: uuid, team :: uuid from user_team where user_id = any ($1 :: uuid[]) order by user_id, team
  |]

selectOneUserTeamStatement :: Hasql.Statement UUID (Maybe UUID)
selectOneUserTeamStatement =
  [maybeStatement|
    select team :: uuid from user_team where user_id = $1 :: uuid limit 1
  |]

-- | Batched @GetTeamsBindings@: one round trip for many teams.
selectTeamBindingsInStatement :: Hasql.Statement (Vector UUID) (Vector (UUID, Text))
selectTeamBindingsInStatement =
  [vectorStatement|
    select team :: uuid, binding :: text from team where team = any ($1 :: uuid[])
  |]

selectUserTeamsInStatement :: Hasql.Statement (UUID, Vector UUID) (Vector UUID)
selectUserTeamsInStatement =
  [vectorStatement|
    select team :: uuid from user_team
     where user_id = $1 :: uuid and team = any ($2 :: uuid[])
     order by team
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

-- | Copy-loop insert used by the migration: unlike 'insertTeamStatement' it
-- writes every column including splash screen and search visibility, carries
-- the Cassandra writetimes, and does nothing on conflict so re-runs and stale
-- snapshot pages can never overwrite newer dual-written rows.
migrateTeamStatement ::
  Hasql.Statement
    (UUID, UUID, Text, Text, Maybe Text, Text, Maybe UTCTime, Text, Maybe UTCTime, Maybe Text, Maybe Text)
    ()
migrateTeamStatement =
  [resultlessStatement|
    insert into team
      (team, creator, name, icon, icon_key, status, status_time, binding, binding_time, splash_screen, search_visibility)
    values
      ($1 :: uuid, $2 :: uuid, $3 :: text, $4 :: text, $5 :: text?, $6 :: text, $7 :: timestamptz?, $8 :: text, $9 :: timestamptz?, $10 :: text?, $11 :: text?)
    on conflict (team) do nothing
  |]

migrateTeamMemberStatement ::
  Hasql.Statement
    (UUID, UUID, Int64, Int64, Maybe UUID, Maybe UTCTime, Maybe Text, UTCTime)
    ()
migrateTeamMemberStatement =
  [resultlessStatement|
    insert into team_member (team, user_id, perms_self, perms_copy, invited_by, invited_at, legalhold_status, perms_write_time)
    values ($1 :: uuid, $2 :: uuid, $3 :: int8, $4 :: int8, $5 :: uuid?, $6 :: timestamptz?, $7 :: text?, $8 :: timestamptz)
    on conflict (team, user_id) do nothing
  |]
