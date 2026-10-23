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

-- | Postgres storage for team invitations and pending SCIM users.
--
-- Cassandra stores invitations with a row TTL (the invitation-code timeout)
-- and collapses multiple invites to the same (email, team) pair into a single
-- row (last invite wins).  Here the main @team_invitation@ table keeps every
-- invitation and carries an @expires_at@ column; the @expires_at > now()@
-- guard reproduces Cassandra's vanished-row semantics, and lookups by email
-- pick the latest invitation per (email, team) via @DISTINCT ON@.  The
-- denormalized @team_invitation_info@ and @team_invitation_email@ Cassandra
-- tables become plain indexes on @code@ and @email@.
--
-- Unlike Cassandra's TTL garbage collection, expired rows are retained but
-- hidden by the @expires_at@ guard (same accepted precedent as
-- @vcodes@/@password_reset@); purging is left to a future cleanup job.
-- Cassandra's @team_invitation_email@ index deletes hid newer invitations of
-- an (email, team) pair; the Postgres port intentionally keeps them visible.
module Wire.InvitationStore.Postgres
  ( interpretInvitationStoreToPostgres,
    insertInvitationQ,
    insertPendingScimUserQ,
    InvitationRow,
  )
where

import Data.Id (Id (..), InvitationId, TeamId, UserId)
import Data.Json.Util (toUTCTimeMillis)
import Data.Range (Range, fromRange)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import Wire.API.PostgresMarshall
import Wire.API.Team.Role (Role)
import Wire.API.User (EmailAddress, InvitationCode, Name)
import Wire.API.User.EmailAddress ()
import Wire.InvitationStore
import Wire.Postgres

-- | The @team_invitation@ row encoding shared by the effect-level insert and
-- the migration copy loop so they cannot drift.
type InvitationRow =
  ( UUID, -- team
    UUID, -- invitation id
    Maybe Int32, -- role
    UTCTime, -- created_at
    Maybe UUID, -- created_by
    Text, -- email
    Maybe Text, -- name
    Text, -- code
    Int32 -- ttl seconds (expires_at = now() + ttl)
  )

-- | The @team_invitation@ row as read back from Postgres, before decoding
-- into 'StoredInvitation'.
type InvitationRowDb =
  ( UUID, -- team
    Maybe Int32, -- role
    UUID, -- invitation id
    UTCTime, -- created_at
    Maybe UUID, -- created_by
    Text, -- email
    Maybe Text, -- name
    Text -- code
  )

interpretInvitationStoreToPostgres ::
  (PGConstraints r) =>
  InterpreterFor InvitationStore r
interpretInvitationStoreToPostgres = interpret $ \case
  InsertInvitation inv timeout -> do
    insertInvitationQ (invitationTuple inv (round timeout))
    pure (insertInvToStoredInv inv)
  LookupInvitation tid iid ->
    lookupInvitationQ (toUUID tid, toUUID iid)
  LookupInvitationByCode code ->
    lookupInvitationByCodeQ code
  LookupInvitationsByEmail email ->
    lookupInvitationsByEmailQ email
  InsertPendingScimUser tid email uid ->
    insertPendingScimUserQ (toUUID tid, postgresMarshall email, toUUID uid)
  LookupPendingScimUsers tid email ->
    lookupPendingScimUsersQ (toUUID tid, postgresMarshall email)
  DeletePendingScimUser tid email uid ->
    deletePendingScimUserQ (toUUID tid, postgresMarshall email, toUUID uid)
  LookupInvitationsPaginated mSize tid miid ->
    lookupInvitationsPaginatedQ mSize tid miid
  CountInvitations tid ->
    countInvitationsQ tid
  DeleteInvitation tid invId ->
    deleteInvitationQ (toUUID tid, toUUID invId)
  DeleteAllTeamInvitations tid ->
    deleteAllTeamInvitationsQ tid

-- | Flatten an 'InsertInvitation' (plus the code timeout in seconds) into the
-- canonical row tuple.
invitationTuple :: InsertInvitation -> Int32 -> InvitationRow
invitationTuple (MkInsertInvitation invId teamId role createdAt createdBy email name code) ttl =
  ( toUUID teamId,
    toUUID invId,
    Just (postgresMarshall role),
    createdAt,
    toUUID <$> createdBy,
    postgresMarshall email,
    postgresMarshall <$> name,
    postgresMarshall code,
    ttl
  )

-- | Turn a raw row into a 'StoredInvitation'.
toStoredInvitation :: InvitationRowDb -> Either Text StoredInvitation
toStoredInvitation (team, role, invId, createdAt, createdBy, email, name, code) =
  MkStoredInvitation
    <$> Right (Id team)
    <*> traverse (postgresUnmarshall @Int32 @Role) role
    <*> Right (Id invId)
    <*> Right (toUTCTimeMillis createdAt)
    <*> Right (Id <$> createdBy)
    <*> postgresUnmarshall @Text @EmailAddress email
    <*> traverse (postgresUnmarshall @Text @Name) name
    <*> postgresUnmarshall @Text @InvitationCode code

-- | A row whose payload fails to decode is a schema drift, not a per-request
-- condition; surface it as an IO error.
decodeOrDie :: (Member (Embed IO) r) => Either Text StoredInvitation -> Sem r StoredInvitation
decodeOrDie = either (embed . fail . Text.unpack) pure

insertInvitationQ :: (PGConstraints r) => InvitationRow -> Sem r ()
insertInvitationQ = flip runStatement insertInvitationQStatement

insertInvitationQStatement :: Hasql.Statement InvitationRow ()
insertInvitationQStatement =
  [resultlessStatement|INSERT INTO team_invitation
                         (team, invitation_id, role, created_at, created_by, email, name, code, expires_at)
                       VALUES
                         ($1 :: uuid, $2 :: uuid, $3 :: int4?, $4 :: timestamptz, $5 :: uuid?, $6 :: text, $7 :: text?, $8 :: text, now() + make_interval(secs => $9 :: int4))
                       ON CONFLICT (team, invitation_id) DO UPDATE
                       SET role = ($3 :: int4?),
                           created_at = ($4 :: timestamptz),
                           created_by = ($5 :: uuid?),
                           email = ($6 :: text),
                           name = ($7 :: text?),
                           code = ($8 :: text),
                           expires_at = now() + make_interval(secs => $9 :: int4)
     |]

lookupInvitationQ :: (PGConstraints r) => (UUID, UUID) -> Sem r (Maybe StoredInvitation)
lookupInvitationQ key = do
  mRow <- runStatement key selectInvitationStatement
  traverse (decodeOrDie . toStoredInvitation) mRow

selectInvitationStatement :: Hasql.Statement (UUID, UUID) (Maybe InvitationRowDb)
selectInvitationStatement =
  [maybeStatement|SELECT (team :: uuid), (role :: int4?), (invitation_id :: uuid), (created_at :: timestamptz), (created_by :: uuid?), (email :: text), (name :: text?), (code :: text)
                  FROM team_invitation
                  WHERE team = ($1 :: uuid) AND invitation_id = ($2 :: uuid) AND expires_at > now()
                 |]

lookupInvitationByCodeQ :: (PGConstraints r) => InvitationCode -> Sem r (Maybe StoredInvitation)
lookupInvitationByCodeQ code = do
  mRow <- runStatement (postgresMarshall code) selectInvitationByCodeStatement
  traverse (decodeOrDie . toStoredInvitation) mRow

selectInvitationByCodeStatement :: Hasql.Statement Text (Maybe InvitationRowDb)
selectInvitationByCodeStatement =
  [maybeStatement|SELECT (team :: uuid), (role :: int4?), (invitation_id :: uuid), (created_at :: timestamptz), (created_by :: uuid?), (email :: text), (name :: text?), (code :: text)
                  FROM team_invitation
                  WHERE code = ($1 :: text) AND expires_at > now()
                  ORDER BY created_at DESC, invitation_id DESC
                  LIMIT 1
                 |]

lookupInvitationsByEmailQ :: (PGConstraints r) => EmailAddress -> Sem r [StoredInvitation]
lookupInvitationsByEmailQ email = do
  rows <- runStatement (postgresMarshall email) selectInvitationsByEmailStatement
  traverse (decodeOrDie . toStoredInvitation) (toList rows)

-- | One invitation per (email, team) pair — the latest one — reproducing
-- Cassandra's last-invite-wins @team_invitation_email@ table.
selectInvitationsByEmailStatement :: Hasql.Statement Text (Vector InvitationRowDb)
selectInvitationsByEmailStatement =
  [vectorStatement|SELECT DISTINCT ON (email, team) (team :: uuid), (role :: int4?), (invitation_id :: uuid), (created_at :: timestamptz), (created_by :: uuid?), (email :: text), (name :: text?), (code :: text)
                   FROM team_invitation
                   WHERE email = ($1 :: text) AND expires_at > now()
                   ORDER BY email, team, created_at DESC, invitation_id DESC
                  |]

lookupInvitationsPaginatedQ ::
  (PGConstraints r) =>
  Maybe (Range 1 500 Int32) ->
  TeamId ->
  Maybe InvitationId ->
  Sem r (PaginatedResult [StoredInvitation])
lookupInvitationsPaginatedQ mSize tid miid = do
  let pageSize = maybe 100 fromRange mSize
      fetchLimit = pageSize + 1
  rows <- case miid of
    Just ref ->
      runStatement
        (toUUID tid, toUUID ref, fetchLimit)
        selectInvitationsPaginatedFromStatement
    Nothing ->
      runStatement
        (toUUID tid, fetchLimit)
        selectInvitationsPaginatedStatement
  invitations <- traverse (decodeOrDie . toStoredInvitation) (take (fromIntegral pageSize) (toList rows))
  pure $
    if Vector.length rows > fromIntegral pageSize
      then PaginatedResultHasMore invitations
      else PaginatedResult invitations

selectInvitationsPaginatedStatement :: Hasql.Statement (UUID, Int32) (Vector InvitationRowDb)
selectInvitationsPaginatedStatement =
  [vectorStatement|SELECT (team :: uuid), (role :: int4?), (invitation_id :: uuid), (created_at :: timestamptz), (created_by :: uuid?), (email :: text), (name :: text?), (code :: text)
                   FROM team_invitation
                   WHERE team = ($1 :: uuid) AND expires_at > now()
                   ORDER BY invitation_id ASC
                   LIMIT ($2 :: int4)
                  |]

selectInvitationsPaginatedFromStatement :: Hasql.Statement (UUID, UUID, Int32) (Vector InvitationRowDb)
selectInvitationsPaginatedFromStatement =
  [vectorStatement|SELECT (team :: uuid), (role :: int4?), (invitation_id :: uuid), (created_at :: timestamptz), (created_by :: uuid?), (email :: text), (name :: text?), (code :: text)
                   FROM team_invitation
                   WHERE team = ($1 :: uuid) AND invitation_id > ($2 :: uuid) AND expires_at > now()
                   ORDER BY invitation_id ASC
                   LIMIT ($3 :: int4)
                  |]

countInvitationsQ :: (PGConstraints r) => TeamId -> Sem r Int64
countInvitationsQ = flip runStatement countInvitationsStatement

countInvitationsStatement :: Hasql.Statement TeamId Int64
countInvitationsStatement =
  lmapPG
    [singletonStatement|SELECT count(*) :: int8 FROM team_invitation WHERE team = ($1 :: uuid) AND expires_at > now()
       |]

deleteInvitationQ :: (PGConstraints r) => (UUID, UUID) -> Sem r ()
deleteInvitationQ = flip runStatement deleteInvitationStatement

deleteInvitationStatement :: Hasql.Statement (UUID, UUID) ()
deleteInvitationStatement =
  [resultlessStatement|DELETE FROM team_invitation WHERE team = ($1 :: uuid) AND invitation_id = ($2 :: uuid)
     |]

deleteAllTeamInvitationsQ :: (PGConstraints r) => TeamId -> Sem r ()
deleteAllTeamInvitationsQ = flip runStatement deleteAllTeamInvitationsStatement

deleteAllTeamInvitationsStatement :: Hasql.Statement TeamId ()
deleteAllTeamInvitationsStatement =
  lmapPG
    [resultlessStatement|DELETE FROM team_invitation WHERE team = ($1 :: uuid)
     |]

insertPendingScimUserQ :: (PGConstraints r) => (UUID, Text, UUID) -> Sem r ()
insertPendingScimUserQ = flip runStatement insertPendingScimUserStatement

insertPendingScimUserStatement :: Hasql.Statement (UUID, Text, UUID) ()
insertPendingScimUserStatement =
  [resultlessStatement|INSERT INTO team_scim_pending_user_email (team, email, user_id)
                       VALUES ($1 :: uuid, $2 :: text, $3 :: uuid)
                       ON CONFLICT (team, email, user_id) DO NOTHING
     |]

lookupPendingScimUsersQ :: (PGConstraints r) => (UUID, Text) -> Sem r [UserId]
lookupPendingScimUsersQ key = map Id . toList <$> runStatement key lookupPendingScimUsersStatement

lookupPendingScimUsersStatement :: Hasql.Statement (UUID, Text) (Vector UUID)
lookupPendingScimUsersStatement =
  lmapPG
    [vectorStatement|SELECT (user_id :: uuid) FROM team_scim_pending_user_email
                     WHERE team = ($1 :: uuid) AND email = ($2 :: text)
                     ORDER BY user_id ASC
                    |]

deletePendingScimUserQ :: (PGConstraints r) => (UUID, Text, UUID) -> Sem r ()
deletePendingScimUserQ = flip runStatement deletePendingScimUserStatement

deletePendingScimUserStatement :: Hasql.Statement (UUID, Text, UUID) ()
deletePendingScimUserStatement =
  [resultlessStatement|DELETE FROM team_scim_pending_user_email
                       WHERE team = ($1 :: uuid) AND email = ($2 :: text) AND user_id = ($3 :: uuid)
     |]
