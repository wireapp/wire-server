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

module Wire.ScimExternalIdStore.Postgres
  ( interpretScimExternalIdStoreToPostgres,
  )
where

import Data.Id
import Data.UUID (UUID)
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import Wire.API.PostgresMarshall
import Wire.API.User.Scim (ScimUserCreationStatus (..), ValidScimId (..))
import Wire.Postgres
import Wire.ScimExternalIdStore (ScimExternalIdStore (..))

interpretScimExternalIdStoreToPostgres :: (PGConstraints r) => InterpreterFor ScimExternalIdStore r
interpretScimExternalIdStoreToPostgres = interpret $ \case
  Insert tid eid uid -> upsertScimExternalId tid eid uid Nothing
  Lookup tid eid -> lookupScimExternalId tid eid
  Delete tid eid -> deleteScimExternalId tid eid
  InsertStatus tid veid uid status ->
    upsertScimExternalId tid (validScimIdExternal veid) uid (Just status)
  LookupStatus tid veid -> lookupScimExternalIdStatus tid (validScimIdExternal veid)

-- | Upsert a full row; used by the effect interpreter (Insert with @Nothing@,
-- InsertStatus with @Just@). @Nothing@ for the status leaves the stored status
-- untouched, like the C* insert.
upsertScimExternalId :: (PGConstraints r) => TeamId -> Text -> UserId -> Maybe ScimUserCreationStatus -> Sem r ()
upsertScimExternalId tid eid uid status =
  runStatement (toUUID tid, eid, toUUID uid, postgresMarshall status) upsertStatement
  where
    upsertStatement :: Hasql.Statement (UUID, Text, UUID, Maybe Int32) ()
    upsertStatement =
      [resultlessStatement|INSERT INTO scim_external
                             (team, external_id, user_id, creation_status)
                           VALUES
                             ($1 :: uuid, $2 :: text, $3 :: uuid, $4 :: int?)
                           ON CONFLICT (team, external_id) DO UPDATE
                           SET user_id = excluded.user_id,
                               creation_status = coalesce (excluded.creation_status, scim_external.creation_status)
       |]

-- | Same row as 'lookupScimExternalIdStatus', but kept as a separate query on
-- purpose: 'Lookup' must return the user id even if @creation_status@ holds an
-- unknown code, which 'lookupScimExternalIdStatus' would map to 'Nothing'.
lookupScimExternalId :: (PGConstraints r) => TeamId -> Text -> Sem r (Maybe UserId)
lookupScimExternalId tid eid =
  fmap Id <$> runStatement (toUUID tid, eid) selectStatement
  where
    selectStatement :: Hasql.Statement (UUID, Text) (Maybe UUID)
    selectStatement =
      [maybeStatement|SELECT user_id :: uuid
                       FROM scim_external
                       WHERE team = ($1 :: uuid) AND external_id = ($2 :: text)
                      |]

deleteScimExternalId :: (PGConstraints r) => TeamId -> Text -> Sem r ()
deleteScimExternalId tid eid =
  runStatement (toUUID tid, eid) deleteStatement
  where
    deleteStatement :: Hasql.Statement (UUID, Text) ()
    deleteStatement =
      [resultlessStatement|DELETE FROM scim_external
                           WHERE team = ($1 :: uuid) AND external_id = ($2 :: text)
                          |]

lookupScimExternalIdStatus :: (PGConstraints r) => TeamId -> Text -> Sem r (Maybe (UserId, ScimUserCreationStatus))
lookupScimExternalIdStatus tid eid = do
  mRow <- runStatement (toUUID tid, eid) selectStatusStatement
  pure $
    mRow >>= \(uid, status) -> do
      -- a missing creation_status means the user was created (pre-status rows)
      status' <- either (const Nothing) Just (traverse postgresUnmarshall status)
      pure (Id uid, fromMaybe ScimUserCreated status')
  where
    selectStatusStatement :: Hasql.Statement (UUID, Text) (Maybe (UUID, Maybe Int32))
    selectStatusStatement =
      [maybeStatement|SELECT user_id :: uuid, creation_status :: int?
                       FROM scim_external
                       WHERE team = ($1 :: uuid) AND external_id = ($2 :: text)
                      |]
