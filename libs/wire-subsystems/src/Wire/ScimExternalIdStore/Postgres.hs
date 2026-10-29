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
    upsertScimExternalIdRow,
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
  -- Nothing: the Insert effect leaves creation_status untouched, like the C* insert.
  Insert tid eid uid -> upsertScimExternalIdRow tid eid uid Nothing
  Lookup tid eid -> lookupScimExternalIdPg tid eid
  Delete tid eid -> deleteScimExternalIdPg tid eid
  InsertStatus tid veid uid status ->
    upsertScimExternalIdRow tid (validScimIdExternal veid) uid (Just status)
  LookupStatus tid veid -> lookupScimExternalIdStatusPg tid (validScimIdExternal veid)

-- | Upsert a full row; used by the effect interpreter (Insert with @Nothing@,
-- InsertStatus with @Just@) and the migration worker. @Nothing@ for the status
-- leaves the stored status untouched.
upsertScimExternalIdRow :: (PGConstraints r) => TeamId -> Text -> UserId -> Maybe ScimUserCreationStatus -> Sem r ()
upsertScimExternalIdRow tid eid uid status =
  runStatement (postgresMarshall tid, eid, postgresMarshall uid, postgresMarshall status) upsertStatement

upsertStatement :: Hasql.Statement (UUID, Text, UUID, Maybe Int32) ()
upsertStatement =
  lmapPG
    [resultlessStatement|INSERT INTO scim_external
                           (team, external_id, "user", creation_status)
                         VALUES
                           ($1 :: uuid, $2 :: text, $3 :: uuid, $4 :: int?)
                         ON CONFLICT (team, external_id) DO UPDATE
                         SET "user" = ($3 :: uuid),
                             creation_status = coalesce (($4 :: int?), scim_external.creation_status)
     |]

lookupScimExternalIdPg :: (PGConstraints r) => TeamId -> Text -> Sem r (Maybe UserId)
lookupScimExternalIdPg tid eid = do
  mRow <- runStatement (postgresMarshall tid, eid) selectStatement
  pure $ do
    uid <- mRow
    either (const Nothing) Just (postgresUnmarshall uid)
  where
    selectStatement :: Hasql.Statement (UUID, Text) (Maybe UUID)
    selectStatement =
      [maybeStatement|SELECT "user" :: uuid
                       FROM scim_external
                       WHERE team = ($1 :: uuid) AND external_id = ($2 :: text)
                      |]

deleteScimExternalIdPg :: (PGConstraints r) => TeamId -> Text -> Sem r ()
deleteScimExternalIdPg tid eid =
  runStatement (postgresMarshall tid, eid) deleteStatement

deleteStatement :: Hasql.Statement (UUID, Text) ()
deleteStatement =
  [resultlessStatement|DELETE FROM scim_external
                       WHERE team = ($1 :: uuid) AND external_id = ($2 :: text)
                      |]

lookupScimExternalIdStatusPg :: (PGConstraints r) => TeamId -> Text -> Sem r (Maybe (UserId, ScimUserCreationStatus))
lookupScimExternalIdStatusPg tid eid = do
  mRow <- runStatement (postgresMarshall tid, eid) selectStatusStatement
  pure $
    mRow >>= \(uid, status) -> do
      uid' <- either (const Nothing) Just (postgresUnmarshall uid)
      -- a missing creation_status means the user was created (pre-status rows)
      status' <- either (const Nothing) Just (traverse postgresUnmarshall status)
      pure (uid', fromMaybe ScimUserCreated status')
  where
    selectStatusStatement :: Hasql.Statement (UUID, Text) (Maybe (UUID, Maybe Int32))
    selectStatusStatement =
      [maybeStatement|SELECT "user" :: uuid, creation_status :: int?
                       FROM scim_external
                       WHERE team = ($1 :: uuid) AND external_id = ($2 :: text)
                      |]
