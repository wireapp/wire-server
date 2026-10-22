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

module Wire.ScimUserMetaStore.Postgres
  ( interpretScimUserMetaStoreToPostgres,
    upsertScimUserMetaRow,
  )
where

import Data.Id (UserId)
import Data.Json.Util (UTCTimeMillis, toUTCTimeMillis)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import Web.Scim.Schema.Common (WithId (..))
import Web.Scim.Schema.Meta (Meta (..), WithMeta (..))
import Wire.API.PostgresMarshall
import Wire.Postgres
import Wire.ScimUserMetaStore (ScimUserMeta (..), ScimUserMetaStore (..))

interpretScimUserMetaStoreToPostgres :: (PGConstraints r) => InterpreterFor ScimUserMetaStore r
interpretScimUserMetaStoreToPostgres = interpret $ \case
  Write emailType emailPrimary (WithMeta meta (WithId uid _)) ->
    upsertScimUserMetaRow uid (toUTCTimeMillis (created meta)) (toUTCTimeMillis (lastModified meta)) emailType emailPrimary
  Read uid -> readScimUserMetaPg uid
  ReadMulti uids -> readScimUserMetaMultiPg uids
  Delete uid -> deleteScimUserMetaPg uid

-- | Upsert a full row; used by the effect interpreter and the migration
-- worker. The C* write overwrites the whole row, so the upsert overwrites all
-- columns (no @coalesce@).
upsertScimUserMetaRow :: (PGConstraints r) => UserId -> UTCTimeMillis -> UTCTimeMillis -> Maybe Text -> Maybe Bool -> Sem r ()
upsertScimUserMetaRow uid createdAt lastUpdatedAt emailType emailPrimary =
  runStatement
    (postgresMarshall uid, postgresMarshall createdAt, postgresMarshall lastUpdatedAt, emailType, emailPrimary)
    upsertStatement

upsertStatement :: Hasql.Statement (UUID, UTCTime, UTCTime, Maybe Text, Maybe Bool) ()
upsertStatement =
  lmapPG
    [resultlessStatement|INSERT INTO scim_user_times
                           (uid, created_at, last_updated_at, email_type, email_primary)
                         VALUES
                           ($1 :: uuid, $2 :: timestamptz, $3 :: timestamptz, $4 :: text?, $5 :: bool?)
                         ON CONFLICT (uid) DO UPDATE
                         SET created_at = ($2 :: timestamptz),
                             last_updated_at = ($3 :: timestamptz),
                             email_type = ($4 :: text?),
                             email_primary = ($5 :: bool?)
     |]

readScimUserMetaPg :: (PGConstraints r) => UserId -> Sem r (Maybe ScimUserMeta)
readScimUserMetaPg uid = do
  mRow <- runStatement (postgresMarshall uid) selectStatement
  pure $ do
    (createdAt, lastUpdatedAt, emailType, emailPrimary) <- mRow
    ScimUserMeta
      <$> hush (postgresUnmarshall createdAt)
      <*> hush (postgresUnmarshall lastUpdatedAt)
      <*> pure emailType
      <*> pure emailPrimary
  where
    selectStatement :: Hasql.Statement UUID (Maybe (UTCTime, UTCTime, Maybe Text, Maybe Bool))
    selectStatement =
      [maybeStatement|SELECT created_at :: timestamptz,
                              last_updated_at :: timestamptz,
                              email_type :: text?,
                              email_primary :: bool?
                       FROM scim_user_times
                       WHERE uid = ($1 :: uuid)
                      |]

readScimUserMetaMultiPg :: (PGConstraints r) => [UserId] -> Sem r [(UserId, ScimUserMeta)]
readScimUserMetaMultiPg uids = do
  rows <- runStatement (fmap postgresMarshall uids) selectMultiStatement
  pure $
    mapMaybe
      ( \(uid, createdAt, lastUpdatedAt, emailType, emailPrimary) ->
          (,)
            <$> hush (postgresUnmarshall uid)
            <*> ( ScimUserMeta
                    <$> hush (postgresUnmarshall createdAt)
                    <*> hush (postgresUnmarshall lastUpdatedAt)
                    <*> pure emailType
                    <*> pure emailPrimary
                )
      )
      rows
  where
    selectMultiStatement :: Hasql.Statement [UUID] [(UUID, UTCTime, UTCTime, Maybe Text, Maybe Bool)]
    selectMultiStatement =
      dimapPG @(Vector _)
        [vectorStatement|SELECT uid :: uuid,
                               created_at :: timestamptz,
                               last_updated_at :: timestamptz,
                               email_type :: text?,
                               email_primary :: bool?
                        FROM scim_user_times
                        WHERE uid = ANY (($1 :: uuid[]))
                       |]

deleteScimUserMetaPg :: (PGConstraints r) => UserId -> Sem r ()
deleteScimUserMetaPg uid = runStatement (postgresMarshall uid) deleteStatement

deleteStatement :: Hasql.Statement UUID ()
deleteStatement =
  [resultlessStatement|DELETE FROM scim_user_times
                       WHERE uid = ($1 :: uuid)
                      |]

hush :: Either a b -> Maybe b
hush = either (const Nothing) Just
