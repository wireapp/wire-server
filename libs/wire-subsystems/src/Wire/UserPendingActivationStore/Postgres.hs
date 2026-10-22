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

module Wire.UserPendingActivationStore.Postgres
  ( interpretUserPendingActivationStoreToPostgres,
    upsertUserPendingActivation,
  )
where

import Data.Id (UserId)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import Wire.API.PostgresMarshall
import Wire.Postgres
import Wire.UserPendingActivationStore

interpretUserPendingActivationStoreToPostgres ::
  (PGConstraints r) =>
  InterpreterFor UserPendingActivationStore r
interpretUserPendingActivationStoreToPostgres = interpret $ \case
  Add upa -> upsertUserPendingActivation upa.upaUserId upa.upaDay
  List -> listImpl
  RemoveMultiple uids -> removeMultipleImpl uids

upsertUserPendingActivation :: (PGConstraints r) => UserId -> UTCTime -> Sem r ()
upsertUserPendingActivation uid expiresAt =
  runStatement (uid, expiresAt) insertStatement
  where
    insertStatement :: Hasql.Statement (UserId, UTCTime) ()
    insertStatement =
      lmapPG
        [resultlessStatement|INSERT INTO users_pending_activation (user_id, expires_at)
                             VALUES ($1 :: uuid, $2 :: timestamptz)
                             ON CONFLICT (user_id) DO UPDATE SET expires_at = EXCLUDED.expires_at|]

listImpl :: (PGConstraints r) => Sem r [UserPendingActivation]
listImpl =
  fmap (uncurry UserPendingActivation) <$> runStatement () listStatement
  where
    listStatement :: Hasql.Statement () [(UserId, UTCTime)]
    listStatement =
      dimapPG
        [vectorStatement|SELECT (user_id :: uuid), (expires_at :: timestamptz)
                         FROM users_pending_activation|]

removeMultipleImpl :: (PGConstraints r) => [UserId] -> Sem r ()
removeMultipleImpl uids =
  runStatement (postgresMarshall uids) deleteStatement
  where
    deleteStatement :: Hasql.Statement (Vector UUID) ()
    deleteStatement =
      [resultlessStatement|DELETE FROM users_pending_activation
                           WHERE user_id = ANY (($1 :: uuid[]))|]
