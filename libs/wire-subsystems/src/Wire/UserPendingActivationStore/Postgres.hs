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
import Data.Typeable (cast)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Cassandra qualified as Cas
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import Wire.API.PostgresMarshall
import Wire.Postgres
import Wire.Sem.Paging.Cassandra (InternalPage (..), InternalPaging, InternalPagingState (..))
import Wire.UserPendingActivationStore

interpretUserPendingActivationStoreToPostgres ::
  (PGConstraints r) =>
  InterpreterFor (UserPendingActivationStore InternalPaging) r
interpretUserPendingActivationStoreToPostgres = interpret $ \case
  Add upa -> upsertUserPendingActivation upa.upaUserId upa.upaDay
  List pagingState -> listImpl pagingState
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

-- | Keyset pagination over @users_pending_activation@, ordered by @user_id@.
--
-- The 'InternalPage' continuation runs in Cassandra's 'Client' monad, which a
-- Postgres interpreter cannot execute. Each page therefore carries the last
-- @user_id@ of the page as its paging-state marker (the 'Cas.Page' items), and
-- continuation happens by the caller passing 'Wire.Sem.Paging.pageState' back
-- into 'UserPendingActivationStore.list' — the round-trip used by every
-- consumer of this effect (Brig.Run.forExpirationsPaged,
-- Brig.API.Internal.getUserPendingActivationsH) — never by invoking the
-- 'Client' continuation.
listImpl ::
  (PGConstraints r) =>
  Maybe (InternalPagingState UserPendingActivation) ->
  Sem r (InternalPage UserPendingActivation)
listImpl pagingState = do
  let lastSeen = case pagingState of
        -- States are only ever constructed by this interpreter (in postgresql
        -- mode the states of other interpreters never reach this branch); the
        -- 'Typeable s' constraint on 'InternalPagingState' makes recovering
        -- the keyset marker sound.
        Just (InternalPagingState (page, _)) -> listToMaybe (Cas.result page) >>= cast
        Nothing -> Nothing
  rows <- runStatement (lastSeen, listPageSize + 1) listPageStatement
  let pageRows = take (fromIntegral listPageSize) (fmap (uncurry UserPendingActivation) rows)
      moreComing = length rows > fromIntegral listPageSize
      marker = case reverse pageRows of
        (u : _) -> [u.upaUserId]
        [] -> []
  pure $
    InternalPage
      ( Cas.Page
          { Cas.hasMore = moreComing,
            Cas.nextPage = error "unreachable: Postgres pages continue via UserPendingActivationStore.list",
            Cas.result = marker
          },
        \_ -> error "unreachable: Postgres pages continue via UserPendingActivationStore.list",
        pageRows
      )

-- | Upper bound on rows served per 'List' page (the Cassandra interpreter
-- pages via cql-io's default page size; this bounds the Postgres analogue).
listPageSize :: Int32
listPageSize = 1000

listPageStatement :: Hasql.Statement (Maybe UserId, Int32) [(UserId, UTCTime)]
listPageStatement =
  dimapPG
    [vectorStatement|SELECT (user_id :: uuid), (expires_at :: timestamptz)
                     FROM users_pending_activation
                     WHERE user_id > coalesce(($1 :: uuid?), ('00000000-0000-0000-0000-000000000000' :: uuid))
                     ORDER BY user_id
                     LIMIT ($2 :: int4)|]

removeMultipleImpl :: (PGConstraints r) => [UserId] -> Sem r ()
removeMultipleImpl uids =
  runStatement (postgresMarshall uids) deleteStatement
  where
    deleteStatement :: Hasql.Statement (Vector UUID) ()
    deleteStatement =
      [resultlessStatement|DELETE FROM users_pending_activation
                           WHERE user_id = ANY (($1 :: uuid[]))|]
