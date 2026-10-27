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

module Wire.NonceStore.Postgres
  ( interpretNonceStoreToPostgres,
    insertNonce,
    deleteNonce,
  )
where

import Data.Id (UserId, toUUID)
import Data.Nonce (Nonce (..), NonceTtlSecs (..))
import Data.UUID (UUID)
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import Wire.NonceStore (NonceStore (..))
import Wire.Postgres

interpretNonceStoreToPostgres ::
  (PGConstraints r) =>
  InterpreterFor NonceStore r
interpretNonceStoreToPostgres = interpret $ \case
  InsertNonce ttl uid key nonce -> insertNonce ttl uid key nonce
  LookupAndDeleteNonce uid key -> lookupAndDeleteNonce uid key
  DeleteNonce uid key -> deleteNonce uid key

insertNonce :: (PGConstraints r) => NonceTtlSecs -> UserId -> Text -> Nonce -> Sem r ()
insertNonce ttl uid key nonce =
  runStatement (toUUID uid, key, unNonce nonce, fromIntegral (unNonceTtlSecs ttl) :: Int32) insertStatement
  where
    insertStatement :: Hasql.Statement (UUID, Text, UUID, Int32) ()
    insertStatement =
      [resultlessStatement|INSERT INTO nonce (user_id, key, nonce, expires_at)
                           VALUES ($1 :: uuid, $2 :: text, $3 :: uuid, now() + make_interval(secs => $4 :: int))
                           ON CONFLICT (user_id, key)
                           DO UPDATE SET nonce = EXCLUDED.nonce, expires_at = EXCLUDED.expires_at
                          |]

-- | Atomically read and consume the nonce. A row whose expiry has passed is
-- treated as absent (expired = consumed).
lookupAndDeleteNonce :: (PGConstraints r) => UserId -> Text -> Sem r (Maybe Nonce)
lookupAndDeleteNonce uid key = do
  mrow <- runStatement (toUUID uid, key) lookupAndDeleteStatement
  pure $ do
    (nonce, mLive) <- mrow
    if mLive == Just True then Just (Nonce nonce) else Nothing
  where
    lookupAndDeleteStatement :: Hasql.Statement (UUID, Text) (Maybe (UUID, Maybe Bool))
    lookupAndDeleteStatement =
      [maybeStatement|DELETE FROM nonce
                      WHERE user_id = $1 :: uuid AND key = $2 :: text
                      RETURNING (nonce :: uuid), ((expires_at > now()) :: bool?)
                     |]

deleteNonce :: (PGConstraints r) => UserId -> Text -> Sem r ()
deleteNonce uid key =
  runStatement (toUUID uid, key) deleteStatement
  where
    deleteStatement :: Hasql.Statement (UUID, Text) ()
    deleteStatement =
      [resultlessStatement|DELETE FROM nonce
                           WHERE user_id = $1 :: uuid AND key = $2 :: text
                          |]
