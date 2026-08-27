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

module Wire.ActivationCodeStore.Postgres
  ( interpretActivationCodeStoreToPostgres,
    insertActivationKeyRow,
    deleteExpiredActivationKeys,
  )
where

import Data.Id (UserId)
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import Util.Timeout
import Wire.API.PostgresMarshall
import Wire.API.User.Activation
import Wire.API.User.EmailAddress
import Wire.ActivationCodeStore
import Wire.Postgres
import Wire.UserKeyStore

interpretActivationCodeStoreToPostgres ::
  (PGConstraints r) =>
  InterpreterFor ActivationCodeStore r
interpretActivationCodeStoreToPostgres = interpret $ \case
  LookupActivationCode ek -> do
    key <- embed (mkActivationKey ek)
    runStatement key lookupCode
  NewActivationCode ek timeout uid -> newActivationCodeImpl ek timeout uid
  DeleteActivationCode ek -> do
    key <- embed (mkActivationKey ek)
    runStatement key deleteCode
  LookupActivationKey key -> do
    mRow <- runStatement key selectForVerify
    pure $ case mRow of
      Just (keyType, keyText, code, user, retries) -> Just (ActivationKeyRow keyType keyText code user retries)
      Nothing -> Nothing
  DecrementActivationRetries key -> runStatement key decrementRetries
  DeleteActivationKey key -> runStatement key deleteCode

-- | Delete all expired activation key rows in bounded batches; returns the
-- total number deleted.
deleteExpiredActivationKeys :: (PGConstraints r) => Sem r Int
deleteExpiredActivationKeys = go 0
  where
    batchSize :: Int32
    batchSize = 10000
    go !acc = do
      deleted <- length <$> runStatement batchSize deleteExpiredBatch
      if deleted >= fromIntegral batchSize then go (acc + deleted) else pure (acc + deleted)

-- | Delete one batch of expired rows; returns one element per deleted row.
deleteExpiredBatch :: Hasql.Statement Int32 [Int32]
deleteExpiredBatch =
  rmapPG
    [vectorStatement|
      DELETE FROM activation_keys
      WHERE key IN (SELECT key FROM activation_keys WHERE expires_at <= now() LIMIT $1 :: int4)
      RETURNING 1 :: int4
    |]

lookupCode :: Hasql.Statement ActivationKey (Maybe (Maybe UserId, ActivationCode))
lookupCode =
  dimapPG
    [maybeStatement|
      SELECT user_id :: uuid?, code :: text
      FROM activation_keys
      WHERE key = ($1 :: text) AND expires_at > now()
    |]

newActivationCodeImpl ::
  (PGConstraints r) =>
  EmailKey ->
  Timeout ->
  Maybe UserId ->
  Sem r Activation
newActivationCodeImpl ek timeout u = do
  key <- embed (mkActivationKey ek)
  code <- embed genActivationCode
  let keyText = fromEmail (emailKeyOrig ek)
  runStatement (key, "email", keyText, code, u, maxAttempts, round timeout) insertWithTtl
  pure $ Activation key code

-- | Used by the migration loop to copy an existing row verbatim (with a
-- computed @expires_at@ derived from the Cassandra TTL).
insertActivationKeyRow ::
  (PGConstraints r) =>
  (ActivationKey, Text, Text, ActivationCode, Maybe UserId, Int32, Int32) ->
  Sem r ()
insertActivationKeyRow (key, keyType, keyText, code, mUser, retries, ttlSecs) =
  runStatement (key, keyType, keyText, code, mUser, retries, ttlSecs) insertWithTtl

--------------------------------------------------------------------------------
-- Statements

insertWithTtl ::
  Hasql.Statement (ActivationKey, Text, Text, ActivationCode, Maybe UserId, Int32, Int32) ()
insertWithTtl =
  lmapPG
    [resultlessStatement|
      INSERT INTO activation_keys (key, key_type, key_text, code, user_id, retries, expires_at)
      VALUES ($1 :: text, $2 :: text, $3 :: text, $4 :: text, $5 :: uuid?, $6 :: int4, now() + make_interval(secs => $7 :: int4))
      ON CONFLICT (key) DO UPDATE
      SET key_type = ($2 :: text),
          key_text = ($3 :: text),
          code = ($4 :: text),
          user_id = ($5 :: uuid?),
          retries = ($6 :: int4),
          expires_at = now() + make_interval(secs => $7 :: int4)
    |]

selectForVerify ::
  Hasql.Statement ActivationKey (Maybe (Text, Text, ActivationCode, Maybe UserId, Int32))
selectForVerify =
  dimapPG
    [maybeStatement|
      SELECT key_type :: text,
             key_text :: text,
             code :: text,
             user_id :: uuid?,
             retries :: int4
      FROM activation_keys
      WHERE key = ($1 :: text) AND expires_at > now()
    |]

decrementRetries :: Hasql.Statement ActivationKey ()
decrementRetries =
  lmapPG
    [resultlessStatement|
      UPDATE activation_keys SET retries = retries - 1 WHERE key = ($1 :: text) AND retries > 0
    |]

deleteCode :: Hasql.Statement ActivationKey ()
deleteCode =
  lmapPG
    [resultlessStatement|
      DELETE FROM activation_keys WHERE key = ($1 :: text)
    |]
